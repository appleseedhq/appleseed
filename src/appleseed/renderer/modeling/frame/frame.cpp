
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

// Interface header.
#include "frame.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/aov/aovsettings.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/denoising/denoiser.h"
#include "renderer/modeling/aov/aovfactoryregistrar.h"
#include "renderer/modeling/aov/denoiseraov.h"
#include "renderer/modeling/aov/iaovfactory.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/core/exceptions/exception.h"
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/color.h"
#include "foundation/image/drawing.h"
#include "foundation/image/image.h"
#include "foundation/image/imageattributes.h"
#include "foundation/image/oiioimagefilewriter.h"
#include "foundation/image/pixel.h"
#include "foundation/image/text/textrenderer.h"
#include "foundation/image/tile.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/system.h"
#include "foundation/resources/logo/appleseed-seeds-16.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Standard headers.
#include <algorithm>
#include <memory>
#include <string>

using namespace bcd;
using namespace foundation;
using namespace std;
namespace bf = boost::filesystem;
namespace bsys = boost::system;

namespace renderer
{

//
// Frame class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();

    const string DefaultRenderStampFormat = "appleseed {lib-version} | Time: {render-time}";
}

UniqueID Frame::get_class_uid()
{
    return g_class_uid;
}

struct Frame::Impl
{
    // Parameters.
    size_t                  m_frame_width;
    size_t                  m_frame_height;
    size_t                  m_tile_width;
    size_t                  m_tile_height;
    string                  m_filter_name;
    float                   m_filter_radius;
    unique_ptr<Filter2f>    m_filter;
    AABB2u                  m_crop_window;
    bool                    m_render_stamp_enabled;
    string                  m_render_stamp_format;
    DenoisingMode           m_denoising_mode;
    bool                    m_save_extra_aovs;

    // Images.
    unique_ptr<Image>       m_image;
    unique_ptr<ImageStack>  m_aov_images;
    AOVContainer            m_aovs;
    AOVContainer            m_internal_aovs;
    DenoiserAOV*            m_denoiser_aov;
    vector<size_t>          m_extra_aovs;
};

Frame::Frame(
    const char*         name,
    const ParamArray&   params,
    const AOVContainer& aovs)
  : Entity(g_class_uid, params)
  , impl(new Impl())
{
    set_name(name);

    extract_parameters();

    // Create the underlying image.
    impl->m_image.reset(
        new Image(
            impl->m_frame_width,
            impl->m_frame_height,
            impl->m_tile_width,
            impl->m_tile_height,
            4,
            PixelFormatFloat));

    // Retrieve the image properties.
    m_props = impl->m_image->properties();

    // Create the image stack for AOVs.
    impl->m_aov_images.reset(
        new ImageStack(
            impl->m_frame_width,
            impl->m_frame_height,
            impl->m_tile_width,
            impl->m_tile_height));

    // Copy and add the aovs.
    if (aovs.size() > MaxAOVCount)
    {
        RENDERER_LOG_WARNING(
            "could not create all aovs, keeping the first (" FMT_SIZE_T ") aovs.",
            MaxAOVCount);
    }

    const AOVFactoryRegistrar aov_registrar;
    for (size_t i = 0, e = min(aovs.size(), MaxAOVCount); i < e; ++i)
    {
        const AOV* original_aov = aovs.get_by_index(i);
        const IAOVFactory* aov_factory = aov_registrar.lookup(original_aov->get_model());
        assert(aov_factory);

        auto_release_ptr<AOV> aov = aov_factory->create(original_aov->get_parameters());
        aov->create_image(
            impl->m_frame_width,
            impl->m_frame_height,
            impl->m_tile_width,
            impl->m_tile_height,
            aov_images());
        impl->m_aovs.insert(aov);
    }

    // Create internal AOVs.
    if (impl->m_denoising_mode != DenoisingMode::Off)
    {
        auto_release_ptr<DenoiserAOV> aov = DenoiserAOVFactory::create();
        impl->m_denoiser_aov = aov.get();

        aov->create_image(
            impl->m_frame_width,
            impl->m_frame_height,
            impl->m_tile_width,
            impl->m_tile_height,
            aov_images());

        impl->m_internal_aovs.insert(auto_release_ptr<AOV>(aov));
    }
    else
        impl->m_denoiser_aov = nullptr;
}

Frame::~Frame()
{
    delete impl;
}

void Frame::release()
{
    delete this;
}

void Frame::print_settings()
{
    const char* camera_name = get_active_camera_name();

    RENDERER_LOG_INFO(
        "frame \"%s\" settings:\n"
        "  camera                        %s\n"
        "  resolution                    %s x %s\n"
        "  tile size                     %s x %s\n"
        "  filter                        %s\n"
        "  filter size                   %f\n"
        "  crop window                   (%s, %s)-(%s, %s)\n"
        "  denoising mode                %s\n"
        "  render stamp                  %s\n"
        "  save extra aovs               %s",
        get_path().c_str(),
        camera_name ? camera_name : "none",
        pretty_uint(impl->m_frame_width).c_str(),
        pretty_uint(impl->m_frame_height).c_str(),
        pretty_uint(impl->m_tile_width).c_str(),
        pretty_uint(impl->m_tile_height).c_str(),
        impl->m_filter_name.c_str(),
        impl->m_filter_radius,
        pretty_uint(impl->m_crop_window.min[0]).c_str(),
        pretty_uint(impl->m_crop_window.min[1]).c_str(),
        pretty_uint(impl->m_crop_window.max[0]).c_str(),
        pretty_uint(impl->m_crop_window.max[1]).c_str(),
        impl->m_denoising_mode == DenoisingMode::Off ? "off" :
        impl->m_denoising_mode == DenoisingMode::WriteOutputs ? "write outputs" : "denoise",
        impl->m_render_stamp_enabled ? "on" : "off",
        impl->m_save_extra_aovs ? "on" : "off");
}

const char* Frame::get_active_camera_name() const
{
    if (m_params.strings().exist("camera"))
        return m_params.strings().get("camera");

    return nullptr;
}

Image& Frame::image() const
{
    return *impl->m_image.get();
}

void Frame::clear_main_and_aov_images()
{
    impl->m_image->clear(Color4f(0.0));

    for (size_t i = 0, e = aovs().size(); i < e; ++i)
        aovs().get_by_index(i)->clear_image();

    for (size_t i = 0, e = internal_aovs().size(); i < e; ++i)
        internal_aovs().get_by_index(i)->clear_image();
}

ImageStack& Frame::aov_images() const
{
    return *impl->m_aov_images;
}

const AOVContainer& Frame::aovs() const
{
    return impl->m_aovs;
}

const AOVContainer& Frame::internal_aovs() const
{
    return impl->m_internal_aovs;
}

size_t Frame::create_extra_aov_image(const char* name) const
{
    const size_t index = aov_images().get_index(name);

    if (index == size_t(~0) && aov_images().size() < MaxAOVCount)
    {
        const size_t add_index = aov_images().append(name, 4, PixelFormatFloat);
        impl->m_extra_aovs.push_back(add_index);
        return add_index;
    }

    return index;
}

const Filter2f& Frame::get_filter() const
{
    return *impl->m_filter.get();
}

void Frame::reset_crop_window()
{
    impl->m_crop_window =
        AABB2u(
            Vector2u(0, 0),
            Vector2u(impl->m_frame_width - 1, impl->m_frame_height - 1));

    m_params.strings().remove("crop_window");
}

bool Frame::has_crop_window() const
{
    return
        impl->m_crop_window.min.x > 0 ||
        impl->m_crop_window.min.y > 0 ||
        impl->m_crop_window.max.x < impl->m_frame_width - 1 ||
        impl->m_crop_window.max.y < impl->m_frame_height - 1;
}

void Frame::set_crop_window(const AABB2u& crop_window)
{
    impl->m_crop_window = crop_window;

    m_params.insert("crop_window", crop_window);
}

const AABB2u& Frame::get_crop_window() const
{
    return impl->m_crop_window;
}

size_t Frame::get_pixel_count() const
{
    return impl->m_crop_window.volume();
}

void Frame::post_process_aov_images() const
{
    for (size_t i = 0, e = aovs().size(); i < e; ++i)
        aovs().get_by_index(i)->post_process_image();

    for (size_t i = 0, e = internal_aovs().size(); i < e; ++i)
        internal_aovs().get_by_index(i)->post_process_image();
}

bool Frame::is_render_stamp_enabled() const
{
    return impl->m_render_stamp_enabled;
}

void Frame::add_render_stamp(const RenderStampInfo& info) const
{
    RENDERER_LOG_INFO("adding render stamp...");

    // Render stamp settings.
    const float FontSize = 14.0f;
    const Color4f FontColor(0.8f, 0.8f, 0.8f, 1.0f);
    const Color4f BackgroundColor(0.0f, 0.0f, 0.0f, 0.9f);
    const float MarginH = 6.0f;
    const float MarginV = 4.0f;

    // Compute the final string.
    string text = impl->m_render_stamp_format;
    text = replace(text, "{lib-name}", Appleseed::get_lib_name());
    text = replace(text, "{lib-version}", Appleseed::get_lib_version());
    text = replace(text, "{lib-variant}", Appleseed::get_lib_variant());
    text = replace(text, "{lib-config}", Appleseed::get_lib_configuration());
    text = replace(text, "{lib-build-date}", Appleseed::get_lib_compilation_date());
    text = replace(text, "{lib-build-time}", Appleseed::get_lib_compilation_time());
    text = replace(text, "{render-time}", pretty_time(info.m_render_time, 1).c_str());
    text = replace(text, "{peak-memory}", pretty_size(System::get_peak_process_virtual_memory_size()).c_str());

    // Compute the height in pixels of the string.
    const CanvasProperties& props = impl->m_image->properties();
    const float image_height = static_cast<float>(props.m_canvas_height);
    const float text_height = TextRenderer::compute_string_height(FontSize, text.c_str());
    const float origin_y = image_height - text_height - MarginV;

    // Draw the background rectangle.
    Drawing::draw_filled_rect(
        *impl->m_image,
        Vector2i(
            0,
            truncate<int>(origin_y - MarginV)),
        Vector2i(
            static_cast<int>(props.m_canvas_width - 1),
            static_cast<int>(props.m_canvas_height - 1)),
        BackgroundColor);

    // Draw the string into the image.
    TextRenderer::draw_string(
        *impl->m_image,
        ColorSpaceLinearRGB,
        TextRenderer::Font::UbuntuL,
        FontSize,
        FontColor,
        MarginH + appleseed_seeds_16_width + MarginH,
        origin_y,
        text.c_str());

    // Blit the appleseed logo.
    Drawing::blit_bitmap(
        *impl->m_image,
        Vector2i(
            static_cast<int>(MarginH),
            static_cast<int>(props.m_canvas_height - appleseed_seeds_16_height - MarginV)),
        appleseed_seeds_16,
        appleseed_seeds_16_width,
        appleseed_seeds_16_height,
        PixelFormatUInt8,
        Color4f(1.0f, 1.0f, 1.0f, 0.8f));
}

Frame::DenoisingMode Frame::get_denoising_mode() const
{
    return impl->m_denoising_mode;
}

void Frame::denoise(
    const size_t        thread_count,
    IAbortSwitch*       abort_switch) const
{
    DenoiserOptions options;

    options.m_prefilter_spikes = m_params.get_optional<bool>("prefilter_spikes", true);

    options.m_prefilter_threshold_stddev_factor =
        m_params.get_optional<float>(
            "spike_threshold",
            options.m_prefilter_threshold_stddev_factor);

    options.m_prefilter_threshold_stddev_factor =
        m_params.get_optional<float>(
            "spike_threshold",
            options.m_prefilter_threshold_stddev_factor);

    options.m_histogram_patch_distance_threshold =
        m_params.get_optional<float>(
            "patch_distance_threshold",
            options.m_histogram_patch_distance_threshold);

    options.m_num_scales =
        m_params.get_optional<size_t>(
            "denoise_scales",
            options.m_num_scales);

    options.m_num_cores = thread_count;

    options.m_mark_invalid_pixels =
        m_params.get_optional<bool>("mark_invalid_pixels", false);

    assert(impl->m_denoiser_aov);

    impl->m_denoiser_aov->fill_empty_samples();

    Deepimf num_samples;
    impl->m_denoiser_aov->extract_num_samples_image(num_samples);

    Deepimf covariances;
    impl->m_denoiser_aov->compute_covariances_image(covariances);

    RENDERER_LOG_INFO("denoising beauty image...");
    denoise_beauty_image(
        image(),
        num_samples,
        impl->m_denoiser_aov->histograms_image(),
        covariances,
        options,
        abort_switch);

    for (size_t i = 0, e = aovs().size(); i < e; ++i)
    {
        const AOV* aov = aovs().get_by_index(i);

        if (aov->has_color_data())
        {
            RENDERER_LOG_INFO("denoising %s aov...", aov->get_name());
            denoise_aov_image(
                aov->get_image(),
                num_samples,
                impl->m_denoiser_aov->histograms_image(),
                covariances,
                options,
                abort_switch);
        }
    }
}

namespace
{
    void add_chromaticities(ImageAttributes& image_attributes)
    {
        // Scene-linear sRGB / Rec. 709 chromaticities.
        image_attributes.insert("white_xy_chromaticity", Vector2f(0.3127f, 0.3290f));
        image_attributes.insert("red_xy_chromaticity", Vector2f(0.64f, 0.33f));
        image_attributes.insert("green_xy_chromaticity", Vector2f(0.30f, 0.60f));
        image_attributes.insert("blue_xy_chromaticity",  Vector2f(0.15f, 0.06f));
    }

    void create_parent_directories(const bf::path& file_path)
    {
        const bf::path parent_path = file_path.parent_path();

        if (!parent_path.empty() && !bf::exists(parent_path))
        {
            bsys::error_code ec;
            if (!bf::create_directories(parent_path, ec))
            {
                RENDERER_LOG_ERROR(
                    "could not create directory %s: %s",
                    parent_path.c_str(),
                    ec.message().c_str());
            }
        }
    }

    void create_parent_directories(const char* file_path)
    {
        create_parent_directories(bf::path(file_path));
    }

    void write_exr_image(
        const bf::path&         file_path,
        const Image&            image,
        const ImageAttributes&  image_attributes,
        const AOV*              aov)
    {
        create_parent_directories(file_path);

        const std::string filename = file_path.string();

        OIIOImageFileWriter writer;

        writer.create(filename.c_str());

        if (aov)
        {
            if (aov->has_color_data())
            {
                // If the AOV has color data, assume we can save it as half floats.
                const CanvasProperties& props = image.properties();
                const Image half_image(image, props.m_tile_width, props.m_tile_height, PixelFormatHalf);

                writer.append_image(&half_image);
            }
            else
                writer.append_image(&image);

            writer.set_image_channels(aov->get_channel_count(), aov->get_channel_names());

        }
        else
        {
            writer.append_image(&image);
        }

        writer.set_image_attributes(image_attributes);
        writer.write();
        writer.destroy();
    }

    void transform_to_srgb(Tile& tile)
    {
        assert(tile.get_channel_count() == 4);
        assert(tile.get_pixel_format() == PixelFormatHalf);

        typedef Color<half, 4> Color4h;

        Color4h* pixel_ptr = reinterpret_cast<Color4h*>(tile.pixel(0));
        Color4h* pixel_end = pixel_ptr + tile.get_pixel_count();

        for (; pixel_ptr < pixel_end; ++pixel_ptr)
        {
            // Load the pixel color.
            Color4f color(*pixel_ptr);

            // Apply color space conversion and clamping.
            color.unpremultiply();
            color.rgb() = fast_linear_rgb_to_srgb(color.rgb());
            color = saturate(color);
            color.premultiply();

            // Store the pixel color.
            *pixel_ptr = color;
        }
    }

    void transform_to_srgb(Image& image)
    {
        const CanvasProperties& image_props = image.properties();

        for (size_t ty = 0; ty < image_props.m_tile_count_y; ++ty)
        {
            for (size_t tx = 0; tx < image_props.m_tile_count_x; ++tx)
                transform_to_srgb(image.tile(tx, ty));
        }
    }

    void write_png_image(
        const bf::path&         file_path,
        const Image&            image,
        const ImageAttributes&  image_attributes)
    {
        const CanvasProperties& props = image.properties();

        Image transformed_image(image);

        if (props.m_channel_count == 4)
            transform_to_srgb(transformed_image);

        create_parent_directories(file_path);

        const std::string filename = file_path.string();

        OIIOImageFileWriter writer;

        writer.create(filename.c_str());
        writer.append_image(&transformed_image);
        writer.set_image_output_format(PixelFormat::PixelFormatUInt8);
        writer.set_image_attributes(image_attributes);
        writer.write();
        writer.destroy();
    }

    bool write_image(
        const char*             file_path,
        const Image&            image,
        const AOV*              aov = nullptr)
    {
        assert(file_path);

        Stopwatch<DefaultWallclockTimer> stopwatch;
        stopwatch.start();

        bf::path bf_file_path(file_path);
        string extension = lower_case(bf_file_path.extension().string());

        if (extension.empty())
        {
            extension = ".exr";
            bf_file_path += extension;
        }

        ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
        add_chromaticities(image_attributes);

        try
        {
            if (extension == ".exr")
            {
                write_exr_image(
                    bf_file_path,
                    image,
                    image_attributes,
                    aov);
            }
            else if (extension == ".png")
            {
                write_png_image(
                    bf_file_path,
                    image,
                    image_attributes);
            }
            else
            {
                RENDERER_LOG_ERROR(
                    "failed to write image file %s: unsupported image format.",
                    file_path);

                return false;
            }
        }
        catch (const ExceptionIOError&)
        {
            RENDERER_LOG_ERROR(
                "failed to write image file %s: i/o error.",
                file_path);

            return false;
        }
        catch (const Exception& e)
        {
            RENDERER_LOG_ERROR(
                "failed to write image file %s: %s.",
                file_path,
                e.what());

            return false;
        }

        stopwatch.measure();

        RENDERER_LOG_INFO(
            "wrote image file %s in %s.",
            file_path,
            pretty_time(stopwatch.get_seconds()).c_str());

        return true;
    }

    bool write_extra_aovs(
        const ImageStack&       images,
        const vector<size_t>&   aov_indices,
        const bf::path&         directory,
        const string&           base_file_name,
        const string&           extension)
    {
        bool success = true;

        for (size_t i = 0, e = aov_indices.size(); i < e; ++i)
        {
            const size_t image_index = aov_indices[i];
            assert(image_index < images.size());

            const Image& image = images.get_image(image_index);

            // Compute image file path.
            const string aov_name = images.get_name(image_index);
            const string safe_aov_name = make_safe_filename(aov_name);
            const string aov_file_name = base_file_name + "." + safe_aov_name + extension;
            const string aov_file_path = (directory / aov_file_name).string();

            // Write AOV image.
            if (!write_image(aov_file_path.c_str(), image))
                success = false;
        }

        return success;
    }
}

bool Frame::write_main_image(const char* file_path) const
{
    assert(file_path);

    // Convert main image to half floats.
    const Image& image = *impl->m_image;
    const CanvasProperties& props = image.properties();
    const Image half_image(image, props.m_tile_width, props.m_tile_height, PixelFormatHalf);

    // Write main image.
    if (!write_image(file_path, half_image))
        return false;

    // Write BCD histograms and covariances if enabled.
    if (impl->m_denoising_mode == DenoisingMode::WriteOutputs)
    {
        bf::path boost_file_path(file_path);
        boost_file_path.replace_extension(".exr");

        impl->m_denoiser_aov->write_images(boost_file_path.string().c_str());
    }

    return true;
}

bool Frame::write_aov_images(const char* file_path) const
{
    assert(file_path);

    bf::path boost_file_path(file_path);
    boost_file_path.replace_extension(".exr");

    const bf::path directory = boost_file_path.parent_path();
    const string base_file_name = boost_file_path.stem().string();

    bool success = true;

    for (size_t i = 0, e = aovs().size(); i < e; ++i)
    {
        const AOV* aov = aovs().get_by_index(i);

        // Compute AOV image file path.
        const string aov_name = aov->get_name();
        const string safe_aov_name = make_safe_filename(aov_name);
        const string aov_file_name = base_file_name + "." + safe_aov_name + ".exr";
        const string aov_file_path = (directory / aov_file_name).string();

        if (!write_image(aov_file_path.c_str(), aov->get_image(), aov))
            success = false;
    }

    if (impl->m_save_extra_aovs)
    {
        success = success && write_extra_aovs(
            aov_images(),
            impl->m_extra_aovs,
            directory,
            base_file_name,
            ".exr");
    }

    return success;
}

bool Frame::write_main_and_aov_images() const
{
    bool success = true;

    // Get output filename.
    const string filepath = get_parameters().get_optional<string>("output_filename");

    // Write main image.
    if (!filepath.empty())
    {
        if (!write_main_image(filepath.c_str()))
            success = false;
    }

    // Write AOVs.
    for (size_t i = 0, e = aovs().size(); i < e; ++i)
    {
        const AOV* aov = aovs().get_by_index(i);

        // Get output filename.
        bf::path filepath = aov->get_parameters().get_optional<string>("output_filename");
        filepath.replace_extension(".exr");

        // Write AOV image.
        if (!filepath.empty())
        {
            if (!write_image(filepath.string().c_str(), aov->get_image(), aov))
                success = false;
        }
    }

    if (impl->m_save_extra_aovs)
    {
        bf::path boost_file_path(filepath);
        boost_file_path.replace_extension(".exr");

        const bf::path directory = boost_file_path.parent_path();
        const string base_file_name = boost_file_path.stem().string();

        success = success && write_extra_aovs(
            aov_images(),
            impl->m_extra_aovs,
            directory,
            base_file_name,
            ".exr");
    }

    return success;
}

void Frame::write_main_and_aov_images_to_multipart_exr(const char* file_path) const
{
    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
    add_chromaticities(image_attributes);

    std::vector<Image> images;

    create_parent_directories(file_path);

    OIIOImageFileWriter writer;

    writer.create(file_path);

    // Always save the main image as half floats.
    {
        const Image& image = *impl->m_image;
        const CanvasProperties& props = image.properties();
        images.emplace_back(image, props.m_tile_width, props.m_tile_height, PixelFormatHalf);

        image_attributes.insert("Name", "beauty");

        writer.append_image(&(images.back()));
        writer.set_image_attributes(image_attributes);
    }

    for (size_t i = 0, e = impl->m_aovs.size(); i < e; ++i)
    {
        const AOV* aov = impl->m_aovs.get_by_index(i);
        const string aov_name = aov->get_name();
        const Image& image = aov->get_image();

        if (aov->has_color_data())
        {
            // If the AOV has color data, assume we can save it as half floats.
            const CanvasProperties& props = image.properties();
            images.emplace_back(image, props.m_tile_width, props.m_tile_height, PixelFormatHalf);
            writer.append_image(&(images.back()));
        }
        else
            writer.append_image(&image);

        image_attributes.insert("Name", aov_name.c_str());

        writer.set_image_channels(aov->get_channel_count(), aov->get_channel_names());
        writer.set_image_attributes(image_attributes);
    }

    if (impl->m_save_extra_aovs)
    {
        for (size_t i = 0, e = impl->m_extra_aovs.size(); i < e; ++i)
        {
            const size_t image_index = impl->m_extra_aovs[i];
            assert(image_index < aov_images().size());

            const Image& image = aov_images().get_image(image_index);
            const CanvasProperties& props = image.properties();
            const string aov_name = aov_images().get_name(image_index);
            assert(props.m_channel_count == 4);

            image_attributes.insert("Name", aov_name.c_str());

            writer.append_image(&image);
            writer.set_image_attributes(image_attributes);
        }
    }

    writer.write();

    writer.destroy();

    RENDERER_LOG_INFO(
        "wrote multipart exr image file %s in %s.",
        file_path,
        pretty_time(stopwatch.get_seconds()).c_str());
}

bool Frame::archive(
    const char*         directory,
    char**              output_path) const
{
    assert(directory);

    // Construct the name of the image file.
    const string filename =
        "autosave." + get_time_stamp_string() + ".exr";

    // Construct the path to the image file.
    const string file_path = (bf::path(directory) / filename).string();

    // Return the path to the image file.
    if (output_path)
        *output_path = duplicate_string(file_path.c_str());

    return write_image(file_path.c_str(), *impl->m_image);
}

void Frame::extract_parameters()
{
    // Retrieve frame resolution parameter.
    {
        const Vector2i DefaultResolution(512, 512);
        Vector2i resolution = m_params.get_required<Vector2i>("resolution", DefaultResolution);
        if (resolution[0] < 1 || resolution[1] < 1)
        {
            RENDERER_LOG_ERROR(
                "invalid value \"%d %d\" for parameter \"%s\", using default value \"%d %d\".",
                resolution[0],
                resolution[1],
                "resolution",
                DefaultResolution[0],
                DefaultResolution[1]);
            resolution = DefaultResolution;
        }
        impl->m_frame_width = static_cast<size_t>(resolution[0]);
        impl->m_frame_height = static_cast<size_t>(resolution[1]);
    }

    // Retrieve tile size parameter.
    {
        const Vector2i DefaultTileSize(64, 64);
        Vector2i tile_size = m_params.get_optional<Vector2i>("tile_size", DefaultTileSize);
        if (tile_size[0] < 1 || tile_size[1] < 1)
        {
            RENDERER_LOG_ERROR(
                "invalid value \"%d %d\" for parameter \"%s\", using default value \"%d %d\".",
                tile_size[0],
                tile_size[1],
                "tile_size",
                DefaultTileSize[0],
                DefaultTileSize[1]);
            tile_size = DefaultTileSize;
        }
        impl->m_tile_width = static_cast<size_t>(tile_size[0]);
        impl->m_tile_height = static_cast<size_t>(tile_size[1]);
    }

    // Retrieve reconstruction filter parameters.
    {
        const char* DefaultFilterName = "blackman-harris";

        impl->m_filter_name = m_params.get_optional<string>("filter", DefaultFilterName);
        impl->m_filter_radius = m_params.get_optional<float>("filter_size", 1.5f);

        if (impl->m_filter_name == "box")
            impl->m_filter.reset(new BoxFilter2<float>(impl->m_filter_radius, impl->m_filter_radius));
        else if (impl->m_filter_name == "triangle")
            impl->m_filter.reset(new TriangleFilter2<float>(impl->m_filter_radius, impl->m_filter_radius));
        else if (impl->m_filter_name == "gaussian")
            impl->m_filter.reset(new FastGaussianFilter2<float>(impl->m_filter_radius, impl->m_filter_radius, 8.0f));
        else if (impl->m_filter_name == "mitchell")
            impl->m_filter.reset(new MitchellFilter2<float>(impl->m_filter_radius, impl->m_filter_radius, 1.0f/3, 1.0f/3));
        else if (impl->m_filter_name == "bspline")
            impl->m_filter.reset(new MitchellFilter2<float>(impl->m_filter_radius, impl->m_filter_radius, 1.0f, 0.0f));
        else if (impl->m_filter_name == "catmull")
            impl->m_filter.reset(new MitchellFilter2<float>(impl->m_filter_radius, impl->m_filter_radius, 0.0f, 0.5f));
        else if (impl->m_filter_name == "lanczos")
            impl->m_filter.reset(new LanczosFilter2<float>(impl->m_filter_radius, impl->m_filter_radius, 3.0f));
        else if (impl->m_filter_name == "blackman-harris")
            impl->m_filter.reset(new FastBlackmanHarrisFilter2<float>(impl->m_filter_radius, impl->m_filter_radius));
        else
        {
            RENDERER_LOG_ERROR(
                "invalid value \"%s\" for parameter \"%s\", using default value \"%s\".",
                impl->m_filter_name.c_str(),
                "filter",
                DefaultFilterName);
            impl->m_filter_name = DefaultFilterName;
            impl->m_filter.reset(new GaussianFilter2<float>(impl->m_filter_radius, impl->m_filter_radius, 8.0f));
        }
    }

    // Retrieve crop window parameter.
    const AABB2u default_crop_window(
        Vector2u(0, 0),
        Vector2u(impl->m_frame_width - 1, impl->m_frame_height - 1));
    impl->m_crop_window = m_params.get_optional<AABB2u>("crop_window", default_crop_window);

    // Retrieve render stamp parameters.
    impl->m_render_stamp_enabled = m_params.get_optional<bool>("enable_render_stamp", false);
    impl->m_render_stamp_format = m_params.get_optional<string>("render_stamp_format", DefaultRenderStampFormat);

    // Retrieve denoiser parameters.
    {
        const string denoise_mode = m_params.get_optional<string>("denoiser", "off");

        if (denoise_mode == "off")
            impl->m_denoising_mode = DenoisingMode::Off;
        else if (denoise_mode == "on")
            impl->m_denoising_mode = DenoisingMode::Denoise;
        else if (denoise_mode == "write_outputs")
            impl->m_denoising_mode = DenoisingMode::WriteOutputs;
        else
        {
            RENDERER_LOG_ERROR(
                "invalid value \"%s\" for parameter \"%s\", using default value \"%s\".",
                denoise_mode.c_str(),
                "denoiser",
                "off");
            impl->m_denoising_mode = DenoisingMode::Off;
        }
    }

    // Retrieve save extra AOVs parameter
    impl->m_save_extra_aovs = m_params.get_optional<bool>("save_extra_aovs", false);
}


//
// FrameFactory class implementation.
//

DictionaryArray FrameFactory::get_input_metadata()
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "camera")
            .insert("label", "Camera")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary().insert("camera", "Camera"))
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "resolution")
            .insert("label", "Resolution")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "crop_window")
            .insert("label", "Crop Window")
            .insert("type", "text")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "tile_size")
            .insert("label", "Tile Size")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "filter")
            .insert("label", "Filter")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Box", "box")
                    .insert("Triangle", "triangle")
                    .insert("Gaussian", "gaussian")
                    .insert("Mitchell-Netravali", "mitchell")
                    .insert("Cubic B-spline", "bspline")
                    .insert("Catmull-Rom Spline", "catmull")
                    .insert("Lanczos", "lanczos")
                    .insert("Blackman-Harris", "blackman-harris"))
            .insert("use", "optional")
            .insert("default", "blackman-harris"));

    metadata.push_back(
        Dictionary()
            .insert("name", "filter_size")
            .insert("label", "Filter Size")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.5")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "4.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "denoiser")
            .insert("label", "Denoiser")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Off", "off")
                    .insert("On", "on")
                    .insert("Write Outputs", "write_outputs"))
            .insert("use", "required")
            .insert("default", "off")
            .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "prefilter_spikes")
            .insert("label", "Prefilter Spikes")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "true")
            .insert("visible_if",
                Dictionary()
                    .insert("denoiser", "on")));

    metadata.push_back(
        Dictionary()
            .insert("name", "spike_threshold")
            .insert("label", "Spike Threshold")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.1")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "4.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "2.0")
            .insert("visible_if",
                Dictionary()
                    .insert("denoiser", "on")));

    metadata.push_back(
        Dictionary()
            .insert("name", "patch_distance_threshold")
            .insert("label", "Patch Distance")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.5")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "3.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("visible_if",
                Dictionary()
                    .insert("denoiser", "on")));

    metadata.push_back(
        Dictionary()
            .insert("name", "denoise_scales")
            .insert("label", "Denoise Scales")
            .insert("type", "integer")
            .insert("min",
                Dictionary()
                    .insert("value", "1")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "10")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "3")
            .insert("visible_if",
                Dictionary()
                    .insert("denoiser", "on")));

    metadata.push_back(
        Dictionary()
            .insert("name", "mark_invalid_pixels")
            .insert("label", "Mark Invalid pixels")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "false")
            .insert("visible_if",
                Dictionary()
                    .insert("denoiser", "on")));

    metadata.push_back(
        Dictionary()
            .insert("name", "enable_render_stamp")
            .insert("label", "Enable Render Stamp")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "false")
            .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "render_stamp_format")
            .insert("label", "Render Stamp Format")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", DefaultRenderStampFormat)
            .insert("visible_if",
                Dictionary()
                    .insert("enable_render_stamp", "true")));

    metadata.push_back(
        Dictionary()
            .insert("name", "save_extra_aovs")
            .insert("label", "Save Extra AOVs")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "false"));

    return metadata;
}

auto_release_ptr<Frame> FrameFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<Frame>(
            new Frame(name, params, AOVContainer()));
}

auto_release_ptr<Frame> FrameFactory::create(
    const char*         name,
    const ParamArray&   params,
    const AOVContainer& aovs)
{
    return
        auto_release_ptr<Frame>(
            new Frame(name, params, aovs));
}

}   // namespace renderer
