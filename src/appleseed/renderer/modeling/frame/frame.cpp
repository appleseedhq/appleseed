
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
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/core/exceptions/exceptionunsupportedfileformat.h"
#include "foundation/image/color.h"
#include "foundation/image/exceptionunsupportedimageformat.h"
#include "foundation/image/exrimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/imageattributes.h"
#include "foundation/image/pixel.h"
#include "foundation/image/pngimagefilewriter.h"
#include "foundation/image/tile.h"
#include "foundation/math/matrix.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/platform/timers.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cmath>
#include <memory>
#include <string>

using namespace foundation;
using namespace std;
namespace bf = boost::filesystem;

namespace renderer
{

//
// Frame class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID Frame::get_class_uid()
{
    return g_class_uid;
}

namespace
{

class WorkingColorSpace
{
  public:
    explicit WorkingColorSpace(const string& name = "scene_linear_srgb_rec_709")
      : m_name(name)
    {
        if (m_name == "scene_linear_rec_2020")
        {
            m_chromaticity_rxy = Vector2f(0.708f, 0.292f);
            m_chromaticity_gxy = Vector2f(0.170f, 0.797f);
            m_chromaticity_bxy = Vector2f(0.131f, 0.046f);
            m_chromaticity_wxy = Vector2f(0.31270f, 0.3290f);

            m_lighting_conditions = LightingConditions(IlluminantCIED65, XYZCMFCIE19312Deg);

            static const float rgb_to_xyz_coeffs[9] =
            {
                0.63695805f,  0.14461690f,  0.16888098f,
                0.26270021f,  0.67799807f,  0.05930172f,
                0.00000000f,  0.02807269f,  1.06098506f
            };
            m_rgb_to_xyz = Matrix3f(rgb_to_xyz_coeffs);

            static const float xyz_to_rgb_coeffs[9] =
            {
                1.71665119f, -0.35567078f, -0.25336628f,
               -0.66668435f,  1.61648124f,  0.01576855f,
                0.01763986f, -0.04277061f,  0.94210312f
            };
            m_xyz_to_rgb = Matrix3f(xyz_to_rgb_coeffs);
        }
        else if (m_name == "scene_linear_dci_p3")
        {
            m_chromaticity_rxy = Vector2f(0.6800f, 0.3200f);
            m_chromaticity_gxy = Vector2f(0.2650f, 0.6900f);
            m_chromaticity_bxy = Vector2f(0.1500f, 0.0600f);
            m_chromaticity_wxy = Vector2f(0.31400f, 0.3290f);

            m_lighting_conditions = LightingConditions(IlluminantCIED65, XYZCMFCIE19312Deg);

            static const float rgb_to_xyz_coeffs[9] =
            {
                0.44516982f,  0.27713441f,  0.17228267f,
                0.20949168f,  0.72159525f,  0.06891307f,
               -0.00000000f,  0.04706056f,  0.90735539f
            };
            m_rgb_to_xyz = Matrix3f(rgb_to_xyz_coeffs);

            static const float xyz_to_rgb_coeffs[9] =
            {
                2.72539403f, -1.01800301f, -0.44016320f,
               -0.79516803f,  1.68973205f,  0.02264719f,
                0.04124189f, -0.08763902f,  1.10092938f
            };
            m_xyz_to_rgb = Matrix3f(xyz_to_rgb_coeffs);
        }
        else if (m_name == "scene_linear_acescg_ap1")
        {
            m_chromaticity_rxy = Vector2f(0.7130f, 0.2930f);
            m_chromaticity_gxy = Vector2f(0.1650f, 0.8300f);
            m_chromaticity_bxy = Vector2f(0.1280f, 0.0440f);
            m_chromaticity_wxy = Vector2f(0.32168f, 0.33767f);

            m_lighting_conditions = LightingConditions(IlluminantCIED65, XYZCMFCIE19312Deg);

            static const float rgb_to_xyz_coeffs[9] =
            {
                0.66245418f,  0.13400421f,  0.15618769f,
                0.27222872f,  0.67408177f,  0.05368952f,
               -0.00557465f,  0.00406073f,  1.01033910f
            };
            m_rgb_to_xyz = Matrix3f(rgb_to_xyz_coeffs);

            static const float xyz_to_rgb_coeffs[9] =
            {
                1.64102338f, -0.32480329f, -0.23642470f,
               -0.66366286f,  1.61533159f,  0.01675635f,
                0.01172189f, -0.00828444f,  0.98839486f
            };
            m_xyz_to_rgb = Matrix3f(xyz_to_rgb_coeffs);
        }
        else // if (m_name == "scene_linear_srgb_rec_709")
        {
            m_chromaticity_rxy = Vector2f(0.64f, 0.33f);
            m_chromaticity_gxy = Vector2f(0.30f, 0.60f);
            m_chromaticity_bxy = Vector2f(0.15f, 0.06f);
            m_chromaticity_wxy = Vector2f(0.3127f, 0.3290f);

            m_lighting_conditions = LightingConditions(IlluminantCIED65, XYZCMFCIE19312Deg);

            static const float rgb_to_xyz_coeffs[9] =
            {
                0.41239080f, 0.35758434f, 0.18048079f,
                0.21263901f, 0.71516868f, 0.07219232f,
                0.01933082f, 0.11919478f, 0.95053215f
            };
            m_rgb_to_xyz = Matrix3f(rgb_to_xyz_coeffs);

            static const float xyz_to_rgb_coeffs[9] =
            {
                 3.24096994f, -1.53738318f, -0.49861076f,
                -0.96924364f,  1.87596750f,  0.04155506f,
                 0.05563008f, -0.20397696f,  1.05697151f
            };
            m_xyz_to_rgb = Matrix3f(xyz_to_rgb_coeffs);
        }
    }

    static const char* nice_name(const string& name)
    {
        if (name == "scene_linear_rec_2020")
            return "Scene Linear Rec 2020";
        else if (name == "scene_linear_dci_p3")
            return "Scene Linear DCI-P3";
        else if (name == "scene_linear_acescg_ap1")
            return "Scene Linear ACEScg AP1";
        else // if (m_name == "scene_linear_srgb_rec_709")
            return "Scene Linear sRGB / Rec 709";
    }

    void insert_chromaticity_attributes(
        ImageAttributes&        image_attributes) const
    {
        image_attributes.insert(
            "chromaticity_wxy", m_chromaticity_wxy);
        image_attributes.insert(
            "chromaticity_rxy", m_chromaticity_rxy);
        image_attributes.insert(
            "chromaticity_gxy", m_chromaticity_gxy);
        image_attributes.insert(
            "chromaticity_bxy", m_chromaticity_bxy);
    }

    Matrix3f            m_rgb_to_xyz;
    Matrix3f            m_xyz_to_rgb;
    LightingConditions  m_lighting_conditions;
    Vector2f            m_chromaticity_rxy;
    Vector2f            m_chromaticity_gxy;
    Vector2f            m_chromaticity_bxy;
    Vector2f            m_chromaticity_wxy;
    string              m_name;
};

}

struct Frame::Impl
{
    size_t                  m_frame_width;
    size_t                  m_frame_height;
    size_t                  m_tile_width;
    size_t                  m_tile_height;
    string                  m_filter_name;
    float                   m_filter_radius;
    auto_ptr<Filter2f>      m_filter;
    WorkingColorSpace       m_working_color_space;
    AABB2u                  m_crop_window;
    auto_ptr<Image>         m_image;
    auto_ptr<ImageStack>    m_aov_images;
    AOVContainer            m_aovs;

    Impl()
      : m_working_color_space()
    {
    }
};

Frame::Frame(
    const char*         name,
    const ParamArray&   params)
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
        "frame settings:\n"
        "  camera                        %s\n"
        "  resolution                    %s x %s\n"
        "  tile size                     %s x %s\n"
        "  filter                        %s\n"
        "  filter size                   %f\n"
        "  working color space           %s\n"
        "  premultiplied alpha           %s\n"
        "  crop window                   (%s, %s)-(%s, %s)",
        camera_name ? camera_name : "none",
        pretty_uint(impl->m_frame_width).c_str(),
        pretty_uint(impl->m_frame_height).c_str(),
        pretty_uint(impl->m_tile_width).c_str(),
        pretty_uint(impl->m_tile_height).c_str(),
        impl->m_filter_name.c_str(),
        impl->m_filter_radius,
        WorkingColorSpace::nice_name(impl->m_working_color_space.m_name),
        m_is_premultiplied_alpha ? "on" : "off",
        pretty_uint(impl->m_crop_window.min[0]).c_str(),
        pretty_uint(impl->m_crop_window.min[1]).c_str(),
        pretty_uint(impl->m_crop_window.max[0]).c_str(),
        pretty_uint(impl->m_crop_window.max[1]).c_str());
}

const char* Frame::get_active_camera_name() const
{
    if (m_params.strings().exist("camera"))
        return m_params.strings().get("camera");

    return 0;
}

Image& Frame::image() const
{
    return *impl->m_image.get();
}

void Frame::clear_main_image()
{
    impl->m_image->clear(Color4f(0.0));
}

ImageStack& Frame::aov_images() const
{
    return *impl->m_aov_images.get();
}

void Frame::add_aov(foundation::auto_release_ptr<AOV> aov)
{
    assert(aov.get());

    const size_t aov_index = aov_images().get_index(aov->get_name());
    if (aov_index == size_t(~0) && aov_images().size() < MaxAOVCount)
    {
        aov_images().append(
            aov->get_name(),
            4, // aov->get_channel_count(),
            PixelFormatFloat);
        aovs().insert(aov);
    }
    else
    {
        RENDERER_LOG_WARNING(
            "could not create %s aov, maximum number of aovs (" FMT_SIZE_T ") reached.",
            aov->get_name(),
            MaxAOVCount);
    }
}

void Frame::transfer_aovs(AOVContainer& aovs)
{
    while (!aovs.empty())
    {
        auto_release_ptr<AOV> aov = aovs.remove(aovs.get_by_index(0));
        add_aov(aov);
    }
}

AOVContainer& Frame::aovs() const
{
    return impl->m_aovs;
}

size_t Frame::create_extra_aov_image(const char* name) const
{
    const size_t index = aov_images().get_index(name);
    if (index == size_t(~0) && aov_images().size() < MaxAOVCount)
        return aov_images().append(name, 4, PixelFormatFloat);

    return index;
}

const Filter2f& Frame::get_filter() const
{
    return *impl->m_filter.get();
}

const LightingConditions& Frame::get_lighting_conditions() const
{
    return impl->m_working_color_space.m_lighting_conditions;
}

const float* Frame::get_xyz_to_rgb_matrix() const
{
    return &impl->m_working_color_space.m_xyz_to_rgb[0];
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

bool Frame::write_main_image(const char* file_path) const
{
    assert(file_path);

    ImageAttributes image_attributes =
        ImageAttributes::create_default_attributes();
    impl->m_working_color_space.insert_chromaticity_attributes(image_attributes);

    return write_image(file_path, *impl->m_image, image_attributes);
}

bool Frame::write_aov_images(const char* file_path) const
{
    assert(file_path);

    ImageAttributes image_attributes =
        ImageAttributes::create_default_attributes();
    impl->m_working_color_space.insert_chromaticity_attributes(image_attributes);

    bool result = true;

    if (!impl->m_aov_images->empty())
    {
        const bf::path boost_file_path(file_path);
        const bf::path directory = boost_file_path.parent_path();
        const string base_file_name = boost_file_path.stem().string();
        const string extension = boost_file_path.extension().string();

        for (size_t i = 0; i < impl->m_aov_images->size(); ++i)
        {
            const string aov_name = impl->m_aov_images->get_name(i);
            const string safe_aov_name = make_safe_filename(aov_name);
            const string aov_file_name = base_file_name + "." + safe_aov_name + extension;
            const string aov_file_path = (directory / aov_file_name).string();

            if (!write_image(
                    aov_file_path.c_str(),
                    impl->m_aov_images->get_image(i),
                    image_attributes))
                result = false;
        }
    }

    return result;
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

    return
        write_image(
            file_path.c_str(),
            *impl->m_image,
            ImageAttributes::create_default_attributes());
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

    // Retrieve reconstruction filter parameter.
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

    // Retrieve color management parameters.
    {
        const char* DefaultWorkingColorSpaceName = "scene_linear_srgb_rec_709";
        string working_color_space_name =
            m_params.get_optional<string>("working_color_space", DefaultWorkingColorSpaceName);

        if (working_color_space_name == "scene_linear_srgb_rec_709")
            impl->m_working_color_space = WorkingColorSpace(working_color_space_name);
        else if (working_color_space_name == "scene_linear_rec_2020")
            impl->m_working_color_space = WorkingColorSpace(working_color_space_name);
        else if (working_color_space_name == "scene_linear_dci_p3")
            impl->m_working_color_space = WorkingColorSpace(working_color_space_name);
        else if (working_color_space_name == "scene_linear_acescg_ap1")
            impl->m_working_color_space = WorkingColorSpace(working_color_space_name);
        else
        {
            RENDERER_LOG_ERROR(
                "invalid value \"%s\" for parameter \"%s\", using default value \"%s\".",
                working_color_space_name.c_str(),
                "working_color_space",
                DefaultWorkingColorSpaceName);
            impl->m_working_color_space = WorkingColorSpace(DefaultWorkingColorSpaceName);
        }
    }

    // Retrieve premultiplied alpha parameter.
    m_is_premultiplied_alpha = m_params.get_optional<bool>("premultiplied_alpha", true);

    // Retrieve crop window parameter.
    const AABB2u default_crop_window(
        Vector2u(0, 0),
        Vector2u(impl->m_frame_width - 1, impl->m_frame_height - 1));
    impl->m_crop_window = m_params.get_optional<AABB2u>("crop_window", default_crop_window);
}

bool Frame::write_image(
    const char*             file_path,
    const Image&            image,
    const ImageAttributes&  image_attributes) const
{
    assert(file_path);

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    const bf::path filepath(file_path);
    const string extension = lower_case(filepath.extension().string());

    try
    {
        if (extension == ".png")
            write_png_image(file_path, image, image_attributes);
        else if (extension == ".exr")
            write_exr_image(file_path, image, image_attributes);
        else if (extension.empty())
        {
            string file_path_with_ext(file_path);
            file_path_with_ext += ".exr";
            write_exr_image(file_path_with_ext.c_str(), image, image_attributes);
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

void Frame::write_exr_image(
    const char*             file_path,
    const Image&            image,
    const ImageAttributes&  image_attributes,
    const bool              convert_to_half) const
{
    EXRImageFileWriter writer;

    if (convert_to_half)
    {
        const CanvasProperties& props = image.properties();
        Image half_image(
            image,
            props.m_tile_width,
            props.m_tile_height,
            PixelFormatHalf);

        writer.write(file_path, half_image, image_attributes);
    }
    else
        writer.write(file_path, image, image_attributes);
}

namespace
{

void transform_to_srgb(Tile& tile)
{
    assert(tile.get_channel_count() == 4);

    Color4f* pixel_ptr = reinterpret_cast<Color4f*>(tile.pixel(0));
    Color4f* pixel_end = pixel_ptr + tile.get_pixel_count();

    for (; pixel_ptr < pixel_end; ++pixel_ptr)
    {
        // Load the pixel color.
        Color4f color(*pixel_ptr);

        // Apply color space conversion.
        color.rgb() = fast_linear_rgb_to_srgb(color.rgb());

        // Apply clamping.
        color = saturate(color);

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

}

void Frame::write_png_image(
    const char*             file_path,
    const Image&            image,
    const ImageAttributes&  image_attributes) const
{
    Image transformed_image(image);
    transform_to_srgb(transformed_image);

    PNGImageFileWriter writer;
    writer.write(file_path, transformed_image, image_attributes);
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
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "1.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "working_color_space")
            .insert("label", "Working Color Space")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert(WorkingColorSpace::nice_name("scene_linear_srgb_rec_709"), "scene_linear_srgb_rec_709")
                    .insert(WorkingColorSpace::nice_name("scene_linear_rec_2020"), "scene_linear_rec_2020")
                    .insert(WorkingColorSpace::nice_name("scene_linear_dci_p3"), "scene_linear_dci_p3")
                    .insert(WorkingColorSpace::nice_name("scene_linear_acescg_ap1"), "scene_linear_acescg_ap1"))
            .insert("use", "optional")
            .insert("default", "scene_linear_srgb_rec_709"));

    metadata.push_back(
        Dictionary()
            .insert("name", "premultiplied_alpha")
            .insert("label", "Premultiplied Alpha")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "true"));

    return metadata;
}

auto_release_ptr<Frame> FrameFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<Frame>(new Frame(name, params));
}

}   // namespace renderer
