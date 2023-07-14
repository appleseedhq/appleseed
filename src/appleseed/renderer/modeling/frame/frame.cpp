
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/rendering/ishadingresultframebufferfactory.h"
#include "renderer/kernel/rendering/shadingresultframebuffer.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/aov/aovfactoryregistrar.h"
#include "renderer/modeling/aov/denoiseraov.h"
#include "renderer/modeling/aov/iaovfactory.h"
#include "renderer/modeling/postprocessingstage/postprocessingstage.h"
#include "renderer/utility/bbox.h"
#include "renderer/utility/filesystem.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/analysis.h"
#include "foundation/image/color.h"
#include "foundation/image/conversion.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/genericprogressiveimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/image/imageattributes.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/math/filtersamplingtable.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/path.h"
#include "foundation/platform/types.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/stopwatch.h"

// Boost headers.
#include "boost/filesystem.hpp"

// BCD headers.
#include "bcd/DeepImage.h"
#include "bcd/ImageIO.h"

// Standard headers.
#include <algorithm>
#include <exception>
#include <memory>
#include <string>
#include <tuple>
#include <vector>

using namespace bcd;
using namespace foundation;
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

struct Frame::Impl
{
    // Parameters.
    size_t                               m_frame_width;
    size_t                               m_frame_height;
    size_t                               m_tile_width;
    size_t                               m_tile_height;
    std::string                          m_filter_name;
    float                                m_filter_radius;
    AABB2u                               m_crop_window;
    bool                                 m_enable_dithering;
    std::uint32_t                        m_noise_seed;
    DenoisingMode                        m_denoising_mode;
    bool                                 m_checkpoint_create;
    std::string                          m_checkpoint_create_path;
    bool                                 m_checkpoint_resume;
    std::string                          m_checkpoint_resume_path;
    std::string                          m_ref_image_path;

    // Child entities.
    AOVContainer                    m_aovs;
    DenoiserAOV*                    m_denoiser_aov;
    AOVContainer                    m_internal_aovs;
    AOVContainer                    m_lpe_aovs;
    PostProcessingStageContainer    m_post_processing_stages;

    // Images.
    std::unique_ptr<Image>               m_image;
    std::unique_ptr<Image>               m_ref_image;
    std::unique_ptr<ImageStack>          m_aov_images;

    // Internal state.
    std::unique_ptr<FilterSamplingTable> m_filter_sampling_table;
    ParamArray                           m_render_info;
    size_t                               m_initial_pass = 0;

    explicit Impl(Frame* parent)
      : m_aovs(parent)
      , m_internal_aovs(parent)
      , m_lpe_aovs(parent)
      , m_post_processing_stages(parent)
    {
    }
};

Frame::Frame(
    const char*         name,
    const ParamArray&   params,
    const AOVContainer& aovs,
    const AOVContainer& lpe_aovs,
    const SearchPaths&  search_paths)
  : Entity(g_class_uid, params)
  , impl(new Impl(this))
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

    // Load the reference image.
    if (!impl->m_ref_image_path.empty())
    {
        RENDERER_LOG_DEBUG("loading reference image %s...", impl->m_ref_image_path.c_str());

        try
        {
            GenericImageFileReader reader;
            impl->m_ref_image.reset(
                reader.read(
                    search_paths.qualify(impl->m_ref_image_path).c_str()));
        }
        catch (const ExceptionIOError&)
        {
            RENDERER_LOG_ERROR(
                "failed to load reference image %s; continuing without a reference image.",
                impl->m_ref_image_path.c_str());
        }

        if (impl->m_ref_image && !has_valid_ref_image())
        {
            RENDERER_LOG_ERROR(
                "the reference image has different dimensions than the frame; continuing without a reference image");
        }
    }

    // Create the image stack for AOVs.
    impl->m_aov_images.reset(
        new ImageStack(
            impl->m_frame_width,
            impl->m_frame_height,
            impl->m_tile_width,
            impl->m_tile_height));

    if (aovs.size() > MaxAOVCount)
    {
        RENDERER_LOG_WARNING(
            "could not create all aovs, keeping the first (" FMT_SIZE_T ") aovs.",
            MaxAOVCount);
    }

    // Copy and store AOVs.
    const AOVFactoryRegistrar aov_registrar;
    for (size_t i = 0, e = std::min(aovs.size(), MaxAOVCount); i < e; ++i)
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

    // Copy and store LPE AOVs.
    for (size_t i = 0, e = lpe_aovs.size(); i < e; ++i)
    {
        const AOV* original_lpe_aov = lpe_aovs.get_by_index(i);

        const IAOVFactory* aov_factory = aov_registrar.lookup(original_lpe_aov->get_model());
        assert(aov_factory);

        auto_release_ptr<AOV> aov = aov_factory->create(original_lpe_aov->get_parameters());

        aov->create_image(
            impl->m_frame_width,
            impl->m_frame_height,
            impl->m_tile_width,
            impl->m_tile_height,
            aov_images());

        impl->m_lpe_aovs.insert(aov);
    }

    // Create internal AOVs.
    if (impl->m_denoising_mode != DenoisingMode::Off)
    {
        auto_release_ptr<DenoiserAOV> aov = DenoiserAOVFactory::create();
        aov->set_parent(this);

        aov->create_image(
            impl->m_frame_width,
            impl->m_frame_height,
            impl->m_tile_width,
            impl->m_tile_height,
            aov_images());

        impl->m_denoiser_aov = aov.get();
        impl->m_internal_aovs.insert(auto_release_ptr<AOV>(aov));
    }
    else impl->m_denoiser_aov = nullptr;
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
        "frame \"%s\" (#" FMT_UNIQUE_ID ") parameters:\n"
        "  camera name                   \"%s\"\n"
        "  resolution                    %s x %s\n"
        "  tile size                     %s x %s\n"
        "  filter                        %s\n"
        "  filter size                   %f\n"
        "  crop window                   (%s, %s)-(%s, %s)\n"
        "  dithering                     %s\n"
        "  noise seed                    %s\n"
        "  denoising mode                %s\n"
        "  create checkpoint             %s\n"
        "  resume checkpoint             %s\n"
        "  reference image path          %s",
        get_path().c_str(),
        get_uid(),
        camera_name != nullptr ? camera_name : "none",
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
        impl->m_enable_dithering ? "on" : "off",
        pretty_uint(impl->m_noise_seed).c_str(),
        impl->m_denoising_mode == DenoisingMode::Off ? "off" :
        impl->m_denoising_mode == DenoisingMode::WriteOutputs ? "write outputs" : "denoise",
        impl->m_checkpoint_create ? impl->m_checkpoint_create_path.c_str() : "off",
        impl->m_checkpoint_resume ? impl->m_checkpoint_resume_path.c_str() : "off",
        impl->m_ref_image_path.empty() ? "n/a" : impl->m_ref_image_path.c_str());
}

const AOVContainer& Frame::aovs() const
{
    return impl->m_aovs;
}

const AOVContainer& Frame::lpe_aovs() const
{
    return impl->m_lpe_aovs;
}

PostProcessingStageContainer& Frame::post_processing_stages() const
{
    return impl->m_post_processing_stages;
}

const char* Frame::get_active_camera_name() const
{
    return
        m_params.strings().exist("camera")
            ? m_params.strings().get("camera")
            : nullptr;
}

Image& Frame::image() const
{
    return *impl->m_image.get();
}

Image* Frame::ref_image() const
{
    return impl->m_ref_image.get();
}

bool Frame::has_valid_ref_image() const
{
    return
        impl->m_ref_image &&
        are_images_compatible(*impl->m_image.get(), *impl->m_ref_image.get());
}

void Frame::clear_main_and_aov_images()
{
    impl->m_image->clear(Color4f(0.0));

    for (AOV& aov : impl->m_aovs)
        aov.clear_image();

    for (AOV& aov : impl->m_internal_aovs)
        aov.clear_image();
}

ImageStack& Frame::aov_images() const
{
    return *impl->m_aov_images;
}

const FilterSamplingTable& Frame::get_filter_sampling_table() const
{
    return *impl->m_filter_sampling_table;
}

size_t Frame::get_initial_pass() const
{
    return impl->m_initial_pass;
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

std::uint32_t Frame::get_noise_seed() const
{
    return impl->m_noise_seed;
}

void Frame::collect_asset_paths(StringArray& paths) const
{
    for (const AOV& aov : impl->m_aovs)
        aov.collect_asset_paths(paths);

    for (const PostProcessingStage& stage : impl->m_post_processing_stages)
        stage.collect_asset_paths(paths);
}

void Frame::update_asset_paths(const StringDictionary& mappings)
{
    for (AOV& aov : impl->m_aovs)
        aov.update_asset_paths(mappings);

    for (PostProcessingStage& stage : impl->m_post_processing_stages)
        stage.update_asset_paths(mappings);
}

bool Frame::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    if (!Entity::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    if (!invoke_on_frame_begin(impl->m_aovs, project, parent, recorder, abort_switch))
        return false;

    if (!invoke_on_frame_begin(impl->m_post_processing_stages, project, parent, recorder, abort_switch))
        return false;

    return true;
}

void Frame::post_process_aov_images() const
{
    for (AOV& aov : impl->m_aovs)
        aov.post_process_image(*this);

    for (AOV& aov : impl->m_internal_aovs)
        aov.post_process_image(*this);
}

ParamArray& Frame::render_info()
{
    return impl->m_render_info;
}

Frame::DenoisingMode Frame::get_denoising_mode() const
{
    return impl->m_denoising_mode;
}

void Frame::denoise(
    const size_t            thread_count,
    IAbortSwitch*           abort_switch) const
{
    DenoiserOptions options;

    const bool skip_denoised = m_params.get_optional<bool>("skip_denoised", true);
    options.m_marked_pixels_skipping_probability = skip_denoised ? 1.0f : 0.0f;

    options.m_use_random_pixel_order = m_params.get_optional<bool>("random_pixel_order", true);

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

    Deepimf num_samples_image;
    impl->m_denoiser_aov->extract_num_samples_image(num_samples_image);

    Deepimf covariances_image;
    impl->m_denoiser_aov->compute_covariances_image(covariances_image);

    RENDERER_LOG_INFO("denoising frame \"%s\"...", get_path().c_str());
    denoise_beauty_image(
        image(),
        num_samples_image,
        impl->m_denoiser_aov->histograms_image(),
        covariances_image,
        options,
        abort_switch);

    for (const AOV& aov : impl->m_aovs)
    {
        if (aov.has_color_data())
        {
            RENDERER_LOG_INFO("denoising aov \"%s\"...", aov.get_path().c_str());
            denoise_aov_image(
                aov.get_image(),
                num_samples_image,
                impl->m_denoiser_aov->histograms_image(),
                covariances_image,
                options,
                abort_switch);
        }
    }
}

namespace
{
    size_t get_checkpoint_total_channel_count(const size_t aov_count)
    {
        // The beauty image plus the shading result framebuffer.
        return ShadingResultFrameBuffer::get_total_channel_count(aov_count) + 1;
    }

    typedef std::vector<std::tuple<std::string, CanvasProperties, ImageAttributes>> CheckpointProperties;


    //
    // Interface used to save the rendering buffer in checkpoints.
    //

    class ShadingBufferCanvas
      : public ICanvas
    {
      public:
        ShadingBufferCanvas(
            const Frame&                        frame,
            IShadingResultFrameBufferFactory*   buffer_factory)
          : m_buffer_factory(buffer_factory)
          , m_frame(frame)
          , m_props(
                m_frame.image().properties().m_canvas_width,
                m_frame.image().properties().m_canvas_height,
                m_frame.image().properties().m_tile_width,
                m_frame.image().properties().m_tile_height,
                get_checkpoint_total_channel_count(m_frame.aov_images().size()),
                PixelFormatFloat)
        {
            assert(buffer_factory);
        }

        const CanvasProperties& properties() const override
        {
            return m_props;
        }

        Tile& tile(
            const size_t    tile_x,
            const size_t    tile_y) override
        {
            ShadingResultFrameBuffer* tile_buffer = m_buffer_factory->create(
                m_frame,
                tile_x,
                tile_y,
                get_tile_bbox(tile_x, tile_y));

            assert(tile_buffer);
            return *tile_buffer;
        }

        const Tile& tile(
            const size_t    tile_x,
            const size_t    tile_y) const override
        {
            const ShadingResultFrameBuffer* tile_buffer = m_buffer_factory->create(
                m_frame,
                tile_x,
                tile_y,
                get_tile_bbox(tile_x, tile_y));

            assert(tile_buffer);
            return *tile_buffer;
        }

      private:
        IShadingResultFrameBufferFactory*   m_buffer_factory;
        const Frame&                        m_frame;
        const CanvasProperties              m_props;

        AABB2i get_tile_bbox(
            const size_t    tile_x,
            const size_t    tile_y) const
        {
            const Image& image = m_frame.image();
            const CanvasProperties& props = image.properties();

            // Compute the image space bounding box of the tile.
            const size_t tile_origin_x = props.m_tile_width * tile_x;
            const size_t tile_origin_y = props.m_tile_height * tile_y;

            const Tile& frame_tile = image.tile(tile_x, tile_y);

            // Compute the tile space bounding box of the pixels to render.
            return compute_tile_space_bbox(
                tile_origin_x,
                tile_origin_y,
                frame_tile.get_width(),
                frame_tile.get_height(),
                m_frame.get_crop_window());
        }
    };

    void get_denoiser_checkpoint_paths(
        const std::string&                   checkpoint_path,
        std::string&                         hist_path,
        std::string&                         cov_path,
        std::string&                         sum_path)
    {
        const bf::path boost_file_path(checkpoint_path);
        const bf::path directory = boost_file_path.parent_path();
        const std::string base_file_name = boost_file_path.stem().string() + ".denoiser";
        const std::string extension = boost_file_path.extension().string();

        const std::string hist_file_name = base_file_name + ".hist" + extension;
        hist_path = (directory / hist_file_name).string();

        const std::string cov_file_name = base_file_name + ".cov" + extension;
        cov_path = (directory / cov_file_name).string();

        const std::string sum_file_name = base_file_name + ".sum" + extension;
        sum_path = (directory / sum_file_name).string();
    }

    bool is_checkpoint_compatible(
        const std::string&              checkpoint_path,
        const Frame&                    frame,
        const CheckpointProperties&     checkpoint_props)
    {
        const Image& frame_image = frame.image();
        const CanvasProperties& frame_props = frame_image.properties();
        const CanvasProperties& beauty_props = std::get<1>(checkpoint_props[0]);
        const ImageAttributes& exr_attributes = std::get<2>(checkpoint_props[0]);
        const std::string initial_layer_name = std::get<0>(checkpoint_props[0]);

        // Check for atttributes.
        if (!exr_attributes.exist("appleseed:LastPass"))
        {
            RENDERER_LOG_ERROR("incorrect checkpoint: some attributes are missing.");
            return false;
        }

        // Check for beauty layer.
        if (initial_layer_name != "beauty")
        {
            RENDERER_LOG_ERROR("incorrect checkpoint: beauty layer is missing.");
            return false;
        }

        // Check for shading buffer layer.
        const std::string second_layer_name = std::get<0>(checkpoint_props[1]);
        if (second_layer_name != "appleseed:RenderingBuffer")
        {
            RENDERER_LOG_ERROR("incorrect checkpoint: rendering buffer layer is missing.");
            return false;
        }

        // Check that the shading buffer layer has correct amount of channel.
        // The checkpoint should contain the beauty image and the shading buffer.
        const size_t expect_channel_count = get_checkpoint_total_channel_count(frame.aov_images().size());
        if (std::get<1>(checkpoint_props[1]).m_channel_count != expect_channel_count)
        {
            RENDERER_LOG_ERROR("incorrect checkpoint: the shading buffer doesn't contain the correct number of channels.");
            return false;
        }

        // Check canvas properties.
        if (frame_props.m_canvas_width != beauty_props.m_canvas_width ||
            frame_props.m_canvas_height != beauty_props.m_canvas_height ||
            frame_props.m_tile_width != beauty_props.m_tile_width ||
            frame_props.m_tile_height != beauty_props.m_tile_height ||
            frame_props.m_channel_count != beauty_props.m_channel_count ||
            frame_props.m_pixel_format != beauty_props.m_pixel_format)
        {
            RENDERER_LOG_ERROR("incorrect checkpoint: the beauty layer properties doesn't match the renderer properties.");
            return false;
        }

        // Check if AOVs are here.
        if (checkpoint_props.size() < frame.aovs().size() + 2)
        {
            RENDERER_LOG_ERROR("incorrect checkpoint: some aovs are missing.");
            return false;
        }

        // Check if denoising is enabled and pass exists.
        if (frame.get_denoising_mode() != Frame::DenoisingMode::Off)
        {
            std::string hist_file_path, cov_file_path, sum_file_path;
            get_denoiser_checkpoint_paths(
                checkpoint_path,
                hist_file_path,
                cov_file_path,
                sum_file_path);

            if (!bf::exists(bf::path(hist_file_path.c_str())) ||
                !bf::exists(bf::path(cov_file_path.c_str())) ||
                !bf::exists(bf::path(sum_file_path.c_str())))
            {
                RENDERER_LOG_ERROR("cannot load denoiser's checkpoint from disk because one or several files are missing.");
                return false;
            }
        }

        return true;
    }

    bool load_denoiser_checkpoint(
        const std::string&              checkpoint_path,
        DenoiserAOV*                    denoiser_aov)
    {
        // todo: reload denoiser checkpoint from the same file.
        Deepimf& histograms_image = denoiser_aov->histograms_image();
        Deepimf& covariance_image = denoiser_aov->covariance_image();
        Deepimf& sum_image = denoiser_aov->sum_image();

        std::string hist_file_path, cov_file_path, sum_file_path;
        get_denoiser_checkpoint_paths(checkpoint_path, hist_file_path, cov_file_path, sum_file_path);

        // Load histograms.
        bool result = ImageIO::loadMultiChannelsEXR(histograms_image, hist_file_path.c_str());

        // Load covariance accumulator.
        result = result && ImageIO::loadMultiChannelsEXR(covariance_image, cov_file_path.c_str());

        // Load sum accumulator.
        result = result && ImageIO::loadMultiChannelsEXR(sum_image, sum_file_path.c_str());

        if (!result)
            RENDERER_LOG_ERROR("could not load denoiser checkpoint.");

        return result;
    }

    void save_denoiser_checkpoint(
        const std::string&              checkpoint_path,
        const DenoiserAOV*              denoiser_aov)
    {
        // todo: save denoiser checkpoint in the same file.
        const Deepimf& histograms_image = denoiser_aov->histograms_image();
        const Deepimf& covariance_image = denoiser_aov->covariance_image();
        const Deepimf& sum_image = denoiser_aov->sum_image();

        std::string hist_file_path, cov_file_path, sum_file_path;
        get_denoiser_checkpoint_paths(checkpoint_path, hist_file_path, cov_file_path, sum_file_path);

        // Add histograms layer.
        bool result = ImageIO::writeMultiChannelsEXR(histograms_image, hist_file_path.c_str());

        // Add covariances layer.
        result = result && ImageIO::writeMultiChannelsEXR(covariance_image, cov_file_path.c_str());

        // Add sum layer.
        result = result && ImageIO::writeMultiChannelsEXR(sum_image, sum_file_path.c_str());

        if (!result)
            RENDERER_LOG_ERROR("could not save denoiser checkpoint.");
    }
}

bool Frame::load_checkpoint(
    IShadingResultFrameBufferFactory*   buffer_factory,
    const size_t                        pass_count)
{
    if  (!impl->m_checkpoint_resume)
        return true;

    bf::path bf_path(impl->m_checkpoint_resume_path.c_str());

    // Check if the file exists.
    if (!bf::exists(bf_path))
    {
        RENDERER_LOG_WARNING("no checkpoint found, starting a new render.");
        return true;
    }

    // Open the file.
    GenericProgressiveImageFileReader reader;
    reader.open(impl->m_checkpoint_resume_path.c_str());

    // First, read layers name and properties.
    CheckpointProperties checkpoint_props;
    size_t layer_index = 0;
    while (reader.choose_subimage(layer_index))
    {
        CanvasProperties layer_canvas_props;
        reader.read_canvas_properties(layer_canvas_props);

        ImageAttributes layer_attributes;
        reader.read_image_attributes(layer_attributes);

        const std::string layer_name =
            layer_attributes.exist("name")
                ? layer_attributes.get<std::string>("name")
                : "undefined";

        checkpoint_props.emplace_back(
            layer_name,
            layer_canvas_props,
            layer_attributes);

        ++layer_index;
    }

    // Check checkpoint file's compatibility.
    if (!is_checkpoint_compatible(impl->m_checkpoint_resume_path, *this, checkpoint_props))
        return false;

    // Compute the index of the first pass to render.
    const size_t start_pass = std::get<2>(checkpoint_props[0]).get<size_t>("appleseed:LastPass") + 1;

    // Check if passes have already been rendered.
    if (start_pass < pass_count)
    {
        // Interface the shading buffer in a canvas.
        ShadingBufferCanvas shading_canvas(*this, buffer_factory);

        try
        {
            // Load tiles from the checkpoint.
            const CanvasProperties& beauty_props = std::get<1>(checkpoint_props[0]);
            for (size_t tile_y = 0; tile_y < beauty_props.m_tile_count_y; ++tile_y)
            {
                for (size_t tile_x = 0; tile_x < beauty_props.m_tile_count_x; ++tile_x)
                {
                    // Read shading buffer.
                    reader.choose_subimage(1);
                    Tile& shading_tile = shading_canvas.tile(tile_x, tile_y);
                    reader.read_tile(tile_x, tile_y, &shading_tile);

                    // No need to read beauty and filtered AOVs because they
                    // are in the shading buffer.

                    // Read unfiltered AOV layers.
                    std::vector<std::unique_ptr<Tile>> aov_tiles;
                    std::vector<const float*> aov_ptrs;
                    for (size_t aov_index = 0; aov_index < aovs().size(); ++aov_index)
                    {
                        UnfilteredAOV* aov = dynamic_cast<UnfilteredAOV*>(aovs().get_by_index(aov_index));
                        if (aov == nullptr)
                            continue;

                        const std::string aov_name = aov->get_name();

                        // Search layer index in the file.
                        size_t subimage_index(~0);
                        for (size_t prop_index = 0; prop_index < checkpoint_props.size(); ++prop_index)
                        {
                            if (std::get<0>(checkpoint_props[prop_index]) == aov_name)
                            {
                                subimage_index = prop_index;
                                break;
                            }
                        }
                        assert(subimage_index != size_t(~0));

                        // Read AOV tile.
                        Image& aov_image = aov->get_image();
                        Tile& aov_tile = aov_image.tile(tile_x, tile_y);
                        reader.choose_subimage(subimage_index);
                        reader.read_tile(tile_x, tile_y, &aov_tile);
                    }
                }
            }

            // Load internal AOVs (from external files).
            for (size_t i = 0, e = internal_aovs().size(); i < e; ++i)
            {
                AOV* aov = internal_aovs().get_by_index(i);
                DenoiserAOV* denoiser_aov = dynamic_cast<DenoiserAOV*>(aov);

                // Load denoiser checkpoint.
                if (denoiser_aov != nullptr)
                {
                    if (!load_denoiser_checkpoint(impl->m_checkpoint_resume_path, denoiser_aov))
                        return false;
                }
            }
        }
        catch (const ExceptionIOError& ex)
        {
            RENDERER_LOG_ERROR(
                "failed to load checkpoint file %s: %s",
                impl->m_checkpoint_resume_path.c_str(),
                ex.what());
            return false;
        }
    }

    RENDERER_LOG_INFO(
        "successfully read checkpoint file %s, resuming rendering at pass %s.",
        impl->m_checkpoint_resume_path.c_str(),
        pretty_uint(start_pass + 1).c_str());

    impl->m_initial_pass = start_pass;

    return true;
}

void Frame::save_checkpoint(
    IShadingResultFrameBufferFactory*   buffer_factory,
    const size_t                        pass_index) const
{
    if (!impl->m_checkpoint_create)
        return;

    create_parent_directories(impl->m_checkpoint_create_path.c_str());

    GenericImageFileWriter writer(impl->m_checkpoint_create_path.c_str());

    // Add the beauty image.
    {
        writer.append_image(&image());
        ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
        image_attributes.insert("appleseed:LastPass", pass_index);
        image_attributes.insert("image_name", "beauty");
        writer.set_image_attributes(image_attributes);
    }

    // Buffer containing pixels' weight.
    ShadingBufferCanvas pixels_weight_buffer(*this, buffer_factory);
    const size_t shading_channel_count = pixels_weight_buffer.properties().m_channel_count;

    // Create channel names.
    std::vector<std::string> shading_channel_names;
    std::vector<const char *> shading_channel_names_cstr;
    {
        static const std::string channel_name_prefix = "channel_";
        for (size_t i = 0; i < shading_channel_count; ++i)
        {
            shading_channel_names.push_back(channel_name_prefix + pad_left(to_string(i + 1), '0', 4));
            shading_channel_names_cstr.push_back(shading_channel_names[i].c_str());
        }

        assert(shading_channel_names.size() == shading_channel_count);
    }

    // Add the shading buffer.
    {
        writer.append_image(&pixels_weight_buffer);
        writer.set_image_channels(shading_channel_count, shading_channel_names_cstr.data());

        ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
        image_attributes.insert("image_name", "appleseed:RenderingBuffer");
        writer.set_image_attributes(image_attributes);
    }

    // Add AOV images.
    for (const AOV& aov : aovs())
    {
        const char* aov_name = aov.get_name();
        const Image& aov_image = aov.get_image();
        writer.append_image(&aov_image);
        writer.set_image_channels(aov.get_channel_count(), aov.get_channel_names());

        ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
        image_attributes.insert("image_name", aov_name);
        writer.set_image_attributes(image_attributes);
    }

    // Write the file.
    writer.write();

    // Add internal AOVs layers (in external files).
    for (const AOV& aov : internal_aovs())
    {
        // Save denoiser checkpoint.
        const DenoiserAOV* denoiser_aov = dynamic_cast<const DenoiserAOV*>(&aov);
        if (denoiser_aov != nullptr)
            save_denoiser_checkpoint(impl->m_checkpoint_create_path, denoiser_aov);
    }

    RENDERER_LOG_INFO(
        "wrote pass %s to checkpoint file %s.",
        pretty_uint(pass_index + 1).c_str(),
        impl->m_checkpoint_create_path.c_str());
}

namespace
{
    void add_chromaticities_attributes(ImageAttributes& image_attributes)
    {
        // Scene-linear sRGB / Rec. 709 chromaticities.
        image_attributes.insert("white_xy_chromaticity", Vector2f(0.3127f, 0.3290f));
        image_attributes.insert("red_xy_chromaticity", Vector2f(0.64f, 0.33f));
        image_attributes.insert("green_xy_chromaticity", Vector2f(0.30f, 0.60f));
        image_attributes.insert("blue_xy_chromaticity",  Vector2f(0.15f, 0.06f));
    }

    //
    // Default export formats:
    //
    // OpenEXR   .exr          4-channel   16-bit (half)                            Linear
    // RGBE      .hdr          3-channel   32-bit (8-bit RGB + shared exponent)     Linear
    // TIFF      .tiff/.tif    4-channel   16-bit (std::uint16_t)                   Linear
    // BMP       .bmp          4-channel    8-bit (std::uint8_t)                    sRGB
    // PNG       .png          4-channel    8-bit (std::uint8_t)                    sRGB
    // JPEG      .jpg/.jpe/    3-channel    8-bit (std::uint8_t)                    sRGB
    //           .jpeg/.jif/
    //           .jfif/.jfi
    //

    bool write_image(
        const Frame&            frame,
        const char*             file_path,
        const Image&            image,
        ImageAttributes         image_attributes)
    {
        assert(file_path);

        Stopwatch<DefaultWallclockTimer> stopwatch;
        stopwatch.start();

        bf::path bf_file_path(file_path);
        std::string extension = lower_case(bf_file_path.extension().string());

        if (!has_extension(bf_file_path))
        {
            extension = ".exr";
            bf_file_path.replace_extension(extension);
        }

        add_chromaticities_attributes(image_attributes);

        try
        {
            const bool is_linear_format = is_linear_image_file_format(bf_file_path);

            std::unique_ptr<Image> transformed_image;
            if (!is_linear_format && image.properties().m_channel_count == 4)
            {
                transformed_image.reset(new Image(image));
                convert_linear_rgb_to_srgb(*transformed_image);
            }
            else if (extension == ".hdr")
            {
                // HDR files only support 3 channel.
                const size_t shuffle_table[4] = { 0, 1, 2, Pixel::SkipChannel };
                transformed_image.reset(new Image(image, image.properties().m_pixel_format, shuffle_table));
            }
            else if (is_linear_format)
            {
                transformed_image.reset(new Image(image));
            }
            else
            {
                RENDERER_LOG_ERROR(
                    "failed to write image file %s: unsupported image format.",
                    bf_file_path.string().c_str());
                return false;
            }

            image_attributes.insert("color_space",
                is_linear_format ? "linear" : "sRGB");

            create_parent_directories(bf_file_path);

            const std::string filename = bf_file_path.string();
            GenericImageFileWriter writer(filename.c_str());

            writer.append_image(transformed_image.get());
            writer.set_image_attributes(image_attributes);

            if (extension == ".tiff" || extension == ".tif")
                writer.set_image_output_format(PixelFormat::PixelFormatUInt16);

            writer.write();
        }
        catch (const std::exception& e)
        {
            RENDERER_LOG_ERROR(
                "failed to write image file %s for frame \"%s\": %s.",
                bf_file_path.string().c_str(),
                frame.get_path().c_str(),
                e.what());

            return false;
        }

        stopwatch.measure();

        RENDERER_LOG_INFO(
            "wrote image file %s for frame \"%s\" in %s.",
            bf_file_path.string().c_str(),
            frame.get_path().c_str(),
            pretty_time(stopwatch.get_seconds()).c_str());

        return true;
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
    ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
    if (impl->m_enable_dithering)
        image_attributes.insert("dither", 42);  // the value of the dither attribute is a hash seed
    if (!write_image(*this, file_path, half_image, image_attributes))
        return false;

    // Write BCD histograms and covariance AOVs if enabled.
    if (impl->m_denoising_mode == DenoisingMode::WriteOutputs)
    {
        bf::path aov_file_path(file_path);
        aov_file_path.replace_extension(".exr");

        ImageAttributes aov_image_attributes = ImageAttributes::create_default_attributes();
        if (!impl->m_denoiser_aov->write_images(aov_file_path.string().c_str(), aov_image_attributes))
            return false;
    }

    return true;
}

bool Frame::write_aov_images(const char* file_path) const
{
    assert(file_path);

    if (impl->m_aovs.empty() && impl->m_lpe_aovs.empty())
        return true;

    bf::path bf_file_path(file_path);
    const std::string extension = lower_case(bf_file_path.extension().string());

    if (extension != ".exr")
    {
        if (has_extension(bf_file_path))
        {
            RENDERER_LOG_WARNING(
                "aovs cannot be saved to %s files; saving them to exr files instead.",
                extension.substr(1).c_str());
        }

        bf_file_path.replace_extension(".exr");
    }

    const bf::path directory = bf_file_path.parent_path();
    const std::string base_file_name = bf_file_path.stem().string();

    bool success = true;

    for (const AOV& aov : impl->m_aovs)
    {
        // Compute AOV image file path.
        const std::string aov_name = aov.get_name();
        const std::string safe_aov_name = make_safe_filename(aov_name);
        const std::string aov_file_name = base_file_name + "." + safe_aov_name + ".exr";
        const std::string aov_file_path = (directory / aov_file_name).string();

        // Write AOV image.
        ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
        if (!aov.write_images(aov_file_path.c_str(), image_attributes))
            success = false;
    }

    // Write LPE AOV images here
    for (const AOV& lpe_aov : impl->m_lpe_aovs)
    {
        // Compute AOV image file path.
        const std::string aov_name = lpe_aov.get_name();
        const std::string safe_aov_name = make_safe_filename(aov_name);
        const std::string aov_file_name = base_file_name + "." + safe_aov_name + ".exr";
        const std::string aov_file_path = (directory / aov_file_name).string();

        // Write AOV image
        ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
        if (!lpe_aov.write_images(aov_file_path.c_str(), image_attributes))
            success = false;
    }

    return success;
}

bool Frame::write_main_and_aov_images() const
{
    bool success = true;

    // Write main image.
    {
        const std::string file_path = get_parameters().get_optional<std::string>("output_filename");
        if (!file_path.empty())
        {
            if (!write_main_image(file_path.c_str()))
                success = false;
        }
    }

    // Write AOV images.
    for (const AOV& aov : impl->m_aovs)
    {
        bf::path bf_file_path = aov.get_parameters().get_optional<std::string>("output_filename");
        if (!bf_file_path.empty())
        {
            const std::string extension = lower_case(bf_file_path.extension().string());
            if (extension != ".exr")
            {
                if (has_extension(bf_file_path))
                {
                    RENDERER_LOG_WARNING(
                        "aov \"%s\" cannot be saved to %s file; saving it to exr file instead.",
                        aov.get_path().c_str(),
                        extension.substr(1).c_str());
                }

                bf_file_path.replace_extension(".exr");
            }

            ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
            if (!aov.write_images(bf_file_path.string().c_str(), image_attributes))
                success = false;
        }
    }

    // Write LPE AOV images.
    for (const AOV& lpe_aov : impl->m_lpe_aovs)
    {
        bf::path bf_file_path = lpe_aov.get_parameters().get_optional<std::string>("output_filename");
        if (!bf_file_path.empty())
        {
            const std::string extension = lower_case(bf_file_path.extension().string());
            if (extension != ".exr")
            {
                if (has_extension(bf_file_path))
                {
                    RENDERER_LOG_WARNING(
                        "aov \"%s\" cannot be saved to %s file; saving it to exr file instead.",
                        lpe_aov.get_path().c_str(),
                        extension.substr(1).c_str());
                }

                bf_file_path.replace_extension(".exr");
            }

            ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
            if (!lpe_aov.write_images(bf_file_path.string().c_str(), image_attributes))
                success = false;
        }
    }

    return success;
}

void Frame::write_main_and_aov_images_to_multipart_exr(const char* file_path) const
{
    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
    add_chromaticities_attributes(image_attributes);
    image_attributes.insert("color_space", "linear");

    std::vector<Image> images;

    create_parent_directories(file_path);

    GenericImageFileWriter writer(file_path);

    // Always save the main image as half floats.
    {
        const Image& image = *impl->m_image;
        const CanvasProperties& props = image.properties();
        images.emplace_back(image, props.m_tile_width, props.m_tile_height, PixelFormatHalf);

        image_attributes.insert("image_name", "beauty");

        writer.append_image(&(images.back()));
        writer.set_image_attributes(image_attributes);
    }

    for (const AOV& aov : impl->m_aovs)
    {
        const std::string aov_name = aov.get_name();
        const Image& image = aov.get_image();

        if (aov.has_color_data())
        {
            // If the AOV has color data, assume we can save it as half floats.
            const CanvasProperties& props = image.properties();
            images.emplace_back(image, props.m_tile_width, props.m_tile_height, PixelFormatHalf);
            writer.append_image(&(images.back()));
        }
        else writer.append_image(&image);

        image_attributes.insert("image_name", aov_name.c_str());

        writer.set_image_channels(aov.get_channel_count(), aov.get_channel_names());
        writer.set_image_attributes(image_attributes);
    }

    // Add LPE AOV images
    for (const AOV& lpe_aov : impl->m_aovs)
    {
        const std::string aov_name = lpe_aov.get_name();
        const Image& image = lpe_aov.get_image();

        if (lpe_aov.has_color_data())
        {
            const CanvasProperties& props = image.properties();
            images.emplace_back(image, props.m_tile_width, props.m_tile_height, PixelFormatHalf);
            writer.append_image(&(images.back()));
        }
        else writer.append_image(&image);

        image_attributes.insert("image_name", aov_name.c_str());

        writer.set_image_channels(lpe_aov.get_channel_count(), lpe_aov.get_channel_names());
        writer.set_image_attributes(image_attributes);
    }

    writer.write();

    RENDERER_LOG_INFO(
        "wrote multipart exr image file %s in %s.",
        file_path,
        pretty_time(stopwatch.get_seconds()).c_str());
}

bool Frame::archive(
    const char*     directory,
    char**          output_path) const
{
    assert(directory);

    // Construct the name of the image file.
    const std::string filename = "autosave." + get_time_stamp_string() + ".exr";

    // Construct the path to the image file.
    const std::string file_path = (bf::path(directory) / filename).string();

    // Return the path to the image file.
    if (output_path)
        *output_path = duplicate_string(file_path.c_str());

    ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
    return write_image(*this, file_path.c_str(), *impl->m_image, image_attributes);
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
        const Vector2i DefaultTileSize(32, 32);
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

        impl->m_filter_name = m_params.get_optional<std::string>("filter", DefaultFilterName);
        impl->m_filter_radius = m_params.get_optional<float>("filter_size", 1.5f);

        if (impl->m_filter_name == "box")
        {
            impl->m_filter_sampling_table.reset(
                new FilterSamplingTable(BoxFilter1<float>(impl->m_filter_radius)));
        }
        else if (impl->m_filter_name == "triangle")
        {
            impl->m_filter_sampling_table.reset(
                new FilterSamplingTable(TriangleFilter1<float>(impl->m_filter_radius)));
        }
        else if (impl->m_filter_name == "gaussian")
        {
            impl->m_filter_sampling_table.reset(
                new FilterSamplingTable(GaussianFilter1<float>(impl->m_filter_radius, 8.0f)));
        }
        else if (impl->m_filter_name == "blackman-harris")
        {
            impl->m_filter_sampling_table.reset(
                new FilterSamplingTable(BlackmanHarrisFilter1<float>(impl->m_filter_radius)));
        }
        else
        {
            RENDERER_LOG_ERROR(
                "invalid value \"%s\" for parameter \"%s\", using default value \"%s\".",
                impl->m_filter_name.c_str(),
                "filter",
                DefaultFilterName);
            impl->m_filter_name = DefaultFilterName;
            impl->m_filter_sampling_table.reset(
                new FilterSamplingTable(BlackmanHarrisFilter1<float>(impl->m_filter_radius)));
        }
    }

    // Retrieve crop window parameter.
    const AABB2u default_crop_window(
        Vector2u(0, 0),
        Vector2u(impl->m_frame_width - 1, impl->m_frame_height - 1));
    impl->m_crop_window = m_params.get_optional<AABB2u>("crop_window", default_crop_window);

    // Retrieve dithering parameter.
    impl->m_enable_dithering = m_params.get_optional<bool>("enable_dithering", true);

    // Retrieve noise seed.
    impl->m_noise_seed = m_params.get_optional<std::uint32_t>("noise_seed", 0);

    // Retrieve denoiser parameters.
    {
        const std::string denoise_mode = m_params.get_optional<std::string>("denoiser", "off");

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

    // Retrieve checkpoint parameters.
    {
        // Create option.
        impl->m_checkpoint_create = m_params.get_optional<bool>("checkpoint_create", false);
        impl->m_checkpoint_create_path = "";

        // Check if the checkpoint create path is valid.
        if (impl->m_checkpoint_create)
        {
            std::string path = m_params.get_optional<std::string>("checkpoint_create_path", "");
            const bool use_default_path = path.empty();
            if (use_default_path)
                path = m_params.get_required<std::string>("output_path");

            bf::path bf_path(path.c_str());

            const std::string extension = lower_case(bf_path.extension().string());

            if (extension != ".exr")
            {
                // Only .exr files are allowed.
                RENDERER_LOG_ERROR("checkpoint file must be an \".exr\" file, disabling checkpoint creation.");
                impl->m_checkpoint_create = false;
            }
            else
            {
                // Add ".checkpoint" in the filename.
                if (use_default_path && !ends_with(path, ".checkpoint.exr"))
                {
                    const bf::path directory = bf_path.parent_path();
                    const std::string base_file_name = bf_path.stem().string();
                    path = (directory / (base_file_name + ".checkpoint.exr")).string();
                }

                impl->m_checkpoint_create_path = path;
            }
        }

        // Resume option.
        impl->m_checkpoint_resume = m_params.get_optional<bool>("checkpoint_resume", false);
        impl->m_checkpoint_resume_path = "";

        // Check if the checkpoint resume path is valid.
        if (impl->m_checkpoint_resume)
        {
            std::string path = m_params.get_optional<std::string>("checkpoint_resume_path", "");
            const bool use_default_path = path == "";
            if (use_default_path)
                path = m_params.get_required<std::string>("output_path");

            bf::path bf_path(path.c_str());

            const std::string extension = lower_case(bf_path.extension().string());

            if (extension != ".exr")
            {
                // Only .exr files are allowed.
                RENDERER_LOG_ERROR("checkpoint file must be an \".exr\" file, disabling checkpoint resuming.");
                impl->m_checkpoint_resume = false;
            }
            else
            {
                // Add ".checkpoint" in the filename.
                if (use_default_path && !ends_with(path, ".checkpoint.exr"))
                {
                    const bf::path directory = bf_path.parent_path();
                    const std::string base_file_name = bf_path.stem().string();
                    path = (directory / (base_file_name + ".checkpoint.exr")).string();
                }

                impl->m_checkpoint_resume_path = path;
            }
        }
    }

    // Retrieve reference image path parameters.
    impl->m_ref_image_path = m_params.get_optional<std::string>("reference_image", "");
}

AOVContainer& Frame::internal_aovs() const
{
    return impl->m_internal_aovs;
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
            .insert("name", "noise_seed")
            .insert("label", "Noise Seed")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "enable_dithering")
            .insert("label", "Dithering")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "true"));

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
            .insert("name", "skip_denoised")
            .insert("label", "Skip Denoised Pixels")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "true")
            .insert("visible_if",
                Dictionary()
                    .insert("denoiser", "on")));

    metadata.push_back(
        Dictionary()
            .insert("name", "random_pixel_order")
            .insert("label", "Random Pixel Order")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "true")
            .insert("visible_if",
                Dictionary()
                    .insert("denoiser", "on")));

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
            .insert("label", "Mark Invalid Pixels")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "false")
            .insert("visible_if",
                Dictionary()
                    .insert("denoiser", "on")));

    return metadata;
}

auto_release_ptr<Frame> FrameFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<Frame>(
            new Frame(name, params, AOVContainer(), AOVContainer(), SearchPaths()));
}

auto_release_ptr<Frame> FrameFactory::create(
    const char*         name,
    const ParamArray&   params,
    const AOVContainer& aovs,
    const AOVContainer& lpe_aovs)
{
    return
        auto_release_ptr<Frame>(
            new Frame(name, params, aovs, lpe_aovs, SearchPaths()));
}

auto_release_ptr<Frame> FrameFactory::create(
    const char*         name,
    const ParamArray&   params,
    const AOVContainer& aovs,
    const AOVContainer& lpe_aovs,
    const SearchPaths&  search_paths)
{
    return
        auto_release_ptr<Frame>(
            new Frame(name, params, aovs, lpe_aovs, search_paths));
}

}   // namespace renderer
