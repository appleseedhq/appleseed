
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Stephen Agyemang, The appleseedhq Organization
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
#include "texturecontrolledpixelrenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/rendering/pixelrendererbase.h"
#include "renderer/kernel/rendering/shadingresultframebuffer.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/aov/pixelsamplecountaov.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/settingsparsing.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/hash/hash.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/aabb.h"
#include "foundation/math/filtersamplingtable.h"
#include "foundation/math/population.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/statistics.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/imagebufalgo.h"
#include "OpenImageIO/typedesc.h"
#include "OpenImageIO/imagecache.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <cmath>
#include <cstdint>

using namespace foundation;
using namespace OIIO;

namespace renderer
{

namespace
{
    //
    // Texture-controlled pixel renderer.
    //

    class TextureControlledPixelRenderer
      : public PixelRendererBase
    {
      public:
        TextureControlledPixelRenderer(
            const Frame&                          frame,
            const std::shared_ptr<const ImageBuf> texture,
            ISampleRendererFactory*               factory,
            const ParamArray&                     params,
            const size_t                          thread_index)
          : m_params(params)
          , m_texture(texture)
          , m_sample_renderer(factory->create(thread_index))
          , m_min_sample_count(m_params.m_min_samples)
          , m_max_sample_count(m_params.m_max_samples)
        {
            const size_t sample_aov_index = frame.aovs().get_index("pixel_sample_count");

            // If the sample count AOV is enabled, we need to reset its normalization
            // range in case an adaptive render was done previously.
            if (sample_aov_index != ~size_t(0))
            {
                PixelSampleCountAOV* sample_aov =
                    static_cast<PixelSampleCountAOV*>(
                        frame.aovs().get_by_index(sample_aov_index));

                sample_aov->set_normalization_range(0, 0);
            }
        }

        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
            RENDERER_LOG_INFO(
                "texture-controlled pixel renderer settings:\n"
                "  min samples                   %s\n"
                "  max samples                   %s\n"
                "  force anti-aliasing           %s\n",
                pretty_uint(m_params.m_min_samples).c_str(),
                pretty_uint(m_params.m_max_samples).c_str(),
                m_params.m_force_aa ? "on" : "off");

            m_sample_renderer->print_settings();
        }

        void render_pixel(
            const Frame&                frame,
            Tile&                       tile,
            TileStack&                  aov_tiles,
            const AABB2i&               tile_bbox,
            const std::uint32_t         pass_hash,
            const Vector2i&             pi,
            const Vector2i&             pt,
            AOVAccumulatorContainer&    aov_accumulators,
            ShadingResultFrameBuffer&   framebuffer) override
        {
            const size_t aov_count = frame.aov_images().size();

            on_pixel_begin(frame, pi, pt, tile_bbox, aov_accumulators);

            // Create a sampling context.
            const size_t frame_width = frame.image().properties().m_canvas_width;
            const size_t pixel_index = pi.y * frame_width + pi.x;
            const size_t instance = hash_uint32(static_cast<std::uint32_t>(pass_hash + pixel_index));
            SamplingContext::RNGType rng(pass_hash, instance);
            SamplingContext sampling_context(
                rng,
                m_params.m_sampling_mode,
                2,                          // number of dimensions
                0,                          // number of samples -- unknown
                instance);                  // initial instance number

            const ROI roi(
                pi.x,
                pi.x + 1,
                pi.y,
                pi.y + 1,
                0,
                1,
                0,
                1);

            float pixel_red_channel;
            m_texture->get_pixels(roi, TypeDesc::TypeFloat, &pixel_red_channel);

            const size_t sample_count =
                round<size_t>(
                    foundation::lerp(       // qualifier needed
                        static_cast<float>(m_min_sample_count),
                        static_cast<float>(m_max_sample_count),
                        pixel_red_channel));

            for (size_t i = 0; i < sample_count; ++i)
            {
                // Generate a uniform sample in [0,1)^2.
                const Vector2f s =
                    m_max_sample_count > 1 || m_params.m_force_aa
                        ? sampling_context.next2<Vector2f>()
                        : Vector2f(0.5f);

                // Sample the pixel filter.
                const auto& filter_table = frame.get_filter_sampling_table();
                const Vector2d pf(
                    static_cast<double>(filter_table.sample(s[0]) + 0.5f),
                    static_cast<double>(filter_table.sample(s[1]) + 0.5f));

                // Compute the sample position in NDC.
                const Vector2d sample_position = frame.get_sample_position(pi.x + pf.x, pi.y + pf.y);

                // Create a pixel context that identifies the pixel and sample currently being rendered.
                const PixelContext pixel_context(pi, sample_position);

                // Render the sample.
                ShadingResult shading_result(aov_count);
                SamplingContext child_sampling_context(sampling_context);
                m_sample_renderer->render_sample(
                    child_sampling_context,
                    pixel_context,
                    sample_position,
                    aov_accumulators,
                    shading_result);

                // Update sampling statistics.
                m_total_sampling_dim.insert(child_sampling_context.get_total_dimension());

                // Merge the sample into the framebuffer.
                if (shading_result.is_valid())
                    framebuffer.add(Vector2u(pt), shading_result);
                else
                    signal_invalid_sample();
            }

            on_pixel_end(frame, pi, pt, tile_bbox, aov_accumulators);
        }

        StatisticsVector get_statistics() const override
        {
            Statistics stats;
            stats.insert("max sampling dimension", m_total_sampling_dim);

            StatisticsVector vec;
            vec.insert("generic sample generator statistics", stats);
            vec.merge(m_sample_renderer->get_statistics());

            return vec;
        }

        size_t get_max_samples_per_pixel() const override
        {
            return m_max_sample_count;
        }

      private:
        struct Parameters
        {
            const SamplingContext::Mode     m_sampling_mode;
            const size_t                    m_min_samples;
            const size_t                    m_max_samples;
            const bool                      m_force_aa;

            explicit Parameters(const ParamArray& params)
              : m_sampling_mode(get_sampling_context_mode(params))
              , m_min_samples(params.get_required<size_t>("min_samples", 0))
              , m_max_samples(params.get_required<size_t>("max_samples", 64))
              , m_force_aa(params.get_optional<bool>("force_antialiasing", false))
            {
            }
        };

        const Parameters                       m_params;
        const std::shared_ptr<const ImageBuf>  m_texture;
        auto_release_ptr<ISampleRenderer>      m_sample_renderer;
        const size_t                           m_min_sample_count;
        const size_t                           m_max_sample_count;
        Population<std::uint64_t>              m_total_sampling_dim;
    };
}


//
// TextureControlledPixelRendererFactory class implementation.
//

Dictionary TextureControlledPixelRendererFactory::get_params_metadata()
{
    Dictionary metadata;

    metadata.dictionaries().insert(
        "min_samples",
        Dictionary()
            .insert("type", "int")
            .insert("default", "0")
            .insert("label", "Minimum Samples")
            .insert("help", "Minimum number of anti-aliasing samples"));

    metadata.dictionaries().insert(
        "max_samples",
        Dictionary()
            .insert("type", "int")
            .insert("default", "64")
            .insert("label", "Maximum Samples")
            .insert("help", "Maximum number of anti-aliasing samples"));

    metadata.dictionaries().insert(
        "file_path",
        Dictionary()
            .insert("type", "text")
            .insert("default", "")
            .insert("label", "File Path")
            .insert("help", "Path to a black and white texture used to determine the sample count"));

    metadata.dictionaries().insert(
        "force_antialiasing",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "true")
            .insert("label", "Force Anti-Aliasing")
            .insert(
                "help",
                "When using 1 sample/pixel and Force Anti-Aliasing is disabled, samples are placed at the center of pixels"));

    return metadata;
}

TextureControlledPixelRendererFactory::TextureControlledPixelRendererFactory(
    const Frame&                          frame,
    ISampleRendererFactory*               factory,
    const ParamArray&                     params)
  : m_frame(frame)
  , m_factory(factory)
  , m_params(params)
{
}

bool TextureControlledPixelRendererFactory::load_texture(const std::string& texture_path)
{
    // Create a new dedicated ImageCache instead of using the global ImageCache.
    // This is to prevent errors during ImageBuf access if the file behind tex_path was overwritten on disk.
    ImageCache* image_cache = ImageCache::create();
    std::unique_ptr<ImageBuf> texture(new ImageBuf(texture_path, 0, 0, image_cache));

    // Force the read to immediately do the entire disk I/O for the file.
    // We can then safely destroy the cache since the entire image data has been read into the ImageBuf.
    const bool read_successful = texture->read(0, 0, true);
    ImageCache::destroy(image_cache);

    if (!read_successful)
        return false;

    const CanvasProperties& frame_props = m_frame.image().properties();
    const ROI roi(
        0,                                                  // xbegin
        static_cast<int>(frame_props.m_canvas_width),       // xend
        0,                                                  // ybegin
        static_cast<int>(frame_props.m_canvas_height),      // yend
        0,                                                  // zbegin
        1,                                                  // zend
        0,                                                  // chbegin
        texture->nchannels());                              // chend

    m_texture = std::make_shared<ImageBuf>();
    return ImageBufAlgo::resize(*m_texture.get(), *texture.get(), "", 0.0f, roi);
}

void TextureControlledPixelRendererFactory::release()
{
    delete this;
}

IPixelRenderer* TextureControlledPixelRendererFactory::create(
    const size_t                thread_index)
{
    assert(m_texture);

    return new TextureControlledPixelRenderer(m_frame, m_texture, m_factory, m_params, thread_index);
}

}   // namespace renderer
