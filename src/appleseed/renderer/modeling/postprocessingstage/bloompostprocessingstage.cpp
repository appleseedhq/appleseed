
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Tiago Chaves, The appleseedhq Organization
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
#include "bloompostprocessingstage.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/postprocessingstage/effect/additiveblendapplier.h"
#include "renderer/modeling/postprocessingstage/effect/brightpassapplier.h"
#include "renderer/modeling/postprocessingstage/effect/resampleapplier.h"
#include "renderer/modeling/postprocessingstage/effect/resamplex2applier.h"
#include "renderer/modeling/postprocessingstage/postprocessingstage.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cstddef>
#include <cmath>
#include <memory>
#include <vector>

using namespace foundation;

namespace renderer
{

//
// Initialize the two image pyramids used for blurring (through down/up sampling).
//
// Both have level_count images, with pixel format and channel count given by max_level_props,
// that also specifies the dimensions of their first elements (which get subsequently halved).
//

void init_blur_pyramids(
    std::vector<Image>&         blur_pyramid_down,
    std::vector<Image>&         blur_pyramid_up,
    const std::size_t           level_count,
    const CanvasProperties&     max_level_props,
    const std::size_t           max_tile_size)
{
    blur_pyramid_down.reserve(level_count);
    blur_pyramid_up.reserve(level_count);

    // Note that lower levels have larger images.
    std::size_t level_width = max_level_props.m_canvas_width;
    std::size_t level_height = max_level_props.m_canvas_height;

    for (std::size_t level = 0; level < level_count; ++level)
    {
        const CanvasProperties level_props(
            level_width,
            level_height,
            std::min(level_width, max_tile_size),
            std::min(level_height, max_tile_size),
            max_level_props.m_channel_count,
            max_level_props.m_pixel_format);

        blur_pyramid_down.push_back(Image(level_props));
        blur_pyramid_up.push_back(Image(level_props));

        // Halve dimensions for the next buffer level.
        assert(level_width >= 4 && level_height >= 4);
        level_width /= 2;
        level_height /= 2;
    }
}

namespace
{
    //
    // Bloom post-processing stage.
    //

    const char* Model = "bloom_post_processing_stage";

    constexpr std::size_t DefaultIterations = 4;    // number of downsampling iterations used for blurring, which controls the spread radius
    constexpr float DefaultIntensity = 0.1f;        // strength of the Bloom effect (i.e. blending factor with the original image)
    constexpr float DefaultThreshold = 1.0f;        // filters out pixels under this level of brightness
    constexpr float DefaultSoftKnee = 0.5f;         // makes the trasition between under/over-threshold gradual (0 = hard, 1 = soft)
    constexpr bool DefaultDebugBlur = false;        // toggle to only show the Bloom target, instead of compositing it with the final render

    class BloomPostProcessingStage
      : public PostProcessingStage
    {
      public:
        BloomPostProcessingStage(
            const char*             name,
            const ParamArray&       params)
          : PostProcessingStage(name, params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            const OnFrameBeginMessageContext context("post-processing stage", this);

            m_iterations = m_params.get_optional("iterations", DefaultIterations, context);
            m_intensity = m_params.get_optional("intensity", DefaultIntensity, context);
            m_threshold = m_params.get_optional("threshold", DefaultThreshold, context);
            m_soft_knee = m_params.get_optional("soft_knee", DefaultSoftKnee, context);
            m_debug_blur = m_params.get_optional("debug_blur", DefaultDebugBlur, context);

            return true;
        }

        //
        // Bloom post-processing effect execution.
        //

        //
        // Basically, bright pixels of the source image are extracted, blurred, then blended back into it. Thus,
        // creating the effect of light extending from luminous areas. The algorithm consists of four main steps:
        //
        //   1. Prefiltering:
        //       Filter out dark pixels from the image, so that only bright areas are blurred.
        //   2. Downsampling:
        //       Iteratively downsample the original image, leading to some blurring per step.
        //   3. Upsampling and blending:
        //       Iteratively upsample the last downsampling, thus increasing the resulting blur,
        //       and blend the colors from previously downsampled images of the same size, so that
        //       the intensity of bright areas stays high at their centers, and radially falls off.
        //   4. Compositing:
        //       Blend the final blurred image with the original one, weighted by an intensity value.
        //
        //                   "initial"                                                                      "final"
        //                     image  ───────────────────────────────────────────────────────────────────> + image
        //                       |                                                                 (blend) Λ
        //                 (bright pass)                                                                   |
        //                       v                                                                         |
        //               prefiltered_image                                                                 |
        //                       |                                                                         |
        //                 (resample x2)                                                                   |
        //                       v                                                                         |
        //  blur_pyramid_down: @[0] ─(resample)─> @[1] ─(resample)─> ... ─(resample)─> @[iterations - 1]   |
        //                       |                  |                 |                        |           |
        //                    (blend)            (blend)           (blend)                  (copy)         |
        //                       +                  +                 +                        =           |
        //  blur_pyramid_up:   @[0] <─(resample)─ @[1] <─(resample)─ ... <─(resample)─ @[iterations - 1]   |
        //                       |                                                                         |
        //                 (resample x2)                                                                   |
        //                       v                                                                         |
        //                 bloom_target                                                                    |
        //                       |                                                                         |
        //                       └─────────────────────────────────────────────────────────────────────────┘
        //
        // References:
        //
        //   Real-Time 3D Scene Post-processing
        //   http://developer.amd.com/wordpress/media/2012/10/Oat-ScenePostprocessing.pdf
        //
        //   Bloom Advanced Rendering Catlike Coding Tutorial
        //   https://catlikecoding.com/unity/tutorials/advanced-rendering/bloom/
        //
        //   KinoBloom effect for Unity
        //   https://github.com/keijiro/KinoBloom/
        //
        //   EEVEE Bloom
        //   https://github.com/blender/blender/tree/master/source/blender/draw/engines/eevee
        //   https://docs.blender.org/manual/en/latest/render/eevee/render_settings/bloom.html
        //
        //   An investigation of fast real-time GPU-based image blur algorithms
        //   https://software.intel.com/content/www/us/en/develop/blogs/an-investigation-of-fast-real-time-gpu-based-image-blur-algorithms.html
        //
        //   Bandwidth-Efficient Rendering
        //   https://community.arm.com/cfs-file/__key/communityserver-blogs-components-weblogfiles/00-00-00-20-66/siggraph2015_2D00_mmg_2D00_marius_2D00_notes.pdf
        //

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            if (m_iterations == 0 || (m_intensity == 0.0f && !m_debug_blur))
                return;

            Image& image = frame.image();
            const CanvasProperties& props = image.properties();

            // Determine the dimensions of the largest temporary buffer used for blurring.
            const std::size_t max_pyramid_width = props.m_canvas_width / 2;
            const std::size_t max_pyramid_height = props.m_canvas_height / 2;
            const std::size_t min_pyramid_dimension = std::min(max_pyramid_width, max_pyramid_height);

            if (min_pyramid_dimension < 4)
                return;

            // Copy the pixel format, but ignore the alpha channel.
            assert(props.m_channel_count == 4);
            const std::size_t max_tile_size = 32;
            const CanvasProperties blur_props(
                max_pyramid_width,
                max_pyramid_height,
                std::min(max_pyramid_width, max_tile_size),
                std::min(max_pyramid_height, max_tile_size),
                3,
                props.m_pixel_format);

            // Compute how many downsampling iterations we can do before a buffer has a side smaller than 2 pixels.
            const float max_iterations = std::log2(min_pyramid_dimension / 2.0f);
            const std::size_t iterations = clamp<std::size_t>(m_iterations, 1, truncate<std::size_t>(max_iterations));

            if (iterations < m_iterations)
            {
                RENDERER_LOG_WARNING(
                    "post-processing stage \"%s\":\n"
                    "  the frame is too small for " FMT_SIZE_T " iterations\n"
                    "  using " FMT_SIZE_T " iterations instead",
                    get_path().c_str(),
                    m_iterations,
                    iterations);
            }

            //
            // Prefilter pass.
            //

            // Copy the original image but ignore its alpha channel.
            const std::size_t shuffle_table[4] = { 0, 1, 2, Pixel::SkipChannel };
            Image prefiltered_image(image, props.m_pixel_format, shuffle_table);

            // Filter out dark pixels.
            const BrightPassApplier bright_pass(m_threshold, m_soft_knee);
            bright_pass.apply_on_tiles(prefiltered_image, thread_count);

            //
            // Create and initialize the blur buffer pyramids.
            //

            if (iterations == 1)
            {
                // If we have more than one scaling iteration, we can get away with
                // a quicker resampling method for the largest blur_pyramid_down and
                // blur_pyramid_up images (which greatly reduces the bloom stage time).
                //
                // However, with a single iteration this leads to blocky artifacts,
                // so we use the more computationally expensive resampling method here.
                execute_single_iteration(image, prefiltered_image, blur_props, thread_count);
                return;
            }

            std::vector<Image> blur_pyramid_down;
            std::vector<Image> blur_pyramid_up;
            init_blur_pyramids(
                blur_pyramid_down,
                blur_pyramid_up,
                iterations,
                blur_props,
                max_tile_size);

            //
            // Downsample pass.
            //

            const ResampleX2Applier downsample(prefiltered_image, ResampleX2Applier::SamplingMode::HALVE);
            downsample.apply_on_tiles(blur_pyramid_down[0], thread_count);

            for (std::size_t level = 1; level < iterations; ++level)
            {
                const ResampleApplier downsample(blur_pyramid_down[level - 1], ResampleApplier::SamplingMode::DOWN);
                downsample.apply_on_tiles(blur_pyramid_down[level], thread_count);
            }

            //
            // Upsample and blend pass.
            //

            blur_pyramid_up[iterations - 1].copy_from(blur_pyramid_down[iterations - 1]);

            for (std::size_t level_plus_one = iterations - 1; level_plus_one > 0; --level_plus_one)
            {
                const ResampleApplier upsample(blur_pyramid_up[level_plus_one], ResampleApplier::SamplingMode::UP);
                upsample.apply_on_tiles(blur_pyramid_up[level_plus_one - 1], thread_count);

                // Blend each upsampled buffer with the downsample buffer of the same level.
                const AdditiveBlendApplier additive_blend(blur_pyramid_down[level_plus_one - 1]);
                additive_blend.apply_on_tiles(blur_pyramid_up[level_plus_one - 1], thread_count);
            }

            //
            // Composite pass.
            //

            Image bloom_target(prefiltered_image.properties());

            const ResampleX2Applier upsample(blur_pyramid_up[0], ResampleX2Applier::SamplingMode::DOUBLE);
            upsample.apply_on_tiles(bloom_target, thread_count);

            const AdditiveBlendApplier additive_blend(
                bloom_target,
                m_intensity,
                m_debug_blur ? 0.0f : 1.0f);
            additive_blend.apply_on_tiles(image, thread_count);
        }

      private:
        std::size_t     m_iterations;
        float           m_intensity;
        float           m_threshold;
        float           m_soft_knee;
        bool            m_debug_blur;

        void execute_single_iteration(
            Image&                      image,
            const Image&                prefiltered_image,
            const CanvasProperties&     blur_target_props,
            const std::size_t           thread_count) const
        {
            // Downsample the prefiltered image.
            Image blur_target(blur_target_props);
            const ResampleApplier downsample(prefiltered_image, ResampleApplier::SamplingMode::DOWN);
            downsample.apply_on_tiles(blur_target, thread_count);

            // Upsample and blend it with the original image.
            Image bloom_target(prefiltered_image.properties());
            const ResampleApplier upsample(blur_target, ResampleApplier::SamplingMode::UP);
            upsample.apply_on_tiles(bloom_target, thread_count);

            const AdditiveBlendApplier additive_blend(
                bloom_target,
                m_intensity,
                m_debug_blur ? 0.0f : 1.0f);
            additive_blend.apply_on_tiles(image, thread_count);
        }
    };
}


//
// BloomPostProcessingStageFactory class implementation.
//

void BloomPostProcessingStageFactory::release()
{
    delete this;
}

const char* BloomPostProcessingStageFactory::get_model() const
{
    return Model;
}

Dictionary BloomPostProcessingStageFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Bloom");
}

DictionaryArray BloomPostProcessingStageFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_input_metadata(metadata);

    metadata.push_back(
        Dictionary()
            .insert("name", "iterations")
            .insert("label", "Radius")
            .insert("type", "integer")
            .insert("min",
                    Dictionary()
                        .insert("value", "1")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "5")
                        .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "4"));

    metadata.push_back(
        Dictionary()
            .insert("name", "intensity")
            .insert("label", "Intensity")
            .insert("type", "numeric")
            .insert("min",
                    Dictionary()
                        .insert("value", "0.0")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "1.0")
                        .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0.1"));

    metadata.push_back(
        Dictionary()
            .insert("name", "threshold")
            .insert("label", "Threshold")
            .insert("type", "numeric")
            .insert("min",
                    Dictionary()
                        .insert("value", "0.0")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "10.0")
                        .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "soft_knee")
            .insert("label", "Soft Knee")
            .insert("type", "numeric")
            .insert("min",
                    Dictionary()
                        .insert("value", "0.0")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "1.0")
                        .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "debug_blur")
            .insert("label", "Debug Blur")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "false"));

    return metadata;
}

auto_release_ptr<PostProcessingStage> BloomPostProcessingStageFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<PostProcessingStage>(
        new BloomPostProcessingStage(name, params));
}

}   // namespace renderer
