
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
#include "renderer/modeling/postprocessingstage/effect/downsampleapplier.h"
#include "renderer/modeling/postprocessingstage/effect/upsampleapplier.h"
#include "renderer/modeling/postprocessingstage/effect/downsamplex2applier.h"
#include "renderer/modeling/postprocessingstage/effect/upsamplex2applier.h"
#include "renderer/modeling/postprocessingstage/postprocessingstage.h"
#include "renderer/utility/rgbcolorsampling.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cstddef>
#include <cmath>
#include <memory>
#include <vector>

#include "Instrumentor.h"

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Bloom post-processing stage.
    //

    const char* Model = "bloom_post_processing_stage";

    static constexpr std::size_t DefaultIterations = 4;

    static constexpr float DefaultIntensity = 0.5f;

    static constexpr float DefaultThreshold = 0.8f;

    static constexpr float DefaultSoftThreshold = 0.5f;

    static constexpr bool DefaultDebugBlur = false;

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

            m_soft_threshold = m_params.get_optional("soft_threshold", DefaultSoftThreshold, context);

            m_debug_blur = m_params.get_optional("debug_blur", DefaultDebugBlur, context);

            return true;
        }

        //
        // Bloom post-processing effect execution.
        //

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
const std::size_t thread_count_ = 1; // thread_count;
PROFILE_FUNCTION();
            if (m_intensity == 0.0f && !m_debug_blur)
                return;

            Image& image = frame.image();
            const CanvasProperties& props = image.properties();

            // Determine the dimensions of the largest temporary buffer used for blurring.
            #define FAST_MODE true
            const std::size_t width = props.m_canvas_width / (FAST_MODE ? 2 : 1);
            const std::size_t height = props.m_canvas_height / (FAST_MODE ? 2 : 1);

            // Compute how many downsampling iterations we can do before a buffer has a side smaller than 2 pixels.
            const float max_iterations = std::log2(std::min(width, height) / 2.0f) + (FAST_MODE ? - 1.0f : 0.0f);
            const std::size_t iterations = clamp<std::size_t>(m_iterations, 0, static_cast<int>(max_iterations));

            if (iterations == 0)
                return;

            // Create blur buffer pyramids (note that lower levels have larger images).
            std::vector<Image> blur_pyramid_down;
            std::vector<Image> blur_pyramid_up;

            blur_pyramid_down.reserve(iterations);
            blur_pyramid_up.reserve(iterations);

            {
PROFILE_SCOPE("Create blur buffer pyramids");
                // Copy the pixel format, but ignore the alpha channel for bloom.
                assert(props.m_channel_count == 4);
                const std::size_t channel_count = 3;
                const PixelFormat pixel_format = props.m_pixel_format;

                std::size_t level_width = width;
                std::size_t level_height = height;

                // NOTE empirical value (similar to 16 and 64; better than other powers of 2)
                const std::size_t max_tile_size = 32;

                for (std::size_t level = 0; level < iterations; ++level)
                {
                    const CanvasProperties level_props(
                        level_width,
                        level_height,
                        std::min(level_width, max_tile_size),
                        std::min(level_height, max_tile_size),
                        channel_count,
                        pixel_format);

                    blur_pyramid_down.push_back(Image(level_props));
                    blur_pyramid_up.push_back(Image(level_props));

                    // Halve dimensions for the next buffer level.
                    assert(level_width >= 4 && level_height >= 4);
                    level_width /= 2;
                    level_height /= 2;
                }
            }

            //
            // Prefilter pass.
            //

            // Copy the image but ignore its alpha channel.
            const std::size_t shuffle_table[4] = { 0, 1, 2, Pixel::SkipChannel };
            Image prefiltered_image(image, props.m_pixel_format, shuffle_table);

{ PROFILE_SCOPE("Prefilter pass");
            // Filter out dark pixels.
            BrightPassApplier bright_pass({ m_threshold, m_soft_threshold });
            bright_pass.apply_on_tiles(prefiltered_image, thread_count_);
}

            //
            // Downsample pass.
            //

{ PROFILE_SCOPE("Downsample pass");
#if FAST_MODE
            DownsampleX2Applier downsample({ prefiltered_image });
#else
            DownsampleApplier downsample({ prefiltered_image });
#endif
            downsample.apply_on_tiles(blur_pyramid_down[0], thread_count_);

            for (std::size_t level = 1; level < iterations; ++level)
            {
const std::string _scope_name = "Level #" + std::to_string(level);
PROFILE_SCOPE(_scope_name.c_str());
                DownsampleApplier downsample({ blur_pyramid_down[level - 1] });
                downsample.apply_on_tiles(blur_pyramid_down[level], thread_count_);
            }
}

            //
            // Upsample and blend pass.
            //

{ PROFILE_SCOPE("Upsample and blend pass");
            blur_pyramid_up[iterations - 1].copy_from(blur_pyramid_down[iterations - 1]); // TODO optimize away

            for (std::size_t level_plus_one = iterations - 1; level_plus_one > 0; --level_plus_one)
            {
                const std::size_t level = level_plus_one - 1;
const std::string _scope_name = "Level #" + std::to_string(level);
PROFILE_SCOPE(_scope_name.c_str());

                UpsampleApplier upsample({ blur_pyramid_up[level + 1] });
                upsample.apply_on_tiles(blur_pyramid_up[level], thread_count_);

{ PROFILE_SCOPE("Blending");
                // Blend each upsampled buffer with the downsample buffer of the same level.
                AdditiveBlendApplier additive_blend({ blur_pyramid_down[level] });
                additive_blend.apply_on_tiles(blur_pyramid_up[level], thread_count_);
}
            }
}

            //
            // Resolve pass.
            //

{ PROFILE_SCOPE("Resolve pass");
            Image bloom_target(prefiltered_image.properties());

#if FAST_MODE
            UpsampleX2Applier upsample({ blur_pyramid_up[0] });
#else
            UpsampleApplier upsample({ blur_pyramid_up[0] });
#endif
            upsample.apply_on_tiles(bloom_target, thread_count_);

{ PROFILE_SCOPE("Blending (final)");
            AdditiveBlendApplier additive_blend(
                {
                    bloom_target,
                    m_debug_blur ? 1.0f : m_intensity,
                    m_debug_blur ? 0.0f : 1.0f
                });
            additive_blend.apply_on_tiles(image, thread_count_);
}
}
        }

      private:
        std::size_t     m_iterations;
        float           m_intensity;
        float           m_threshold;
        float           m_soft_threshold;
        bool            m_debug_blur;
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
            .insert("label", "Iterations")
            .insert("type", "int")
            .insert("min",
                    Dictionary()
                        .insert("value", "1")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "5")
                        .insert("type", "hard"))
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
                        .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "0.5"));

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
                        .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "0.8"));

    metadata.push_back(
        Dictionary()
            .insert("name", "soft_threshold")
            .insert("label", "Soft Threshold")
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
            .insert("type", "bool")
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
