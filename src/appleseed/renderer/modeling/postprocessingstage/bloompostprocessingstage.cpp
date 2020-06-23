
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
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cstddef>
#include <cmath>
#include <memory>
#include <vector>

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

        static void init_blur_pyramids(
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

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            if (m_iterations == 0 || m_intensity == 0.0f && !m_debug_blur)
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
            const std::size_t iterations = clamp<std::size_t>(m_iterations, 1, static_cast<std::size_t>(max_iterations));

            //
            // Prefilter pass.
            //

            // Copy the original image but ignore its alpha channel.
            const std::size_t shuffle_table[4] = { 0, 1, 2, Pixel::SkipChannel };
            Image prefiltered_image(image, props.m_pixel_format, shuffle_table);

            // Filter out dark pixels.
            BrightPassApplier bright_pass({ m_threshold, m_soft_threshold });
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

            ResampleX2Applier downsample({ prefiltered_image, SamplingX2Mode::HALVE });
            downsample.apply_on_tiles(blur_pyramid_down[0], thread_count);

            for (std::size_t level = 1; level < iterations; ++level)
            {
                ResampleApplier downsample({ blur_pyramid_down[level - 1], SamplingMode::DOWN });
                downsample.apply_on_tiles(blur_pyramid_down[level], thread_count);
            }

            //
            // Upsample and blend pass.
            //

            blur_pyramid_up[iterations - 1].copy_from(blur_pyramid_down[iterations - 1]);

            for (std::size_t level_plus_one = iterations - 1; level_plus_one > 0; --level_plus_one)
            {
                ResampleApplier upsample({ blur_pyramid_up[level_plus_one], SamplingMode::UP });
                upsample.apply_on_tiles(blur_pyramid_up[level_plus_one - 1], thread_count);

                // Blend each upsampled buffer with the downsample buffer of the same level.
                AdditiveBlendApplier additive_blend({ blur_pyramid_down[level_plus_one - 1] });
                additive_blend.apply_on_tiles(blur_pyramid_up[level_plus_one - 1], thread_count);
            }

            //
            // Resolve pass.
            //

            Image bloom_target(prefiltered_image.properties());

            ResampleX2Applier upsample({ blur_pyramid_up[0], SamplingX2Mode::DOUBLE });
            upsample.apply_on_tiles(bloom_target, thread_count);

            AdditiveBlendApplier additive_blend(
                {
                    bloom_target,
                    m_debug_blur ? 1.0f : m_intensity,
                    m_debug_blur ? 0.0f : 1.0f
                });
            additive_blend.apply_on_tiles(image, thread_count);
        }

      private:
        std::size_t     m_iterations;
        float           m_intensity;
        float           m_threshold;
        float           m_soft_threshold;
        bool            m_debug_blur;

        void execute_single_iteration(
            Image&                      image,
            const Image&                prefiltered_image,
            const CanvasProperties&     blur_target_props,
            const std::size_t           thread_count) const
        {
            // Downsample the prefiltered image.
            Image blur_target(blur_target_props);
            ResampleApplier downsample({ prefiltered_image, SamplingMode::DOWN });
            downsample.apply_on_tiles(blur_target, thread_count);

            // Upsample and blend it with the original image.
            Image bloom_target(prefiltered_image.properties());
            ResampleApplier upsample({ blur_target, SamplingMode::UP });
            upsample.apply_on_tiles(bloom_target, thread_count);

            AdditiveBlendApplier additive_blend(
                {
                    bloom_target,
                    m_debug_blur ? 1.0f : m_intensity,
                    m_debug_blur ? 0.0f : 1.0f
                });
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
