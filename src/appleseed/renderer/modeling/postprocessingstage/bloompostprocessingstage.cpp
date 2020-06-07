
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

    static constexpr std::size_t DefaultIterations = 5;
    static constexpr float DefaultIntensity = 0.5f;
    static constexpr float DefaultThreshold = 0.8f;
    static constexpr float DefaultSoftThreshold = 0.5f;

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

            m_fast_mode = m_params.get_optional("fast_mode", false, context);
            m_debug_blur = m_params.get_optional("debug_blur", false, context);
            m_debug_threshold = m_params.get_optional("debug_threshold", false, context);

            return true;
        }

        //
        // Image up/down sampling with bilinear filtering.
        //

        static void bilinear_filter_in_place(const Image& src_image, Image& dst_image)
        {
            const CanvasProperties& src_props = src_image.properties();
            const CanvasProperties& dst_props = dst_image.properties();

            const std::size_t src_width = src_props.m_canvas_width;
            const std::size_t dst_width = dst_props.m_canvas_width;
            const std::size_t src_height = src_props.m_canvas_height;
            const std::size_t dst_height = dst_props.m_canvas_height;

            assert(src_props.m_channel_count == 3);
            assert(dst_props.m_channel_count == 3);

            // Perform bilinear interpolation for up/down sampling.
            for (std::size_t y = 0; y < dst_height; ++y)
            {
                for (std::size_t x = 0; x < dst_width; ++x)
                {
                    const float fx = (static_cast<float>(x) / (dst_width - 1)) * (src_width - 1);
                    const float fy = (static_cast<float>(y) / (dst_height - 1)) * (src_height - 1);

                    const std::size_t x0 = truncate<std::size_t>(fx);
                    const std::size_t y0 = truncate<std::size_t>(fy);
                    const std::size_t x1 = std::min<std::size_t>(x0 + 1, src_width - 1);
                    const std::size_t y1 = std::min<std::size_t>(y0 + 1, src_height - 1);

                    const Color3f result = blerp(src_image, x0, y0, x1, y1, fx, fy); // bilinear interpolation

                    dst_image.set_pixel(x, y, result);
                }
            }
        }

        static Image bilinear_filtered(const Image& image, const float scaling_factor)
        {
            if (feq(scaling_factor, 1.0f))
                return Image(image);

            const CanvasProperties& props = image.properties();

            CanvasProperties scaled_props(
                static_cast<std::size_t>(scaling_factor * props.m_canvas_width),
                static_cast<std::size_t>(scaling_factor * props.m_canvas_height),
                static_cast<std::size_t>(scaling_factor * props.m_tile_width),
                static_cast<std::size_t>(scaling_factor * props.m_tile_height),
                props.m_channel_count,
                props.m_pixel_format);

            Image scaled_image(scaled_props);
            bilinear_filter_in_place(image, scaled_image);

            return scaled_image;
        }

        //
        // Image bright pass prefiltering.
        //

        static Image prefiltered(const Image& image, const float threshold, const float soft_threshold)
        {
            const CanvasProperties& props = image.properties();
            assert(props.m_channel_count == 4);

            Image prefiltered_image(
                props.m_canvas_width,
                props.m_canvas_height,
                props.m_tile_width,
                props.m_tile_height,
                3, // alpha is ignored
                props.m_pixel_format);
            prefiltered_image.clear(Color3f(0.0f));

            const float eps = default_eps<float>(); // used to avoid divisions by zero
            const float knee = threshold * soft_threshold;

            for (std::size_t y = 0; y < props.m_canvas_height; ++y)
            {
                for (std::size_t x = 0; x < props.m_canvas_width; ++x)
                {
                    Color4f color;
                    image.get_pixel(x, y, color);

                    const float brightness = max_value(color.rgb());
                    float contribution = (brightness - threshold);

                    if (knee > 0.0f)
                    {
                        float soft = contribution + knee;
                        soft = clamp(soft, 0.0f, 2.0f * knee);
                        soft = soft * soft * safe_rcp<float>(4.0f * knee, eps);
                        contribution = std::max(soft, contribution);
                    }

                    if (contribution > 0.0f)
                    {
                        color.rgb() *= contribution * safe_rcp<float>(brightness, eps);
                        prefiltered_image.set_pixel(x, y, color.rgb());
                    }
                }
            }

            return prefiltered_image;
        }

        //
        // Kawase bloom.
        //

        static void kawase_blur(const Image& src_image, Image& dst_image, const std::size_t offset)
        {
            const CanvasProperties& src_props = src_image.properties();
            const CanvasProperties& dst_props = dst_image.properties();

            assert(src_props.m_channel_count == 3);
            assert(dst_props.m_channel_count == 3);
            assert(src_props.m_canvas_width == dst_props.m_canvas_width);
            assert(src_props.m_canvas_height == dst_props.m_canvas_height);

            for (std::size_t y = 0; y < src_props.m_canvas_height; ++y)
            {
                for (std::size_t x = 0; x < src_props.m_canvas_width; ++x)
                {
                    Color3f result = kawase_sample(src_image, x, y, offset);

                    Color3f color;
                    dst_image.get_pixel(x, y, color);
                    result += color; // additive blend

                    dst_image.set_pixel(x, y, result);
                }
            }
        }

        static void execute_kawase_bloom(
            const CanvasProperties& props,
            Image&                  image,
            // FIXME update names/params after testing
            const std::size_t       m_iterations,
            const float             m_intensity,
            const float             m_threshold,
            const float             m_soft_threshold,
            const bool              m_fast_mode,
            const bool              m_debug_blur,
            const bool              m_debug_threshold)
        {
            // Set the offset values used for sampling in Kawase blur.
            const std::vector<std::size_t> iteration_offset = { 0, 1, 2, 2, 3 };
            assert(iteration_offset.size() <= m_iterations);

            // Copy the image to temporary render targets used for blurring.
            Image bloom_blur_1 = bilinear_filtered(
                prefiltered(image, m_threshold, m_soft_threshold),
                m_fast_mode ? 0.5f : 1.0f);
            Image bloom_blur_2 = Image(bloom_blur_1);
            Image& bloom_blur = m_iterations % 2 == 1 ? bloom_blur_2 : bloom_blur_1; // last blur target

            // Iteratively blur the image, "ping-ponging" between two temporaries.
            for (std::size_t i = 0; i < m_iterations; ++i)
            {
                if (i % 2 == 0)
                    kawase_blur(bloom_blur_1, bloom_blur_2, iteration_offset[i]);
                else
                    kawase_blur(bloom_blur_2, bloom_blur_1, iteration_offset[i]);
            }

            // Blend colors from the original and the blurred image to achieve bloom.
            const Image &bloom_target = m_fast_mode ? bilinear_filtered(bloom_blur, 2.0f) : bloom_blur;

            for (std::size_t y = 0; y < props.m_canvas_height; ++y)
            {
                for (std::size_t x = 0; x < props.m_canvas_width; ++x)
                {
                    Color4f color;
                    image.get_pixel(x, y, color);

                    Color3f bloom_color;
                    bloom_target.get_pixel(x, y, bloom_color);

                    if (m_debug_blur)
                        color.rgb() = bloom_color;
                    else
                        color.rgb() += m_intensity * bloom_color; // additive blend (weighted by intensity)
                    image.set_pixel(x, y, color);
                }
            }
        }

        //
        // Kino bloom.
        //

        static void execute_kino_bloom(
            const CanvasProperties& props,
            Image&                  image,
            // FIXME update names/params after testing
            const std::size_t       m_iterations,
            const float             m_intensity,
            const float             m_threshold,
            const float             m_soft_threshold,
            const bool              m_fast_mode,
            const bool              m_debug_blur,
            const bool              m_debug_threshold)
        {
            const std::size_t MaxIterations = 16;
            const std::size_t MinBufferSize = 8;
            const float RadiusOffset = 8;

            // Prefilter pass.
            Image prefiltered_image = prefiltered(image, m_threshold, m_soft_threshold);

            // Determine the iteration count.
            const std::size_t width = m_fast_mode ? props.m_canvas_width / 2 : props.m_canvas_width;
            const std::size_t height = m_fast_mode ? props.m_canvas_height / 2 : props.m_canvas_height;

            // const float max_iterations =
            //     std::log2(static_cast<float>(std::min(width, height)))
            //     + m_radius - RadiusOffset; // adjusts the minimum size a buffer can have (e.g. 0.0f = 2x2 buffer)

            const std::size_t iterations = clamp<std::size_t>(
                m_iterations, // FIXME uncomment after testing: static_cast<int>(max_iterations),
                1, MaxIterations);

            // const float sample_scale = 0.5f + max_iterations - static_cast<int>(max_iterations);

            // Create blur buffer pyramids.
            std::vector<Image> blur_pyramid_down;
            std::vector<Image> blur_pyramid_up;

            blur_pyramid_down.reserve(iterations);
            blur_pyramid_up.reserve(iterations);

            for (std::size_t level = 0, level_width = width, level_height = height
                ; level < iterations
                ; ++level)
            {
                const CanvasProperties level_props(
                    level_width, level_height,
                    level_width, level_height,
                    3, // alpha is ignored
                    props.m_pixel_format);

                blur_pyramid_down.push_back(Image(level_props));
                blur_pyramid_up.push_back(Image(level_props));

                // Halve dimensions for the next buffer level.
                if (level_width >= 4 && level_height >= 4)
                {
                    level_width /= 2;
                    level_height /= 2;
                }
            }

            // Downsample.
            bilinear_filter_in_place(prefiltered_image, blur_pyramid_down.at(0));
            for (std::size_t level = 1; level < iterations; ++level)
                bilinear_filter_in_place(blur_pyramid_down.at(level - 1), blur_pyramid_down.at(level));

            // Upsample and blend.
            blur_pyramid_up.at(iterations - 1).copy_from(blur_pyramid_down.at(iterations - 1));
            if (iterations > 1)
            {
                for (std::size_t level = iterations - 2; ; --level)
                {
                    bilinear_filter_in_place(blur_pyramid_up.at(level + 1), blur_pyramid_up.at(level));

                    const CanvasProperties& level_props = blur_pyramid_up.at(level).properties();
                    const std::size_t level_width = level_props.m_canvas_width;
                    const std::size_t level_height = level_props.m_canvas_height;

                    for (std::size_t y = 0; y < level_height; ++y)
                    {
                        for (std::size_t x = 0; x < level_width; ++x)
                        {
                            Color3f color_up;
                            blur_pyramid_up.at(level).get_pixel(x, y, color_up);

                            Color3f color_down;
                            blur_pyramid_down.at(level).get_pixel(x, y, color_down);

                            color_up += color_down; // additive blend
                            blur_pyramid_up.at(level).set_pixel(x, y, color_up);
                        }
                    }

                    if (level == 0)
                        break;
                }
            }

            // Resolve pass.
            Image bloom_target(prefiltered_image.properties());
            bilinear_filter_in_place(blur_pyramid_up.at(0), bloom_target);

            for (std::size_t y = 0; y < props.m_canvas_height; ++y)
            {
                for (std::size_t x = 0; x < props.m_canvas_width; ++x)
                {
                    Color4f color;
                    image.get_pixel(x, y, color);

                    Color3f bloom_color;
                    bloom_target.get_pixel(x, y, bloom_color);

                    color.rgb() += m_intensity * bloom_color; // additive blend (weighted by intensity)
                    image.set_pixel(x, y, color);
                }
            }
        }

        //
        // Bloom post-processing effect execution.
        //

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            const CanvasProperties& props = frame.image().properties();
            Image& image = frame.image();

            // Use a pyramid of up/down scaled blur buffers.
            execute_kino_bloom(
                props,
                image,
                m_iterations,
                m_intensity,
                m_threshold,
                m_soft_threshold,
                m_fast_mode,
                m_debug_blur,
                m_debug_threshold);

            // // Ping-pong between two equally sized blur buffers.
            // execute_kawase_bloom(
            //     props,
            //     image,
            //     m_iterations,
            //     m_intensity,
            //     m_threshold,
            //     m_soft_threshold,
            //     m_fast_mode,
            //     m_debug_blur,
            //     m_debug_threshold);
        }

      private:
        std::size_t     m_iterations;
        float           m_intensity;
        float           m_threshold;
        float           m_soft_threshold;

        bool            m_fast_mode;
        bool            m_debug_blur;
        bool            m_debug_threshold;
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
                        .insert("value", "0")
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

    // TODO remove:

    metadata.push_back(
        Dictionary()
            .insert("name", "fast_mode")
            .insert("label", "Fast Mode")
            .insert("type", "bool")
            .insert("use", "optional")
            .insert("default", "false"));

    metadata.push_back(
        Dictionary()
            .insert("name", "debug_blur")
            .insert("label", "Debug Blur")
            .insert("type", "bool")
            .insert("use", "optional")
            .insert("default", "false"));

    metadata.push_back(
        Dictionary()
            .insert("name", "debug_threshold")
            .insert("label", "Debug Threshold")
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
