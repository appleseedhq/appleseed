
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
            m_downsample = m_params.get_optional("downsample", false, context);
            m_debug_blur = m_params.get_optional("debug_blur", false, context);

            return true;
        }

        //
        // Simple image color sampling.
        //

        // Average the color values of a 2x2 block around (half_x, half_y).
        static Color3f corner_sample(const Image& image, const float half_x, const float half_y)
        {
            const float max_x = image.properties().m_canvas_width - 1.0f;
            const float max_y = image.properties().m_canvas_height - 1.0f;

            // Sample the edge-most pixel when the coordinate is outside the image (i.e. texture clamping).
            const std::size_t top_coord = static_cast<std::size_t>(clamp(half_y + 0.5f, 0.0f, max_y));
            const std::size_t left_coord = static_cast<std::size_t>(clamp(half_x - 0.5f, 0.0f, max_x));
            const std::size_t right_coord = static_cast<std::size_t>(clamp(half_x + 0.5f, 0.0f, max_x));
            const std::size_t bottom_coord = static_cast<std::size_t>(clamp(half_y - 0.5f, 0.0f, max_y));

            Color3f top_left, top_right, bottom_left, bottom_right;
            image.get_pixel(left_coord, top_coord, top_left);
            image.get_pixel(right_coord, top_coord, top_right);
            image.get_pixel(left_coord, bottom_coord, bottom_left);
            image.get_pixel(right_coord, bottom_coord, bottom_right);

            // Average pixel colors.
            return 0.25f * (top_left + top_right + bottom_left + bottom_right);
        }

        //
        // Sampling filter from Masaki Kawase's GDC2003 Presentation: "Frame Buffer Postprocessing Effects in DOUBLE-S.T.E.A.L (Wreckless)".
        //

        static Color3f kawase_sample(const Image& image, const std::size_t x, const std::size_t y, const std::size_t offset)
        {
            const float fx = static_cast<float>(x);
            const float fy = static_cast<float>(y);
            const float off = offset + 0.5f;

            // Since each corner sample is an average of 4 values, 16 pixels are used in total.
            Color3f top_left = corner_sample(image, fx - off, fy + off);
            Color3f top_right = corner_sample(image, fx + off, fy + off);
            Color3f bottom_left = corner_sample(image, fx - off, fy - off);
            Color3f bottom_right = corner_sample(image, fx + off, fy - off);

            // Average sample colors.
            return 0.25f * (top_left + top_right + bottom_left + bottom_right);
        }

        //
        // Sampling filters from Marius Bjørge's SIGGRAPH2015 Presentation: "Bandwidth-Efficient Rendering".
        //

        static Color3f dual_filter_downsample(const Image& image, const float half_x, const float half_y)
        {
            Color3f center = corner_sample(image, half_x, half_y);
            Color3f top_left = corner_sample(image, half_x - 1.0f, half_y + 1.0f);
            Color3f top_right = corner_sample(image, half_x + 1.0f, half_y + 1.0f);
            Color3f bottom_left = corner_sample(image, half_x - 1.0f, half_y - 1.0f);
            Color3f bottom_right = corner_sample(image, half_x + 1.0f, half_y - 1.0f);

            return ((4.0f * center) + (top_left + top_right + bottom_left + bottom_right)) / 8.0f;
        }

        static Color3f dual_filter_upsample(const Image& image, const std::size_t x, const std::size_t y)
        {
            const float fx = static_cast<float>(x);
            const float fy = static_cast<float>(y);
            const float max_x = image.properties().m_canvas_width - 1.0f;
            const float max_y = image.properties().m_canvas_height - 1.0f;

            // Sample the edge-most pixel when the coordinate is outside the image (i.e. texture clamping).
            const std::size_t top_coord = static_cast<std::size_t>(clamp(fy + 1.0f, 0.0f, max_y));
            const std::size_t left_coord = static_cast<std::size_t>(clamp(fx - 1.0f, 0.0f, max_x));
            const std::size_t right_coord = static_cast<std::size_t>(clamp(fx + 1.0f, 0.0f, max_x));
            const std::size_t bottom_coord = static_cast<std::size_t>(clamp(fy - 1.0f, 0.0f, max_y));

            Color3f top, left, right, bottom;
            image.get_pixel(x, top_coord, top);
            image.get_pixel(left_coord, y, left);
            image.get_pixel(right_coord, y, right);
            image.get_pixel(x, bottom_coord, bottom);

            Color3f top_left = corner_sample(image, fx - 0.5f, fy + 0.5f);
            Color3f top_right = corner_sample(image, fx + 0.5f, fy + 0.5f);
            Color3f bottom_left = corner_sample(image, fx - 0.5f, fy - 0.5f);
            Color3f bottom_right = corner_sample(image, fx + 0.5f, fy - 0.5f);

            return ((top, left, right, bottom) + 2.0f * (top_left + top_right + bottom_left + bottom_right)) / 12.0f;
        }

        //
        // Image blurring.
        //

        static void kawase_blur(const Image& src_image, Image& dst_image, const std::size_t offset, const bool additive_blend = true)
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

                    if (additive_blend)
                    {
                        Color3f color;
                        dst_image.get_pixel(x, y, color);
                        result += color;
                    }

                    dst_image.set_pixel(x, y, result);
                }
            }
        }

        //
        // Image up/down sampling through bilinear interpolation.
        //

        static inline CanvasProperties scaled_canvas(const CanvasProperties& props, const float scaling_factor)
        {
            return CanvasProperties(
                static_cast<std::size_t>(scaling_factor * static_cast<float>(props.m_canvas_width)),
                static_cast<std::size_t>(scaling_factor * static_cast<float>(props.m_canvas_height)),
                static_cast<std::size_t>(scaling_factor * static_cast<float>(props.m_tile_width)),
                static_cast<std::size_t>(scaling_factor * static_cast<float>(props.m_tile_height)),
                props.m_channel_count,
                props.m_pixel_format);
        }

        static Image bilinear_interpolation(Image& image, const float scaling_factor)
        {
            if (feq(scaling_factor, 1.0f))
            {
                return Image(image);
            }

            const CanvasProperties& src_props = image.properties();
            assert(src_props.m_channel_count == 3);

            Image scaled_image(scaled_canvas(src_props, scaling_factor));
            const CanvasProperties& dst_props = scaled_image.properties();

            const std::size_t src_width = src_props.m_canvas_width;
            const std::size_t src_height = src_props.m_canvas_height;
            const std::size_t dst_width = dst_props.m_canvas_width; // src_width * scaling_factor
            const std::size_t dst_height = dst_props.m_canvas_height; // src_height * scaling_factor

            // Perform bilinear interpolation for up/down sampling.
            for (std::size_t y = 0; y < dst_height; ++y)
            {
                for (std::size_t x = 0; x < dst_width; ++x)
                {
                    float fx = static_cast<float>(x) / (dst_width - 1);
                    float fy = static_cast<float>(y) / (dst_height - 1);

                    fx *= src_width - 1;
                    fy *= src_height - 1;

                    // Retrieve the four surrounding pixels.
                    Color3f c00, c10, c01, c11;

                    const std::size_t x0 = truncate<std::size_t>(fx);
                    const std::size_t y0 = truncate<std::size_t>(fy);
                    const std::size_t x1 = std::min<std::size_t>(x0 + 1, src_width - 1);
                    const std::size_t y1 = std::min<std::size_t>(y0 + 1, src_height - 1);

                    image.get_pixel(x0, y0, c00);
                    image.get_pixel(x1, y0, c10);
                    image.get_pixel(x0, y1, c01);
                    image.get_pixel(x1, y1, c11);

                    // Compute weights.
                    const float wx1 = fx - x0;
                    const float wy1 = fy - y0;
                    const float wx0 = 1.0f - wx1;
                    const float wy0 = 1.0f - wy1;

                    // Store the weighted sum.
                    const Color3f result =
                        c00 * wx0 * wy0 +
                        c10 * wx1 * wy0 +
                        c01 * wx0 * wy1 +
                        c11 * wx1 * wy1;

                    scaled_image.set_pixel(x, y, result);
                }
            }

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
                        contribution = max(soft, contribution);
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

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            const CanvasProperties& props = frame.image().properties();
            Image& image = frame.image();

            // Set the offset values used for sampling in Kawase blur.
            const std::vector<std::size_t> iteration_offset = { 0, 1, 2, 2, 3 };
            assert(iteration_offset.size() <= m_iterations);

            // Copy the image to temporary render targets used for blurring.
            Image bloom_blur_1 = bilinear_interpolation(
                prefiltered(image, m_threshold, m_soft_threshold),
                m_fast_mode ? 0.5f : 1.0f);
            Image bloom_blur_2 = Image(bloom_blur_1);
            Image& bloom_blur = m_iterations % 2 == 1 ? bloom_blur_2 : bloom_blur_1; // last blur target

            // Iteratively blur the image, "ping-ponging" between the two temporaries.
            for (std::size_t i = 0; i < m_iterations; ++i)
            {
                if (i % 2 == 0)
                    kawase_blur(bloom_blur_1, bloom_blur_2, iteration_offset[i]);
                else
                    kawase_blur(bloom_blur_2, bloom_blur_1, iteration_offset[i]);
            }

            const Image &bloom_target = m_fast_mode ? bilinear_interpolation(bloom_blur, 2.0f) : bloom_blur;

            // Blend colors from the original and the blurred image to achieve bloom.
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

      private:
        std::size_t     m_iterations;
        float           m_intensity;
        float           m_threshold;
        float           m_soft_threshold;
        bool            m_fast_mode;
        bool            m_downsample;
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
            .insert("name", "downsample")
            .insert("label", "Downsample")
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
