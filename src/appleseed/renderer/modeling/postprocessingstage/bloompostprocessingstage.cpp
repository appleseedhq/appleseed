
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

#include <iostream>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Bloom post-processing stage.
    //

    const char* Model = "bloom_post_processing_stage";

    // TODO add default settings
    static constexpr float DefaultIntensity = 0.5f;
    static constexpr std::size_t DefaultIterations = 4;
    static constexpr std::size_t DefaultThreshold = 1;

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

            // TODO add default settings
            m_intensity = m_params.get_optional("intensity", DefaultIntensity, context);
            m_iterations = m_params.get_optional("iterations", DefaultIterations, context);
            m_threshold = m_params.get_optional("threshold", DefaultThreshold, context);

            return true;
        }

        static Color4f kawase_sample(Image& image, float frac_x, float frac_y, std::size_t offset)
        {
            // Sample the edge-most pixel when the coordinate is outside the image (i.e. texture clamping).
            std::size_t bottom_coord = static_cast<std::size_t>(max(frac_y - 0.5f - offset, 0.0f));
            std::size_t right_coord = static_cast<std::size_t>(min(frac_x + 0.5f + offset, image.properties().m_canvas_width - 1.0f));
            std::size_t left_coord = static_cast<std::size_t>(max(frac_x - 0.5f - offset, 0.0f));
            std::size_t top_coord = static_cast<std::size_t>(min(frac_y + 0.5f + offset, image.properties().m_canvas_height - 1.0f));

            Color3f bottom_right, bottom_left, top_right, top_left;
            image.get_pixel(right_coord, bottom_coord, bottom_right);
            image.get_pixel(left_coord, bottom_coord, bottom_left);
            image.get_pixel(right_coord, top_coord, top_right);
            image.get_pixel(left_coord, top_coord, top_left);

            // Average sample colors.
            Color3f sample = 0.25f * (top_right + top_left + bottom_right + bottom_left);
            return Color4f(sample.r, sample.g, sample.b, 1.0f); // TODO sample alphas (?)
        }

        static void kawase_blur(Image& src_image, Image& dst_image, std::size_t offset)
        {
            const CanvasProperties& props = src_image.properties();

            for (std::size_t y = 0; y < props.m_canvas_height; ++y)
            {
                for (std::size_t x = 0; x < props.m_canvas_width; ++x)
                {
                    Color4f bottom_right = kawase_sample(src_image, x + 0.5f, y - 0.5f, offset);
                    Color4f bottom_left = kawase_sample(src_image, x - 0.5f, y - 0.5f, offset);
                    Color4f top_right = kawase_sample(src_image, x + 0.5f, y + 0.5f, offset);
                    Color4f top_left = kawase_sample(src_image, x - 0.5f, y + 0.5f, offset);
                    dst_image.set_pixel(x, y, 0.25f * (top_right + top_left + bottom_right + bottom_left));
                }
            }
        }

        static void prefilter_in_place(Image& image, float threshold)
        {
            const CanvasProperties& props = image.properties();

            for (std::size_t y = 0; y < props.m_canvas_height; ++y)
            {
                for (std::size_t x = 0; x < props.m_canvas_width; ++x)
                {
                    Color4f color;
                    image.get_pixel(x, y, color);

                    float brightness = max_value(color.rgb());
                    float contribution = (brightness - threshold);

                    if (contribution <= 0.0f)
                        color.rgb() = Color3f(0.0f); // FIXME start with a clear image to skip this
                    else
                        color.rgb() *= contribution / max(brightness, 0.0001f); // avoid division by zero (0.0001 is arbitrary)

                    image.set_pixel(x, y, color);
                }
            }
        }

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            const CanvasProperties& props = frame.image().properties();
            Image& image = frame.image();

            // Set the offset values used for sampling in Kawase blur.
            const std::vector<std::size_t> iteration_offset = { 0, 1, 2, 2, 3 };
            assert(iteration_offset.size() <= m_iterations);

            // Copy the image to temporary render targets used for blurring.
            Image blur_buffer_1(frame.image());
            prefilter_in_place(blur_buffer_1, /*m_threshold*/0.5f);
            Image blur_buffer_2(blur_buffer_1);

            // Iteratively blur the image.
            // for (std::size_t i = 0; i < m_iterations; ++i)
            // {
            //     // "Ping-pong" between two temporaries used for storing intermediate results.
            //     if (i % 2 == 0)
            //         kawase_blur(blur_buffer_1, blur_buffer_2, iteration_offset[i]);
            //     else
            //         kawase_blur(blur_buffer_2, blur_buffer_1, iteration_offset[i]);
            // }
            Image& blur_buffer = m_iterations % 2 == 1 ? blur_buffer_2 : blur_buffer_1;

            {//* FIXME remove after debugging (only blurs the right side of image)
                for (size_t ty = 0; ty < props.m_tile_count_y; ++ty)
                    for (size_t tx = props.m_tile_count_x / 2; tx < props.m_tile_count_x; ++tx)
                        image.tile(tx, ty).copy_from(blur_buffer.tile(tx, ty));
                return;
            //*/
            }

            frame.image().copy_from(blur_buffer);
            // TODO bloom :)
        }

      private:
        // TODO add default settings
        float m_intensity;
        std::size_t m_iterations;
        std::size_t m_threshold;
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

    // TODO add default settings

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

    // TODO fix min/max values
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
                        .insert("value", "16")
                        .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "4"));

    // TODO fix min/max values
    metadata.push_back(
        Dictionary()
            .insert("name", "threshold")
            .insert("label", "Threshold")
            .insert("type", "int")
            .insert("min",
                    Dictionary()
                        .insert("value", "0")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "10")
                        .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1"));

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
