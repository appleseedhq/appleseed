
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

        static Color3f sample_box(Image& image, Vector2u pixel_coord, std::size_t offset) {
                std::size_t top_coord    = min(pixel_coord.y + offset, image.properties().m_canvas_height);
                std::size_t right_coord  = min(pixel_coord.x + offset, image.properties().m_canvas_width);
                std::size_t bottom_coord = max(pixel_coord.y, offset) - offset;
                std::size_t left_coord   = max(pixel_coord.x, offset) - offset;

                Color3f top_left, top_right, bottom_left, bottom_right;
                image.get_pixel(left_coord, top_coord, top_left);
                image.get_pixel(right_coord, top_coord, top_right);
                image.get_pixel(left_coord, bottom_coord, bottom_left);
                image.get_pixel(right_coord, bottom_coord, bottom_right);

                return 0.25f * (top_left + top_right + bottom_left + bottom_right);
        }

        static Color3f sample_box_borderless(Image& image, Vector2u pixel_coord, std::size_t offset) {
                std::size_t sampled_count = 0;
                Color3f sampled_color, sampled_color_sum(0.0f, 0.0f, 0.0f);

                bool sample_bottom = pixel_coord.y >= offset;
                bool sample_right = pixel_coord.x + offset <= image.properties().m_canvas_width;
                bool sample_left = pixel_coord.x >= offset;
                bool sample_top = pixel_coord.y + offset <= image.properties().m_canvas_height;

                if (sample_top)
                {
                    std::size_t top_coord = pixel_coord.y + offset;
                    if (sample_right)
                    {
                        image.get_pixel(pixel_coord.x + offset, top_coord, sampled_color);
                        sampled_color_sum += sampled_color;
                        sampled_count += 1;
                    }
                    if (sample_left)
                    {
                        image.get_pixel(pixel_coord.x - offset, top_coord, sampled_color);
                        sampled_color_sum += sampled_color;
                        sampled_count += 1;
                    }
                }
                if (sample_bottom)
                {
                    std::size_t bottom_coord = pixel_coord.y - offset;
                    if (sample_right)
                    {
                        image.get_pixel(pixel_coord.x + offset, bottom_coord, sampled_color);
                        sampled_color_sum += sampled_color;
                        sampled_count += 1;
                    }
                    if (sample_left)
                    {
                        image.get_pixel(pixel_coord.x - offset, bottom_coord, sampled_color);
                        sampled_color_sum += sampled_color;
                        sampled_count += 1;
                    }
                }

                return (1.0f / static_cast<float>(sampled_count)) * sampled_color_sum;
        }

        static void box_blur(Image& src_image, Image& dst_image, std::size_t offset)
        {
            const CanvasProperties& props = src_image.properties();

            for (std::size_t y = 0; y < props.m_canvas_height; ++y)
            {
                for (std::size_t x = 0; x < props.m_canvas_width; ++x)
                {
                    Color3f color = sample_box_borderless(src_image, Vector2u(x, y), offset);
                    dst_image.set_pixel(x, y, color);
                }
            }
        }

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            // TODO bloom :)
            // - start with progressive downsampling and rendering to a temporary "texture"
            Image tmp1(frame.image());
            Image tmp2(frame.image());

            box_blur(tmp1, tmp2, 1);
            box_blur(tmp2, tmp1, 2);
            box_blur(tmp2, tmp1, 2);
            box_blur(tmp1, tmp2, 3);

            frame.image().copy_from(tmp2);
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

    // TODO fix min/max values and enforce uint type
    metadata.push_back(
        Dictionary()
            .insert("name", "iterations")
            .insert("label", "Iterations")
            .insert("type", "numeric")
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

    // TODO fix min/max values and enforce uint type
    metadata.push_back(
        Dictionary()
            .insert("name", "threshold")
            .insert("label", "Threshold")
            .insert("type", "numeric")
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
