
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
#include "chromaticaberrationpostprocessingstage.h"

// appleseed.renderer headers.
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/postprocessingstage/postprocessingstage.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cstddef>
#include <memory>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Chromatic aberration post-processing stage.
    //

    const char* Model = "chromatic_aberration_post_processing_stage";

    constexpr float DefaultOffset = 0.0f;
    constexpr std::size_t DefaultMinShift = 0;
    constexpr std::size_t DefaultMaxShift = 4;

    class ChromaticAberrationPostProcessingStage
      : public PostProcessingStage
    {
      public:
        ChromaticAberrationPostProcessingStage(
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

            m_offset = m_params.get_optional("offset", DefaultOffset, context);
            m_min_shift = m_params.get_optional("min_shift", DefaultMinShift, context);
            m_max_shift = m_params.get_optional("max_shift", DefaultMaxShift, context);

            return true;
        }

        Color3f safe_sample_at(const Image& image, const std::size_t x, const std::size_t y) const
        {
            Color3f sample;

            // Clamp out of range coordinates.
            image.get_pixel(
                std::min(x, image.properties().m_canvas_width - 1),
                std::min(y, image.properties().m_canvas_height - 1),
                sample);

            return sample;
        }

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            const CanvasProperties& props = frame.image().properties();

            Image& image = frame.image();

            const Vector2f resolution(
                static_cast<float>(props.m_canvas_width),
                static_cast<float>(props.m_canvas_height));

            for (std::size_t y = 0; y < props.m_canvas_height; ++y)
            {
                for (std::size_t x = 0; x < props.m_canvas_width; ++x)
                {
                    //
                    // Simulates transverse (lateral) chromatic aberration of lenses.
                    //
                    // References:
                    //
                    //   https://en.wikipedia.org/wiki/Chromatic_aberration#Types
                    //   https://github.com/keijiro/KinoFringe/
                    //

                    // Pixel coordinate normalized to be in the [-1, 1] range vertically.
                    const Vector2f coord(
                        (2.0f * x - resolution.x) / resolution.y,
                        (2.0f * y - resolution.y) / resolution.y);

                    // Increase color shifting (linearly) towards the edges of the frame.
                    const float radial_intensity = norm(coord) - m_offset;

                    const std::size_t shift_amount =
                        round<std::size_t>(
                            mix(static_cast<float>(m_min_shift), static_cast<float>(m_max_shift), radial_intensity));

                    // Sample the original pixel coordinate with slightly different shifts for each color component.
                    const Color3f color(
                        safe_sample_at(image, x, y).r,
                        safe_sample_at(image, x + shift_amount, y + shift_amount).g,
                        safe_sample_at(image, x + 2 * shift_amount, y + 2 * shift_amount).b);

                    image.set_pixel(x, y, color);
                }
            }

        }

      private:
        float m_offset;
        std::size_t m_min_shift;
        std::size_t m_max_shift;
    };
}


//
// ChromaticAberrationPostProcessingStageFactory class implementation.
//

void ChromaticAberrationPostProcessingStageFactory::release()
{
    delete this;
}

const char* ChromaticAberrationPostProcessingStageFactory::get_model() const
{
    return Model;
}

Dictionary ChromaticAberrationPostProcessingStageFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Chromatic Aberration");
}

DictionaryArray ChromaticAberrationPostProcessingStageFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_input_metadata(metadata);

    metadata.push_back(
        Dictionary()
            .insert("name", "offset")
            .insert("label", "Offset")
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
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "min_shift")
            .insert("label", "Min Shift")
            .insert("type", "integer")
            .insert("min",
                    Dictionary()
                        .insert("value", "0")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "5")
                        .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "max_shift")
            .insert("label", "Max Shift")
            .insert("type", "integer")
            .insert("min",
                    Dictionary()
                        .insert("value", "0")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "15")
                        .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "4"));

    return metadata;
}

auto_release_ptr<PostProcessingStage> ChromaticAberrationPostProcessingStageFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<PostProcessingStage>(
        new ChromaticAberrationPostProcessingStage(name, params));
}

}   // namespace renderer
