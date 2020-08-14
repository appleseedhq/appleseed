
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
#include "renderer/modeling/postprocessingstage/effect/chromaticaberrationapplier.h"
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

    constexpr float DefaultStrength = 0.4f;
    constexpr std::size_t DefaultSampleCount = 8;

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

            m_strength = m_params.get_optional("strength", DefaultStrength, context);
            m_sample_count = m_params.get_optional("quality", DefaultSampleCount, context);

            return true;
        }

        Color3f spectrum_offset(const float t) const
        {
            //
            // Linearly interpolates blur-weights from red to green to blue.
            //
            // References:
            //
            //    https://www.shadertoy.com/view/XssGz8
            //    https://www.shadertoy.com/view/MdsyDX
            //

            const float t0 = 3.0f * t - 1.5f;

            return
                Color3f(
                    saturate(-t0),
                    saturate(1.0f - std::abs(t0)),
                    saturate(+t0));
        }

        Vector2f radial_distort(const Vector2f& uv, const float amount) const
        {
            const Vector2f radius(uv - Vector2f(0.5f));

            // Increase distortion towards the image edges.
            return uv + radius * dot(radius, radius) * amount;
        }

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            const CanvasProperties& props = frame.image().properties();

            Image& image = frame.image();

            const Vector2f resolution(
                static_cast<float>(props.m_canvas_width),
                static_cast<float>(props.m_canvas_height));

            const auto sample_at =
                [&resolution, image](const Vector2f uv) -> Color3f
                {
                    // Remap uv to image coordinates, clamping out of range values.
                    const float fx = clamp(uv.x * resolution.x, 0.0f, resolution.x - 1.0f);
                    const float fy = clamp(uv.y * resolution.y, 0.0f, resolution.y - 1.0f);

                    Color3f sample;
                    image.get_pixel(truncate<std::size_t>(fx), truncate<std::size_t>(fy), sample);

                    return sample;
                };

            for (std::size_t y = 0; y < props.m_canvas_height; ++y)
            {
                for (std::size_t x = 0; x < props.m_canvas_width; ++x)
                {
                    const float fx = static_cast<float>(x) + 0.5f;
                    const float fy = static_cast<float>(y) + 0.5f;

                    // Pixel coordinate normalized to be in the [0, 1] range.
                    const Vector2f uv(fx / resolution.x, fy / resolution.y);

                    //
                    // Reference:
                    //
                    //   http://loopit.dk/rendering_inside.pdf (slides 19-20)
                    //

                    Color3f color_sum(0.0f);
                    Color3f weight_sum(0.0f);

                    for (std::size_t i = 0; i < m_sample_count; ++i)
                    {
                        const float t = static_cast<float>(i) / (m_sample_count - 1.0f);

                        const Color3f weight = spectrum_offset(t);
                        weight_sum += weight;

                        const Vector2f d_uv(radial_distort(uv, 0.6f * m_strength * t));
                        color_sum += weight * sample_at(d_uv);
                    }

                    image.set_pixel(x, y, color_sum / weight_sum);
                }
            }

            /*
            const ChromaticAberrationApplier chromatic_aberration(...);
            chromatic_aberration.apply_on_tiles(image, thread_count);
            */
        }

      private:
        float m_strength;
        std::size_t m_sample_count;
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
            .insert("name", "strength")
            .insert("label", "Strength")
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
            .insert("default", "0.4"));

    metadata.push_back(
        Dictionary()
            .insert("name", "quality")
            .insert("label", "Quality")
            .insert("type", "integer")
            .insert("min",
                    Dictionary()
                        .insert("value", "3")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "24")
                        .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "8"));

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
