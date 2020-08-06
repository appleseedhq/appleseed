
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

    //@TODO add default parameters
    constexpr float DefaultAxialStrength = 0.8f;
    constexpr float DefaultAxialShift = 0.3f;
    constexpr float DefaultLateralShift = 0.3f;

    // Poisson disk sample points
#define AXIAL_SAMPLE_LOW 0
#if AXIAL_SAMPLE_LOW
    constexpr std::size_t SAMPLE_NUM = 8;
    constexpr float POISSON_SAMPLES[2 * SAMPLE_NUM] =
    {
         0.373838022357f,   0.662882019975f,
        -0.335774814282f,  -0.940070127794f,
        -0.9115721822f,     0.324130702404f,
         0.837294074715f,  -0.504677167232f,
        -0.0500874221246f, -0.0917990757772f,
        -0.358644570242f,   0.906381100284f,
         0.961200130218f,   0.219135111748f,
        -0.896666615007f,  -0.440304757692f
    };
#else
    constexpr std::size_t SAMPLE_NUM = 16;
    constexpr float POISSON_SAMPLES[2 * SAMPLE_NUM] =
    {
         0.0984258332809f,   0.918808284462f,
         0.00259138629413f, -0.999838959623f,
        -0.987959729023f,   -0.00429660140761f,
         0.981234239267f,   -0.140666219895f,
        -0.0212157973013f,  -0.0443286928994f,
        -0.652058534734f,    0.695078086985f,
        -0.68090417832f,    -0.681862769398f,
         0.779643686501f,    0.603399060386f,
         0.67941165083f,    -0.731372789969f,
         0.468821477499f,   -0.251621416756f,
         0.278991228738f,    0.39302189329f,
        -0.191188273806f,   -0.527976638433f,
        -0.464789669525f,    0.216311272754f,
        -0.559833960421f,   -0.256176089172f,
         0.65988403582f,     0.170056284903f,
        -0.170289189543f,    0.551561042407f
    };
#endif

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

            m_axial_strength  = m_params.get_optional("axial_strength", DefaultAxialStrength, context);
            m_axial_shift  = m_params.get_optional("axial_shift", DefaultAxialShift, context);
            m_lateral_shift  = m_params.get_optional("lateral_shift", DefaultLateralShift, context);

            return true;
        }

        // The coordinates uv are assumed to be in the [0, 1] range.
        Color3f sample_rgb_at(const Image& image, const Vector2f& uv) const
        {
            Color3f rgb_sample;

            const std::size_t x = truncate<std::size_t>(image.properties().m_canvas_width * uv.x);
            const std::size_t y = truncate<std::size_t>(image.properties().m_canvas_height * uv.y);

            image.get_pixel(x, y, rgb_sample);

            return rgb_sample;
        }

        Color3f poisson_filter(const Image& image, const Vector2f& uv, const float aspect_ratio) const
        {
            Color3f acc(0.0f);
            for (std::size_t i = 0; i < SAMPLE_NUM; ++i)
            {
                Vector2f displacement(POISSON_SAMPLES[i] * 0.02f * m_axial_shift);
                displacement.x *= aspect_ratio;
                acc += sample_rgb_at(image, uv + displacement);
            }
            return acc / SAMPLE_NUM;
        }

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            const CanvasProperties& props = frame.image().properties();

            Image& image = frame.image();

            const Vector2f resolution(
                static_cast<float>(props.m_canvas_width),
                static_cast<float>(props.m_canvas_height));

            const float aspect_ratio = resolution.x / resolution.y;
            const float aspect_ratio_rcp = resolution.y / resolution.x;

            for (std::size_t y = 0; y < props.m_canvas_height; ++y)
            {
                for (std::size_t x = 0; x < props.m_canvas_width; ++x)
                {
                    //
                    // Port of Keijiro Takahashi's chromatic aberration image effect for Unity.
                    // Simulates the two types of chromatic aberration (CA) of lenses:
                    //
                    //   * Axial (Longitudinal) CA: introduces purple finges around strong highlights
                    //   * Transverse (Lateral) CA: distorts color planes in the edge region of the screen
                    //
                    // References:
                    //
                    //   https://github.com/keijiro/KinoFringe/
                    //   https://en.wikipedia.org/wiki/Chromatic_aberration#Types
                    //

                    Color4f pixel;
                    image.get_pixel(x, y, pixel);

                    // Pixel coordinates normalized to be in the [0, 1] range.
                    const float u = (x + 0.5f) / resolution.x;
                    const float v = (y + 0.5f) / resolution.y;

                    // Pixel coordinate normalized to be in the [-1/2, 1/2] range vertically.
                    const Vector2f spc((u - 0.5f) * aspect_ratio_rcp, (v - 0.5f)); //@NOTE spc = screen position centered (?)

                    //
                    // Lateral chromatic aberration.
                    //

                    // Defocus the red and blue planes (assume the green is in focus), to simulate "purple fringing".
                    // Since this effect does not occur in the center of the image, and increases towards the edge, we weight it by r2.
                    const float r2 = dot(spc, spc); // [0, 1/4]
                    const float shift_amount = 0.02f * m_lateral_shift * r2; // 0.02 * [0, 1] * [0, 1/4] = [0, 0.005]

                    const float f_r = 1.0f - shift_amount; // [0.995, 1.0]
                    const float f_b = 1.0f + shift_amount; // [1.0, 1.005]

                    // Sample neighboring pixels colors to simulate lateral CA.
                    Color4f pixel_r, pixel_b;

                    image.get_pixel(
                        // [-1/2, 1/2] * [0.995, 1.0] + 1/2 = [0, 1]
                        truncate<std::size_t>(resolution.x * ((u - 0.5f) * f_r + 0.5f)),
                        truncate<std::size_t>(resolution.y * ((v - 0.5f) * f_r + 0.5f)),
                        pixel_r);

                    image.get_pixel(
                        // [-1/2, 1/2] * [1.0, 1.005] + 1/2 = [0.0025, 1.0025]
                        std::min(truncate<std::size_t>(resolution.x * ((u - 0.5f) * f_b + 0.5f)), props.m_canvas_width - 1),
                        std::min(truncate<std::size_t>(resolution.y * ((v - 0.5f) * f_b + 0.5f)), props.m_canvas_height - 1),
                        pixel_b);

                    pixel.r = pixel_r.r;
                    pixel.b = pixel_b.b;

                    //
                    // Axial chromatic aberration.
                    //

                    if (m_axial_strength > 0.0f)
                    {
                        const Color3f blur = poisson_filter(image, Vector2f(u, v), aspect_ratio);
                        const float delta = luminance(blur) - luminance(pixel.rgb());
                        pixel.r = std::max(pixel.r, blur.r * delta * m_axial_strength);
                        pixel.b = std::max(pixel.b, blur.b * delta * m_axial_strength);
                    }

                    image.set_pixel(x, y, pixel);
                }
            }

        }

      private:
        float m_axial_strength;
        float m_axial_shift;
        float m_lateral_shift;
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

    //
    // Axial (Longitudinal) Chromatic Aberration.
    //

    metadata.push_back(
        Dictionary()
            .insert("name", "axial_strength")
            .insert("label", "Axial Strength")
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
            .insert("default", "0.8"));

    metadata.push_back(
        Dictionary()
            .insert("name", "axial_shift")
            .insert("label", "Axial Shift")
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
            .insert("default", "0.3"));

    //
    // Transverse (Lateral) Chromatic Aberration.
    //

    metadata.push_back(
        Dictionary()
            .insert("name", "lateral_shift")
            .insert("label", "Lateral Shift")
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
            .insert("default", "0.3"));

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
