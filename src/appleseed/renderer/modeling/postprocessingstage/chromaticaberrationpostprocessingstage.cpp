
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
    constexpr float DefaultRefractiveIndexRed = 1.0f;
    constexpr float DefaultRefractiveIndexGreen = 1.015f;
    constexpr float DefaultRefractiveIndexBlue = 1.03f;

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

            //@TODO init parameters with m_params.get_optional
            m_refractive_index_red = m_params.get_optional("refractive_index_red", DefaultRefractiveIndexRed, context);
            m_refractive_index_green = m_params.get_optional("refractive_index_green", DefaultRefractiveIndexGreen, context);
            m_refractive_index_blue = m_params.get_optional("refractive_index_blue", DefaultRefractiveIndexBlue, context);

            return true;
        }

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            //@TODO apply the effect :)

            // ref.: https://www.shadertoy.com/view/XssGz8
            //       https://www.shadertoy.com/view/MtXXDr
            //       https://github.com/keijiro/KinoFringe

            const CanvasProperties& props = frame.image().properties();

            Image& image = frame.image();

            const Vector3f normal(0.0, 0.0, -1.0);

            for (std::size_t y = 0; y < props.m_canvas_height; ++y)
            {
                for (std::size_t x = 0; x < props.m_canvas_width; ++x)
                {
                    // Pixel coordinate normalized to be in the [-1, 1] range.
                    const Vector2f coord(
                        static_cast<float>(2 * x - props.m_canvas_width),
                        static_cast<float>(2 * y - props.m_canvas_height));

                    //
                    // Port of byungyoonc's "Physical Chromatic Aberration" ShaderToy.
                    //
                    // Reference:
                    //
                    //   https://www.shadertoy.com/view/MtXXDr
                    //

                    const Vector3f incident = normalize(Vector3f(coord.x, coord.y, 1.0f));

                    Vector3f refraction_red, refraction_green, refraction_blue;
                    refract(incident, normal, m_refractive_index_red, refraction_red);
                    refract(incident, normal, m_refractive_index_green, refraction_green);
                    refract(incident, normal, m_refractive_index_blue, refraction_blue);

                    refraction_red /= refraction_red.z;
                    refraction_green /= refraction_green.z;
                    refraction_blue /= refraction_blue.z;

                    // Sampling coordinates normalized to be in the [0, 1] range.
                    const Vector2f coord_red(0.5f * (refraction_red.x + 1.0f), 0.5f * (refraction_red.y + 1.0f));
                    const Vector2f coord_green(0.5f * (refraction_green.x + 1.0f), 0.5f * (refraction_green.y + 1.0f));
                    const Vector2f coord_blue(0.5f * (refraction_blue.x + 1.0f), 0.5f * (refraction_blue.y + 1.0f));

                    Color3f sample_red, sample_green, sample_blue;
                    image.get_pixel(
                        std::min(static_cast<std::size_t>(x * coord_red.x + 0.5f), props.m_canvas_width - 1),
                        std::min(static_cast<std::size_t>(y * coord_red.y + 0.5f), props.m_canvas_height - 1),
                        sample_red);
                    image.get_pixel(
                        std::min(static_cast<std::size_t>(x * coord_green.x + 0.5f), props.m_canvas_width - 1),
                        std::min(static_cast<std::size_t>(y * coord_green.y + 0.5f), props.m_canvas_height - 1),
                        sample_green);
                    image.get_pixel(
                        std::min(static_cast<std::size_t>(x * coord_blue.x + 0.5f), props.m_canvas_width - 1),
                        std::min(static_cast<std::size_t>(y * coord_blue.y + 0.5f), props.m_canvas_height - 1),
                        sample_blue);

                    image.set_pixel(x, y, Color3f(sample_red.r, sample_green.g, sample_blue.b));
                }
            }
        }

      private:
        //@TODO add parameters
        float m_refractive_index_red;
        float m_refractive_index_green;
        float m_refractive_index_blue;
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

    //@TODO expose parameters
    metadata.push_back(
        Dictionary()
            .insert("name", "refractive_index_red")
            .insert("label", "Refractive Index (Red)")
            .insert("type", "numeric")
            .insert("min",
                    Dictionary()
                        .insert("value", "0.0")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "2.0")
                        .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1.0")); // DefaultRefractiveIndexRed

    metadata.push_back(
        Dictionary()
            .insert("name", "refractive_index_green")
            .insert("label", "Refractive Index (Green)")
            .insert("type", "numeric")
            .insert("min",
                    Dictionary()
                        .insert("value", "0.0")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "2.0")
                        .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1.0")); // DefaultRefractiveIndexGreen

    metadata.push_back(
        Dictionary()
            .insert("name", "refractive_index_blue")
            .insert("label", "Refractive Index (Blue)")
            .insert("type", "numeric")
            .insert("min",
                    Dictionary()
                        .insert("value", "0.0")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "2.0")
                        .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1.0")); // DefaultRefractiveIndexBlue

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
