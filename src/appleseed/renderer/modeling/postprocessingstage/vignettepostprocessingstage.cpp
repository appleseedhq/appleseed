
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
#include "vignettepostprocessingstage.h"

// appleseed.renderer headers.
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

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Vignette post-processing stage.
    //

    const char* Model = "vignette_post_processing_stage";

    static constexpr float DefaultIntensity = 0.5f;

    static constexpr float DefaultAnisotropy = 0.0f;

    class VignettePostProcessingStage
      : public PostProcessingStage
    {
      public:
        VignettePostProcessingStage(
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

            m_intensity = m_params.get_optional("intensity", DefaultIntensity, context);
            m_anisotropy = m_params.get_optional("anisotropy", DefaultAnisotropy, context);

            return true;
        }

        void execute(Frame& frame) const override
        {
            const CanvasProperties& props = frame.image().properties();
            const Vector2f resolution(static_cast<float>(props.m_canvas_width), static_cast<float>(props.m_canvas_height));
            const Vector2f normalization_factor(lerp(resolution.y, resolution.x, m_anisotropy), resolution.y);

            Image& image = frame.image();

            for (std::size_t y = 0; y < props.m_canvas_height; ++y)
            {
                for (std::size_t x = 0; x < props.m_canvas_width; ++x)
                {
                    // Pixel coordinate normalized to be in the [-1, 1] range vertically.
                    const Vector2f coord = (2.0f * Vector2f(static_cast<float>(x), static_cast<float>(y)) - resolution) / normalization_factor;

                    //
                    // Port of Keijiro Takahashi's natural vignetting effect for Unity.
                    // Recreates natural illumination falloff, which is approximated by the "cosine fourth" law of illumination falloff.
                    //
                    // References:
                    //
                    //   https://github.com/keijiro/KinoVignette
                    //   https://en.wikipedia.org/wiki/Vignetting#Natural_vignetting
                    //

                    const float linear_radial_falloff = norm(coord) * m_intensity;
                    const float quadratic_radial_falloff = linear_radial_falloff * linear_radial_falloff + 1.0f;

                    // Inversely proportional to the fourth power of the distance from the pixel to the image center.
                    const float inverse_biquadratic_radial_falloff = 1.0f / (quadratic_radial_falloff * quadratic_radial_falloff);

                    Color4f pixel;
                    image.get_pixel(x, y, pixel);

                    pixel.rgb() *= inverse_biquadratic_radial_falloff;

                    image.set_pixel(x, y, pixel);
                }
            }
        }

      private:
        float m_intensity;
        float m_anisotropy;
    };
}


//
// VignettePostProcessingStageFactory class implementation.
//

void VignettePostProcessingStageFactory::release()
{
    delete this;
}

const char* VignettePostProcessingStageFactory::get_model() const
{
    return Model;
}

Dictionary VignettePostProcessingStageFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Vignette");
}

DictionaryArray VignettePostProcessingStageFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_input_metadata(metadata);

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
            .insert("name", "anisotropy")
            .insert("label", "Anisotropy")
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

    return metadata;
}

auto_release_ptr<PostProcessingStage> VignettePostProcessingStageFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<PostProcessingStage>(
        new VignettePostProcessingStage(name, params));
}

}   // namespace renderer
