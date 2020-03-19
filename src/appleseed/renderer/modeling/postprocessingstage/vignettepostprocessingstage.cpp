
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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

    const float DefaultIntensity = 0.5f;
    const float MinIntensity = 0.0f;
    const float MaxIntensity = 1.0f;

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

        const char* get_model() const override {
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

            return true;
        }

        void execute(Frame& frame) const override
        {
            const CanvasProperties& props = frame.image().properties();
            const Vector2f resolution(static_cast<float>(props.m_canvas_width), static_cast<float>(props.m_canvas_height));
            const Vector2f aspect(resolution.x / resolution.y, 1);

            Image& image = frame.image();

            for (size_t y = 0; y <= resolution.y - 1; ++y)
            {
                for (size_t x = 0; x <= resolution.x - 1; ++x)
                {
                    Vector2f coord = (2.0f * Vector2f(static_cast<float>(x), static_cast<float>(y)) - resolution) / resolution.y;

                    //
                    // Port of Keijiro Takahashi's natural vignetting effect for Unity.
                    //
                    // Reference:
                    //
                    //   https://github.com/keijiro/KinoVignette
                    //

                    float rf = norm(coord) * m_intensity;
                    float rf2_1 = rf * rf + 1.0f;
                    float e = 1.0f / (rf2_1 * rf2_1);

                    Color4f background_premult;
                    image.get_pixel(x, y, background_premult);

                    background_premult.r *= e;
                    background_premult.g *= e;
                    background_premult.b *= e;

                    image.set_pixel(
                        x, y,
                        background_premult);
                }
            }
        }

      private:
          float m_intensity;
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
