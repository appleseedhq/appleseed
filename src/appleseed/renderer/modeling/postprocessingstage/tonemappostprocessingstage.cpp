
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
#include "tonemappostprocessingstage.h"

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
#include "foundation/utility/makevector.h"

// Standard headers.
#include <cstddef>
#include <memory>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Tone map post-processing stage.
    //

    const char* Model = "tone_map_post_processing_stage";

    // TODO add default param values
    static constexpr ToneMapOperator DeafutToneMapOperator = ToneMapOperator::FILMIC;

    static constexpr bool DeafutClampValues = false;

    class ToneMapPostProcessingStage
      : public PostProcessingStage
    {
      public:
        ToneMapPostProcessingStage(
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

            m_clamp_values = m_params.get_optional("clamp_values", DeafutClampValues, context);

            // TODO retrive params
            const std::string tone_map_operator =
                m_params.get_optional<std::string>(
                    "tone_map_operator",
                    "filmic",
                    make_vector("aces_unreal", "filmic"),
                    context);

            if (tone_map_operator == "aces_unreal")
                m_operator = ToneMapOperator::ACES_UNREAL;
            else
            {
                assert(tone_map_operator == "filmic");

                m_operator = ToneMapOperator::FILMIC;
            }

            return true;
        }

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            const CanvasProperties& props = frame.image().properties();

            Image& image = frame.image();

            // FIXME abstract to effect appliers
            for (std::size_t y = 0; y < props.m_canvas_height; ++y)
            {
                for (std::size_t x = 0; x < props.m_canvas_width; ++x)
                {
                    Color4f pixel;
                    image.get_pixel(x, y, pixel);

                    Color3f& color = pixel.rgb();

                    switch (m_operator)
                    {
                      case ToneMapOperator::ACES_UNREAL:
                        color = color / (color + Color3f(0.155f)) * 1.019f;
                        break;

                      case ToneMapOperator::FILMIC:
                        color = component_wise_max(Color3f(0.0f), color - Color3f(0.004f));
                        color = (color * (6.2f * color + Color3f(0.5f))) / (color * (6.2f * color + Color3f(1.7f)) + Color3f(0.06f));
                        break;

                      assert_otherwise;
                    }

                    color = srgb_to_linear_rgb(color); // FIXME needed to match the output of tonemapper

                    if (m_clamp_values)
                        color = saturate(color);

                    image.set_pixel(x, y, pixel);
                }
            }
        }

      private:
        // TODO add params
        ToneMapOperator   m_operator;
        bool              m_clamp_values;
    };
}


//
// ToneMapPostProcessingStageFactory class implementation.
//

void ToneMapPostProcessingStageFactory::release()
{
    delete this;
}

const char* ToneMapPostProcessingStageFactory::get_model() const
{
    return Model;
}

Dictionary ToneMapPostProcessingStageFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Tone Map");
}

DictionaryArray ToneMapPostProcessingStageFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_input_metadata(metadata);

    metadata.push_back(
        Dictionary()
            .insert("name", "tone_map_operator")
            .insert("label", "Operator")
            // FIXME
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("ACES (Unreal)", "aces_unreal")
                    .insert("Filmic", "filmic"))
            .insert("use", "required")
            .insert("default", "filmic"));
            // .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clamp_values")
            .insert("label", "Clamp Values")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "false"));

    return metadata;
}

auto_release_ptr<PostProcessingStage> ToneMapPostProcessingStageFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<PostProcessingStage>(
        new ToneMapPostProcessingStage(name, params));
}

}   // namespace renderer
