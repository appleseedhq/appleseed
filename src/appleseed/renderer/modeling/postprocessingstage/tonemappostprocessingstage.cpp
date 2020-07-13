
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
#include "renderer/modeling/postprocessingstage/effect/tonemapapplier.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/conversion.h"
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

    // TODO improve this:
    enum class ToneMapOperator
    {
        ACES_UNREAL,
        FILMIC,
        REINHARD,
    };

    static constexpr ToneMapOperator DeafutToneMapOperator = ToneMapOperator::FILMIC;
    static constexpr float DeafutGamma = 2.2f;

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

            // TODO retrive params
            const std::string tone_map_operator =
                m_params.get_optional<std::string>(
                    "tone_map_operator",
                    "filmic",
                    make_vector("aces_unreal", "filmic", "reinhard"),
                    context);

            if (tone_map_operator == "aces_unreal")
                m_operator = ToneMapOperator::ACES_UNREAL;
            else if (tone_map_operator == "reinhard")
                m_operator = ToneMapOperator::REINHARD;
            else
            {
                assert(tone_map_operator == "filmic");

                m_operator = ToneMapOperator::FILMIC;
            }

            m_gamma = m_params.get_optional<float>("gamma", DeafutGamma);

            return true;
        }

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            const CanvasProperties& props = frame.image().properties();

            Image& image = frame.image();

            switch (m_operator)
            {
              case ToneMapOperator::ACES_UNREAL:
              {
                (const AcesUnrealApplier()).apply_on_tiles(image, thread_count);
                break;
              }

              case ToneMapOperator::FILMIC:
              {
                (const FilmicHejlApplier()).apply_on_tiles(image, thread_count);
                break;
              }

              case ToneMapOperator::REINHARD:
              {
                (const ReinhardApplier(m_gamma)).apply_on_tiles(image, thread_count);
                break;
              }

              assert_otherwise;
            }

            // TODO figure out if this is actually correct (technically)
            // NOTE conversion needed to match the output of tonemapper: https://github.com/tizian/tonemapper
            convert_srgb_to_linear_rgb(image);
        }

      private:
        // TODO add params
        ToneMapOperator     m_operator;
        float               m_gamma;
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
                    .insert("Filmic", "filmic")
                    .insert("Reinhard", "reinhard"))
            .insert("use", "required")
            .insert("default", "filmic")
            .insert("on_change", "rebuild_form"));

    // TODO add operator params:

    metadata.push_back(
        Dictionary()
            .insert("name", "gamma")
            .insert("label", "Gamma")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "10.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "2.2")
            .insert("visible_if",
                Dictionary()
                    .insert("tone_map_operator", "reinhard")));

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
