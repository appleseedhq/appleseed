
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
#include <list>
#include <memory>
#include <string>
#include <vector>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Tone map post-processing stage.
    //

    const char* Model = "tone_map_post_processing_stage";

    struct ToneMapOperator
    {
        const char* id;
        const char* name;
    };

    // Tone mapping operators.
    constexpr const ToneMapOperator AcesNarkowicz = { "aces_narkowicz", "ACES (Narkowicz)" };
    constexpr const ToneMapOperator ReinhardExtended = { "reinhard_extended", "Reinhard (Extended)" };

    constexpr const char* DeafaultToneMapOperatorId = AcesNarkowicz.id;

    class ToneMapPostProcessingStage
      : public PostProcessingStage
    {
      public:
        ToneMapPostProcessingStage(
            const char*             name,
            const ParamArray&       params)
          : PostProcessingStage(name, params)
          , m_applier(nullptr)
        {
        }

        void release() override
        {
            // delete m_applier; // FIXME read access violation
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

            const std::string tone_map_operator =
                m_params.get_optional<std::string>(
                    "tone_map_operator",
                    DeafaultToneMapOperatorId,
                    make_vector(AcesNarkowicz.id, ReinhardExtended.id),
                    context);

            if (tone_map_operator == AcesNarkowicz.id)
            {
                const float gamma =
                    m_params.get_optional(
                        "aces_narkowicz_gamma",
                        AcesNarkowiczApplier::DefaultGamma,
                        context);

                m_applier = new AcesNarkowiczApplier(gamma);
            }
            else if (tone_map_operator == ReinhardExtended.id)
            {
                const float gamma =
                    m_params.get_optional(
                        "reinhard_extended_gamma",
                        ReinhardExtendedApplier::DefaultGamma,
                        context);

                const float max_white =
                    m_params.get_optional(
                        "reinhard_extended_max_white",
                        ReinhardExtendedApplier::DefaultMaxWhite,
                        context);

                m_applier = new ReinhardExtendedApplier(gamma, max_white);
            }
            else
            {
                m_applier = nullptr; // FIXME
                assert(false);
            }

            return true;
        }

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            const CanvasProperties& props = frame.image().properties();

            Image& image = frame.image();

            m_applier->apply_on_tiles(image, thread_count);

            // TODO figure out if this is actually correct (technically)
            // NOTE conversion needed to match the output of tonemapper: https://github.com/tizian/tonemapper
            convert_srgb_to_linear_rgb(image);
        }

      private:
        ToneMapApplier*     m_applier;
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
            .insert("type", "enumeration")
            .insert("items",
                    Dictionary()
                        .insert(AcesNarkowicz.name, AcesNarkowicz.id)
                        .insert(ReinhardExtended.name, ReinhardExtended.id))
            .insert("use", "required")
            .insert("default", DeafaultToneMapOperatorId)
            .insert("on_change", "rebuild_form"));

    // ACES (Narkowicz)
    metadata.push_back(
        Dictionary()
            .insert("name", "aces_narkowicz_gamma")
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
            .insert("default", /*AcesNarkowiczApplier::DefaultGamma*/ "2.2")
            .insert("visible_if",
                    Dictionary()
                        .insert("tone_map_operator", AcesNarkowicz.id)));

    // Reinhard (Extended)
    metadata.push_back(
        Dictionary()
            .insert("name", "reinhard_extended_gamma")
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
            .insert("default", /*ReinhardExtendedApplier::DefaultGamma*/ "2.2")
            .insert("visible_if",
                    Dictionary()
                        .insert("tone_map_operator", ReinhardExtended.id)));

    metadata.push_back(
        Dictionary()
            .insert("name", "reinhard_extended_max_white")
            .insert("label", "Lmax")
            .insert("type", "numeric")
            // FIXME min/max luminance values can only
            // be accurately computed at run-time.. :(
            .insert("min",
                    Dictionary()
                        .insert("value", "0.0")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "10000.0")
                        .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", /*ReinhardExtendedApplier::DefaultMaxWhite*/ "1.0")
            .insert("visible_if",
                    Dictionary()
                        .insert("tone_map_operator", ReinhardExtended.id)));

#if 0
    for (const auto& parameter : AcesNarkowiczApplier::Parameters)
    {
        metadata.push_back(
            Dictionary()
                .insert("name", AcesNarkowiczApplier::Operator.id + sep + parameter.id)
                .insert("label", parameter.name)
                // FIXME
                .insert("type", "numeric")
                .insert("min",
                    Dictionary()
                        .insert("value", std::to_string(parameter.min_value))
                        .insert("type", "soft"))
                .insert("max",
                    Dictionary()
                        .insert("value", std::to_string(parameter.max_value))
                        .insert("type", "soft"))
                .insert("use", "optional")
                .insert("default", std::to_string(parameter.default_value))
                .insert("visible_if",
                    Dictionary()
                        .insert("tone_map_operator", AcesNarkowiczApplier::Operator.id)));
    }
    for (const auto& parameter : ReinhardExtendedApplier::Parameters)
    {
        metadata.push_back(
            Dictionary()
                .insert("name", ReinhardExtendedApplier::Operator.id + sep + parameter.id)
                .insert("label", parameter.name)
                // FIXME
                .insert("type", "numeric")
                .insert("min",
                    Dictionary()
                        .insert("value", std::to_string(parameter.min_value))
                        .insert("type", "soft"))
                .insert("max",
                    Dictionary()
                        .insert("value", std::to_string(parameter.max_value))
                        .insert("type", "soft"))
                .insert("use", "optional")
                .insert("default", std::to_string(parameter.default_value))
                .insert("visible_if",
                    Dictionary()
                        .insert("tone_map_operator", ReinhardExtendedApplier::Operator.id)));
    }

    // FIXME quick testing, automate this later..
    metadata.push_back(
        Dictionary()
            .insert("name", "aces_narkowicz_gamma")
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
                    .insert("tone_map_operator", "aces_narkowicz")));
    metadata.push_back(
        Dictionary()
            .insert("name", "reinhard_extended_gamma")
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
                    .insert("tone_map_operator", "reinhard_extended")));

    // FIXME should be computed depending on the image (is it feasible?)
    metadata.push_back(
        Dictionary()
            .insert("name", "reinhard_extended_l_max")
            .insert("label", "Lmax")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "10000.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("visible_if",
                Dictionary()
                    .insert("tone_map_operator", "reinhard_extended")));
#endif

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
