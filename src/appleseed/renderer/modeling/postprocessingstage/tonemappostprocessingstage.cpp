
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
#include <string>
#include <vector>

using namespace foundation;

namespace renderer
{

namespace
{

    //
    // Tone mapping operators.
    //

    struct ToneMapOperator
    {
        const char* label;
        const char* id;
    };

    constexpr const ToneMapOperator AcesNarkowicz { "ACES (Narkowicz)", "aces_narkowicz" };
    constexpr const ToneMapOperator AcesUnreal { "ACES (Unreal)", "aces_unreal" };
    constexpr const ToneMapOperator FilmicHejl { "Filmic (Hejl)", "filmic_hejl" };
    constexpr const ToneMapOperator Reinhard { "Reinhard", "reinhard" };
    constexpr const ToneMapOperator ReinhardExtended { "Reinhard (Extended)", "reinhard_extended" };

    //
    // Tone map post-processing stage.
    //

    const char* Model = "tone_map_post_processing_stage";

    constexpr const char* DeafaultToneMapOperatorId = AcesNarkowicz.id;

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
            // delete m_tone_map; // FIXME read access violation
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
                    make_vector(
                        AcesNarkowicz.id,
                        AcesUnreal.id,
                        FilmicHejl.id,
                        Reinhard.id,
                        ReinhardExtended.id),
                    context);

            // Initialize the tone map applier.
            if (tone_map_operator == AcesNarkowicz.id)
            {
                const float gamma =
                    m_params.get_optional(
                        "aces_narkowicz_gamma",
                        AcesNarkowiczApplier::DefaultGamma,
                        context);

                m_tone_map = new AcesNarkowiczApplier(gamma);
            }
            else if (tone_map_operator == AcesUnreal.id)
            {
                m_tone_map = new AcesUnrealApplier();
            }
            else if (tone_map_operator == FilmicHejl.id)
            {
                m_tone_map = new FilmicHejlApplier();
            }
            else if (tone_map_operator == Reinhard.id)
            {
                const float gamma =
                    m_params.get_optional(
                        "reinhard_gamma",
                        ReinhardApplier::DefaultGamma,
                        context);

                m_tone_map = new ReinhardApplier(gamma);
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

                m_tone_map = new ReinhardExtendedApplier(gamma, max_white);
            }
            else
            {
                m_tone_map = nullptr; // FIXME
                assert(false);
            }

            return true;
        }

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            const CanvasProperties& props = frame.image().properties();

            Image& image = frame.image();

            m_tone_map->apply_on_tiles(image, thread_count);

            // TODO figure out if this is actually correct (technically)
            // NOTE conversion needed to match the output of tonemapper: https://github.com/tizian/tonemapper
            convert_srgb_to_linear_rgb(image);
        }

      private:
        ToneMapApplier*     m_tone_map;
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

namespace
{
    inline void add_numeric_param_metadata(
        DictionaryArray&    metadata,
        const char*         name,
        const char*         label,
        const char*         min_value,
        const char*         min_type,
        const char*         max_value,
        const char*         max_type,
        const char*         default_value,
        const char*         tone_map_operator_id)
    {
        metadata.push_back(
            Dictionary()
                .insert("name", name)
                .insert("label", label)
                .insert("type", "numeric")
                .insert("min",
                        Dictionary()
                            .insert("value", min_value)
                            .insert("type", min_type))
                .insert("max",
                        Dictionary()
                            .insert("value", max_value)
                            .insert("type", max_type))
                .insert("use", "optional")
                .insert("default", default_value)
                .insert("visible_if",
                        Dictionary()
                            .insert("tone_map_operator", tone_map_operator_id)));
    }
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
                        .insert(AcesNarkowicz.label, AcesNarkowicz.id)
                        .insert(AcesUnreal.label, AcesUnreal.id)
                        .insert(FilmicHejl.label, FilmicHejl.id)
                        .insert(Reinhard.label, Reinhard.id)
                        .insert(ReinhardExtended.label, ReinhardExtended.id))
            .insert("use", "required")
            .insert("default", DeafaultToneMapOperatorId)
            .insert("on_change", "rebuild_form"));

    // ACES (Narkowicz)
    {
        add_numeric_param_metadata(
            metadata,
            "aces_narkowicz_gamma",
            "Gamma",
            "0.0", "soft",              // min
            "10.0", "soft",             // max
            "2.2",                      // AcesNarkowiczApplier::DefaultGamma
            AcesNarkowicz.id);
    }

    // ACES (Unreal)
    {
        // No parameters.
    }

    // Filmic (Hejl)
    {
        // No parameters.
    }

    // Reinhard
    {
        add_numeric_param_metadata(
            metadata,
            "reinhard_gamma",
            "Gamma",
            "0.0", "soft",              // min
            "10.0", "soft",             // max
            "2.2",                      // ReinhardApplier::DefaultGamma
            Reinhard.id);
    }

    // Reinhard (Extended)
    {
        add_numeric_param_metadata(
            metadata,
            "reinhard_extended_gamma",
            "Gamma",
            "0.0", "soft",              // min
            "10.0", "soft",             // max
            "2.2",                      // ReinhardExtendedApplier::DefaultGamma
            ReinhardExtended.id);

        add_numeric_param_metadata(
            metadata,
            "reinhard_extended_max_white",
            "Lmax",

            // FIXME min/max luminance values can only
            // be accurately computed at run-time.. :(
            "0.0", "hard",              // min
            "10000.0", "soft",          // max

            "1.0",                      // ReinhardExtendedApplier::DefaultMaxWhite
            ReinhardExtended.id);
    }

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
