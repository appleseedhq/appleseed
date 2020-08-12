
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
#include "renderer/modeling/postprocessingstage/effect/clampcolorsapplier.h"
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

    constexpr const ToneMapOperator Linear              { "Linear",                 "linear" };
    constexpr const ToneMapOperator AcesNarkowicz       { "ACES (Narkowicz)",       "aces_narkowicz" };
    constexpr const ToneMapOperator AcesUnreal          { "ACES (Unreal)",          "aces_unreal" };
    constexpr const ToneMapOperator FilmicHejl          { "Filmic (Hejl)",          "filmic_hejl" };
    constexpr const ToneMapOperator FilmicPiecewise     { "Filmic (Piecewise)",     "filmic_piecewise" };
    constexpr const ToneMapOperator FilmicUncharted     { "Filmic (Uncharted)",     "filmic_uncharted" };
    constexpr const ToneMapOperator Reinhard            { "Reinhard",               "reinhard" };
    constexpr const ToneMapOperator ReinhardExtended    { "Reinhard (Extended)",    "reinhard_extended" };

    #define TONE_MAP_OPERATOR_ARRAY {   \
        Linear.id,                      \
        AcesNarkowicz.id,               \
        AcesUnreal.id,                  \
        FilmicHejl.id,                  \
        FilmicPiecewise.id,             \
        FilmicUncharted.id,             \
        Reinhard.id,                    \
        ReinhardExtended.id,            \
    }

    #define INSERT_TONE_MAP_OPERATOR(tmo) insert(tmo.label, tmo.id)

    #define TONE_MAP_OPERATOR_DICTIONARY Dictionary()   \
        .INSERT_TONE_MAP_OPERATOR(Linear)               \
        .INSERT_TONE_MAP_OPERATOR(AcesNarkowicz)        \
        .INSERT_TONE_MAP_OPERATOR(AcesUnreal)           \
        .INSERT_TONE_MAP_OPERATOR(FilmicHejl)           \
        .INSERT_TONE_MAP_OPERATOR(FilmicPiecewise)      \
        .INSERT_TONE_MAP_OPERATOR(FilmicUncharted)      \
        .INSERT_TONE_MAP_OPERATOR(Reinhard)             \
        .INSERT_TONE_MAP_OPERATOR(ReinhardExtended)

    //
    // Tone map post-processing stage.
    //

    const char* Model = "tone_map_post_processing_stage";

    constexpr const char* DeafaultToneMapOperatorId = Linear.id;
    constexpr bool DeafaultClampColors = true;

    class ToneMapPostProcessingStage
      : public PostProcessingStage
    {
      public:
        ToneMapPostProcessingStage(
            const char*             name,
            const ParamArray&       params)
          : PostProcessingStage(name, params)
          , m_tone_map(nullptr)
        {
        }

        void release() override
        {
            if (m_tone_map != nullptr)
                delete m_tone_map;

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

            m_clamp_colors = m_params.get_optional("clamp_colors", DeafaultClampColors, context);

            const std::string tone_map_operator =
                m_params.get_optional<std::string>(
                    "tone_map_operator",
                    DeafaultToneMapOperatorId,
                    TONE_MAP_OPERATOR_ARRAY,
                    context);

            // Initialize the tone map applier.
            if (tone_map_operator == AcesNarkowicz.id)
            {
                const float exposure_bias =
                    m_params.get_optional("aces_narkowicz_exposure_bias", AcesNarkowiczApplier::DefaultExposureBias, context);

                m_tone_map = new AcesNarkowiczApplier(exposure_bias);
            }
            else if (tone_map_operator == AcesUnreal.id)
            {
                m_tone_map = new AcesUnrealApplier();
            }
            else if (tone_map_operator == FilmicHejl.id)
            {
                m_tone_map = new FilmicHejlApplier();
            }
            else if (tone_map_operator == FilmicUncharted.id)
            {
                const float A =
                    m_params.get_optional("filmic_uncharted_A", FilmicUnchartedApplier::DefaultA, context);
                const float B =
                    m_params.get_optional("filmic_uncharted_B", FilmicUnchartedApplier::DefaultB, context);
                const float C =
                    m_params.get_optional("filmic_uncharted_C", FilmicUnchartedApplier::DefaultC, context);
                const float D =
                    m_params.get_optional("filmic_uncharted_D", FilmicUnchartedApplier::DefaultD, context);
                const float E =
                    m_params.get_optional("filmic_uncharted_E", FilmicUnchartedApplier::DefaultE, context);
                const float F =
                    m_params.get_optional("filmic_uncharted_F", FilmicUnchartedApplier::DefaultF, context);
                const float W =
                    m_params.get_optional("filmic_uncharted_W", FilmicUnchartedApplier::DefaultW, context);
                const float exposure_bias =
                    m_params.get_optional("filmic_uncharted_exposure_bias", FilmicUnchartedApplier::DefaultExposureBias, context);

                m_tone_map = new FilmicUnchartedApplier(A, B, C, D, E, F, W, exposure_bias);
            }
            else if (tone_map_operator == FilmicPiecewise.id)
            {
                const float toe_strength =
                    m_params.get_optional("filmic_piecewise_toe_strength", FilmicPiecewiseApplier::DefaultToeStrength, context);
                const float toe_length =
                    m_params.get_optional("filmic_piecewise_toe_length", FilmicPiecewiseApplier::DefaultToeLength, context);
                const float shoulder_strength =
                    m_params.get_optional("filmic_piecewise_shoulder_strength", FilmicPiecewiseApplier::DefaultShoulderStrength, context);
                const float shoulder_length =
                    m_params.get_optional("filmic_piecewise_shoulder_length", FilmicPiecewiseApplier::DefaultShoulderLength, context);
                const float shoulder_angle =
                    m_params.get_optional("filmic_piecewise_shoulder_angle", FilmicPiecewiseApplier::DefaultShoulderAngle, context);

                m_tone_map = new FilmicPiecewiseApplier(toe_strength, toe_length, shoulder_strength, shoulder_length, shoulder_angle);
            }
            else if (tone_map_operator == Reinhard.id)
            {
                const bool use_luminance =
                    m_params.get_optional("reinhard_use_luminance", ReinhardApplier::DefaultUseLuminance, context);

                m_tone_map = new ReinhardApplier(use_luminance);
            }
            else if (tone_map_operator == ReinhardExtended.id)
            {
                const float max_white =
                    m_params.get_optional("reinhard_extended_max_white", ReinhardExtendedApplier::DefaultMaxWhite, context);
                const bool use_luminance =
                    m_params.get_optional("reinhard_extended_use_luminance", ReinhardExtendedApplier::DefaultUseLuminance, context);

                m_tone_map = new ReinhardExtendedApplier(max_white, use_luminance);
            }
            else
            {
                assert(tone_map_operator == Linear.id);

                m_tone_map = new LinearApplier();
            }

            return true;
        }

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            Image& image = frame.image();

            // Apply the selected tone mapping operator to each image tile, in parallel.
            m_tone_map->apply_on_tiles(image, thread_count);

            // Clamp colors to LDR range [0, 1].
            if (m_clamp_colors)
            {
                const ClampColorsApplier clamp_colors;
                clamp_colors.apply_on_tiles(image, thread_count);
            }

        }

      private:
        ToneMapApplier*     m_tone_map;
        bool                m_clamp_colors;
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

    inline void add_boolean_param_metadata(
        DictionaryArray&    metadata,
        const char*         name,
        const char*         label,
        const char*         default_value,
        const char*         tone_map_operator_id)
    {
        metadata.push_back(
            Dictionary()
                .insert("name", name)
                .insert("label", label)
                .insert("type", "boolean")
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
            .insert("items", TONE_MAP_OPERATOR_DICTIONARY)
            .insert("use", "required")
            .insert("default", DeafaultToneMapOperatorId)
            .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clamp_colors")
            .insert("label", "Clamp Colors")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "true"));

    // ACES (Narkowicz)
    {
        add_numeric_param_metadata(
            metadata,
            "aces_narkowicz_exposure_bias",
            "Exposure bias",
            "0.0", "hard",              // min
            "10.0", "hard",             // max
            "0.8",                      // AcesNarkowiczApplier::DefaultExposureBias
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

    // Filmic (Uncharted)
    {
        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_A",
            "Shoulder Strength (A)",
            "0.0", "hard",              // min
            "1.0", "hard",              // max
            "0.22",                     // FilmicUnchartedApplier::DefaultA
            FilmicUncharted.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_B",
            "Linear Strength (B)",
            "0.0", "hard",              // min
            "1.0", "hard",              // max
            "0.30",                     // FilmicUnchartedApplier::DefaultB
            FilmicUncharted.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_C",
            "Linear Angle (C)",
            "0.0", "hard",              // min
            "1.0", "hard",              // max
            "0.10",                     // FilmicUnchartedApplier::DefaultC
            FilmicUncharted.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_D",
            "Toe Strength (D)",
            "0.0", "hard",              // min
            "1.0", "hard",              // max
            "0.20",                     // FilmicUnchartedApplier::DefaultD
            FilmicUncharted.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_E",
            "Toe Numerator (E)",
            "0.0", "hard",              // min
            "1.0", "hard",              // max
            "0.01",                     // FilmicUnchartedApplier::DefaultE
            FilmicUncharted.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_F",
            "Toe Denominator (F)",
            "0.0", "hard",              // min
            "1.0", "hard",              // max
            "0.30",                     // FilmicUnchartedApplier::DefaultF
            FilmicUncharted.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_W",
            "Linear white point",
            "0.0", "hard",              // min
            "1.0", "hard",              // max
            "11.2",                     // FilmicUnchartedApplier::DefaultW
            FilmicUncharted.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_exposure_bias",
            "Exposure bias",
            "0.0", "hard",              // min
            "10.0", "hard",             // max
            "2.0",                      // FilmicUnchartedApplier::DefaultExposureBias
            FilmicUncharted.id);
    }

    // Filmic (Piecewise)
    {
        add_numeric_param_metadata(
            metadata,
            "filmic_piecewise_toe_strength",
            "Toe Strength",
            "0.0", "soft",              // min
            "1.0", "soft",              // max
            "0.0",                      // FilmicPiecewiseApplier::DefaultToeStrength
            FilmicPiecewise.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_piecewise_toe_length",
            "Toe Length",
            "0.0", "soft",              // min
            "1.0", "soft",              // max
            "0.5",                      // FilmicPiecewiseApplier::DefaultToeLength
            FilmicPiecewise.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_piecewise_shoulder_strength",
            "Shoulder Strength",
            "0.0", "soft",              // min
            "1.0", "soft",              // max
            "0.0",                      // FilmicPiecewiseApplier::DefaultShoulderStrength
            FilmicPiecewise.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_piecewise_shoulder_length",
            "Shoulder Length (F-stops)",
            "0.00001", "hard",          // min
            "32.0", "soft",             // max
            "0.5",                      // FilmicPiecewiseApplier::DefaultShoulderLength
            FilmicPiecewise.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_piecewise_shoulder_angle",
            "Shoulder Angle",
            "0.0", "soft",              // min
            "1.0", "soft",              // max
            "0.0",                      // FilmicPiecewiseApplier::DefaultShoulderAngle
            FilmicPiecewise.id);
    }

    // Reinhard
    {
        add_boolean_param_metadata(
            metadata,
            "reinhard_use_luminance",
            "Use Luminance",
            "true",                     // ReinhardApplier::DefaultUseLuminance
            Reinhard.id);
    }

    // Reinhard (Extended)
    {
        add_numeric_param_metadata(
            metadata,
            "reinhard_extended_max_white",
            "Max White",

            // FIXME min/max luminance values can only
            // be accurately computed at run-time.. :(
            "0.0", "hard",              // min
            "10000.0", "soft",          // max

            "1.0",                      // ReinhardExtendedApplier::DefaultMaxWhite
            ReinhardExtended.id);

        add_boolean_param_metadata(
            metadata,
            "reinhard_extended_use_luminance",
            "Use Luminance",
            "true",                     // ReinhardExtendedApplier::DefaultUseLuminance
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
