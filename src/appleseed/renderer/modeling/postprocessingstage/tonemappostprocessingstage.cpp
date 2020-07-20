
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

    constexpr const ToneMapOperator AcesNarkowicz       { "ACES (Narkowicz)",       "aces_narkowicz" };
    constexpr const ToneMapOperator AcesUnreal          { "ACES (Unreal)",          "aces_unreal" };
    constexpr const ToneMapOperator FilmicHejl          { "Filmic (Hejl)",          "filmic_hejl" };
    constexpr const ToneMapOperator FilmicUncharted     { "Filmic (Uncharted)",     "filmic_uncharted" };
    constexpr const ToneMapOperator Reinhard            { "Reinhard",               "reinhard" };
    constexpr const ToneMapOperator ReinhardExtended    { "Reinhard (Extended)",    "reinhard_extended" };

    //@Todo add new TMOs
    #define TONE_MAP_OPERATOR_ARRAY {   \
        AcesNarkowicz.id,               \
        AcesUnreal.id,                  \
        FilmicHejl.id,                  \
        FilmicUncharted.id,             \
        Reinhard.id,                    \
        ReinhardExtended.id,            \
    }

    #define INSERT_TONE_MAP_OPERATOR(tmo) insert(tmo.label, tmo.id)

    //@Todo add new TMOs
    #define TONE_MAP_OPERATOR_DICTIONARY Dictionary()   \
        .INSERT_TONE_MAP_OPERATOR(AcesNarkowicz)        \
        .INSERT_TONE_MAP_OPERATOR(AcesUnreal)           \
        .INSERT_TONE_MAP_OPERATOR(FilmicHejl)           \
        .INSERT_TONE_MAP_OPERATOR(FilmicUncharted)      \
        .INSERT_TONE_MAP_OPERATOR(Reinhard)             \
        .INSERT_TONE_MAP_OPERATOR(ReinhardExtended)

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

            const std::string tone_map_operator =
                m_params.get_optional<std::string>(
                    "tone_map_operator",
                    DeafaultToneMapOperatorId,
                    TONE_MAP_OPERATOR_ARRAY,
                    context);

            //@Todo add new TMOs

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
                    m_params.get_optional(
                        "filmic_uncharted_exposure_bias",
                        FilmicUnchartedApplier::DefaultExposureBias,
                        context);

                m_tone_map = new FilmicUnchartedApplier(A, B, C, D, E, F, W, exposure_bias);
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
                // FIXME we shouldn't reach here.. but what if we do?
                m_tone_map = nullptr;
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
            .insert("items", TONE_MAP_OPERATOR_DICTIONARY)
            .insert("use", "required")
            .insert("default", DeafaultToneMapOperatorId)
            .insert("on_change", "rebuild_form"));

    //@Todo add new TMO params

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

    // Filmic (Uncharted)
    {
        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_A",
            "Shoulder strength",
            "0.0", "hard",              // min
            "1.0", "hard",              // max
            "0.15",                     // FilmicUnchartedApplier::DefaultA
            FilmicUncharted.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_B",
            "Linear strength",
            "0.0", "hard",              // min
            "1.0", "hard",              // max
            "0.50",                     // FilmicUnchartedApplier::DefaultB
            FilmicUncharted.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_C",
            "Linear angle",
            "0.0", "hard",              // min
            "1.0", "hard",              // max
            "0.10",                     // FilmicUnchartedApplier::DefaultC
            FilmicUncharted.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_D",
            "Toe strength",
            "0.0", "hard",              // min
            "1.0", "hard",              // max
            "0.20",                     // FilmicUnchartedApplier::DefaultD
            FilmicUncharted.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_E",
            "Toe numerator",
            "0.0", "hard",              // min
            "1.0", "hard",              // max
            "0.02",                     // FilmicUnchartedApplier::DefaultE
            FilmicUncharted.id);

        add_numeric_param_metadata(
            metadata,
            "filmic_uncharted_F",
            "Toe denominator",
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
