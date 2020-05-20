
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
#include "renderer/global/globallogger.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/postprocessingstage/postprocessingstage.h"
#include "renderer/modeling/postprocessingstage/effect/postprocessingeffectjob.h"
#include "renderer/modeling/postprocessingstage/effect/vignettepostprocessingeffect.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/job/ijob.h"
#include "foundation/utility/job/jobmanager.h"
#include "foundation/utility/job/jobqueue.h"

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

        void execute(Frame& frame, const std::size_t thread_count) const override
        {
            // Skip vignetting if the intensity is zero.
            if (feq(m_intensity, 0.0f))
                return;

            const CanvasProperties& props = frame.image().properties();

            // Initialize effect-specific settings and context.
            VignetteParams effect_params {
                m_intensity,
                m_anisotropy,
                static_cast<float>(props.m_canvas_width),
                static_cast<float>(props.m_canvas_height)
            };

            // Instantiate effect appliers, one per rendering thread.
            VignetteApplierFactory effect_applier_factory(effect_params);
            EffectJob::EffectApplierVector effect_appliers;

            effect_appliers.reserve(thread_count);
            for (std::size_t i = 0; i < thread_count; ++i)
                effect_appliers.push_back(effect_applier_factory.create());

            // Create effect applier jobs.
            EffectJobFactory effect_job_factory;
            EffectJobFactory::EffectJobVector effect_jobs = effect_job_factory.create(
                                                                frame,
                                                                effect_appliers);

            // Schedule effect applier jobs.
            JobQueue job_queue;
            for (const auto effect_job : effect_jobs)
                job_queue.schedule(effect_job);

            // Create a job manager to wait until jobs have effectively stopped.
            JobManager job_manager(
                global_logger(),
                job_queue,
                thread_count);

            job_manager.start();
            job_queue.wait_until_completion();
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
