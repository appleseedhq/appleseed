
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
// #include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/job/ijob.h"
// #include "foundation/utility/job/jobmanager.h"
#include "foundation/utility/job/jobqueue.h"
// #include "foundation/utility/job/workerthread.h"

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

            // FIXME get the value of thread_count

            // TODO schedule on VignetteJob per tile and start a WorkerThread with them
            Logger l;
            JobQueue job_queue;
            JobManager job_manager(l, job_queue, 1);
            AbortSwitch s;
            job_queue.schedule(
                new VignetteJob(
                    frame, 0, 0, 1, s,
                    // effect settings/context
                    m_intensity,
                    m_anisotropy,
                    resolution,
                    normalization_factor));
            job_manager.start();
            job_queue.wait_until_completion();
        }

      private:
        float m_intensity;
        float m_anisotropy;
    };
}

//
// VignetteEffect class implementation.
//


VignetteEffect::VignetteEffect(
    const float                     intensity,
    const float                     anisotropy,
    const foundation::Vector2f&     resolution,
    const foundation::Vector2f&     normalization_factor)
  : m_intensity(intensity)
  , m_anisotropy(anisotropy)
  , m_resolution(resolution)
  , m_normalization_factor(normalization_factor)
{
}

void VignetteEffect::apply(
    const Frame&                frame,
    const size_t                tile_x,
    const size_t                tile_y,
    foundation::IAbortSwitch&   abort_switch) const
{
    Image& image = frame.image();

    assert(tile_x < image.properties().m_tile_count_x);
    assert(tile_y < image.properties().m_tile_count_y);

    Tile& tile = image.tile(tile_x, tile_y);
    const size_t tile_width = tile.get_width();
    const size_t tile_height = tile.get_height();

    for (size_t y = 0; y < tile_height; ++y)
    {
        for (size_t x = 0; x < tile_width; ++x)
        {
            const size_t tile_offset_x = tile_x * image.properties().m_tile_width;
            const size_t tile_offset_y = tile_y * image.properties().m_tile_height;
            const Vector2f pixel_coord = Vector2f(static_cast<float>(tile_offset_x + x), static_cast<float>(tile_offset_y + y));

            // Pixel coordinate normalized to be in the [-1, 1] range vertically.
            const Vector2f coord = (2.0f * pixel_coord - m_resolution) / m_normalization_factor;

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
            tile.get_pixel(x, y, pixel);
            pixel.rgb() *= inverse_biquadratic_radial_falloff;
            tile.set_pixel(x, y, Color4f(1.0f, 0.0f, 1.0f, 1.0f));//pixel);
        }
    }
}

void VignetteEffect::release()
{
}

//
// VignetteJob class implementation.
//

VignetteJob::VignetteJob(
    const Frame&                frame,
    const size_t                tile_x,
    const size_t                tile_y,
    const size_t                thread_count,
    foundation::IAbortSwitch&   abort_switch,
    const float                 intensity,
    const float                 anisotropy,
    const foundation::Vector2f& resolution,
    const foundation::Vector2f& normalization_factor)
  : m_frame(frame)
  , m_tile_x(tile_x)
  , m_tile_y(tile_y)
  , m_thread_count(thread_count)
  , m_abort_switch(abort_switch)
{
    m_effect_applier = new VignetteEffect(intensity, anisotropy, resolution, normalization_factor);
}

void VignetteJob::execute(const size_t thread_index)
{
    // Apply the Vignette post-processing effect to the tile.
    //assert(thread_index < m_effect_appliers.size());
    //m_effect_appliers[thread_index]
    m_effect_applier->apply(
        m_frame,
        m_tile_x,
        m_tile_y,
        m_abort_switch);
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
