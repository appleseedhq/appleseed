
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

#pragma once

// appleseed.renderer headers.
#include "renderer/modeling/postprocessingstage/ipostprocessingstagefactory.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/job.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Frame; } // TODO remove after moving to new file

namespace renderer
{

//
// A post-processing stage that adds vignetting to the frame.
//

class APPLESEED_DLLSYMBOL VignettePostProcessingStageFactory
  : public IPostProcessingStageFactory
{
  public:
    // Delete this instance.
    void release() override;

    // Return a string identifying this stage model.
    const char* get_model() const override;

    // Return metadata for this stage model.
    foundation::Dictionary get_model_metadata() const override;

    // Return metadata for the inputs of this stage model.
    foundation::DictionaryArray get_input_metadata() const override;

    // Create a new stage instance.
    foundation::auto_release_ptr<PostProcessingStage> create(
        const char*         name,
        const ParamArray&   params) const override;
};

// TODO move what's below to a different file
/*
//
// Vignette effect algorithm.
//

// TODO abstract this into an interface (e.g. IPostProcessingEffect)
class VignetteEffect
  : public foundation::IUnknown
{
  public:
    // Constructor.
    VignetteEffect(
        const float                     intensity,
        const float                     anisotropy,
        const foundation::Vector2f&     resolution,
        const foundation::Vector2f&     normalization_factor
    );

    void release() override;

    // Apply the Vignette post-processing effect to a given tile.
    void apply(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y,
        foundation::IAbortSwitch&   abort_switch) const;

  private:
    const float                     m_intensity;
    const float                     m_anisotropy;
    const foundation::Vector2f&     m_resolution;
    const foundation::Vector2f&     m_normalization_factor;
};

//
// Vignette effect algorithm applier job.
//

// TODO abstract this into an interface (e.g. IPostProcessingEffectJob)
class VignetteJob
  : public foundation::IJob
{
  public:
    typedef std::vector<VignetteEffect*> EffectApplierVector;

    // Constructor.
    VignetteJob(
        const Frame&                    frame,
        const size_t                    tile_x,
        const size_t                    tile_y,
        const size_t                    thread_count,
        foundation::IAbortSwitch&       abort_switch,
        // TODO refactor this to use a ParamArray for effect-specific context and settings
        const float                     intensity,
        const float                     anisotropy,
        const foundation::Vector2f&     resolution,
        const foundation::Vector2f&     normalization_factor);

    // Execute the job.
    void execute(const size_t thread_index) override;

  private:
    const VignetteEffect*           m_effect_applier;
    const Frame&                    m_frame;
    const size_t                    m_tile_x;
    const size_t                    m_tile_y;
    const size_t                    m_thread_count;
    foundation::IAbortSwitch&       m_abort_switch;
};
*/
}   // namespace renderer
