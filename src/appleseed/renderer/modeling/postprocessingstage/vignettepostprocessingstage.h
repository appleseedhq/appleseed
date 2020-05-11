
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
#include "foundation/utility/job.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Frame; } // FIXME

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

//
// Vignette effect algorithm.
//

// TODO abstract this into an interface (e.g. IPostProcessingEffect)
class VignetteEffect
  : public foundation::IUnknown
{
  public:
    // Apply this post-processing effect on a given tile.
    void apply(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y,
        // TODO check if pass_hash is needed
        foundation::IAbortSwitch&   abort_switch);

  private:
    // TODO check if using Vector2f& is worth it
    const float m_intensity;
    const float m_anisotropy;
    const Vector2f m_resolution;
    const Vector2f m_normalization_factor;
};

//
// Vignette effect applier job.
//

class VignetteJob
  : public foundation::IJob
{
  public:
    typedef std::vector<VignetteEffect*> EffectApplierVector;

    // Constructor.
    VignetteJob(
        const EffectApplierVector&  effect_appliers,
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y,
        const size_t                thread_count,
        // TODO check if pass_hash is needed
        // TODO add a ParamArray for effect-specific context
        foundation::IAbortSwitch&   abort_switch);

    // Execute the job.
    void execute(const size_t thread_index) override;

  private:
    const EffectApplierVector&      m_effect_appliers;
    const Frame&                    m_frame;
    const size_t                    m_tile_x;
    const size_t                    m_tile_y;
    const size_t                    m_thread_count;
    foundation::IAbortSwitch&       m_abort_switch;
};

}   // namespace renderer
