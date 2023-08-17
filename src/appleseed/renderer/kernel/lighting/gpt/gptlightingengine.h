
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Stephen Agyemang, The appleseedhq Organization
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
#include "renderer/kernel/lighting/gpt/gptparameters.h"
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/lighting/sdtree.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace renderer      { class BackwardLightSampler; }
namespace renderer      { class LightPathRecorder; }

namespace renderer
{

//
// Implementation of "Practical Path Guiding for Efficient Light-Transport Simulation" [Müller et al. 2017].
//

class GPTLightingEngineFactory
  : public ILightingEngineFactory
{
  public:
    // Return parameters metadata.
    static foundation::Dictionary get_params_metadata();

    // Constructor.
    GPTLightingEngineFactory(
        STree*                          sd_tree,
        const BackwardLightSampler&     light_sampler,
        LightPathRecorder&              light_path_recorder,
        const GPTParameters&            params);

    // Delete this instance.
    void release() override;

    // Return a new path tracing lighting engine instance.
    ILightingEngine* create() override;

  private:
    STree*                              m_sd_tree;
    const BackwardLightSampler&         m_light_sampler;
    LightPathRecorder&                  m_light_path_recorder;
    GPTParameters                       m_params;
};

}   // namespace renderer
