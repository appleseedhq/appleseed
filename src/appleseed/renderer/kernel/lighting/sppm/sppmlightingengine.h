
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMLIGHTINGENGINE_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMLIGHTINGENGINE_H

// appleseed.renderer headers.
#include "renderer/kernel/lighting/sppm/sppmparameters.h"
#include "renderer/kernel/lighting/ilightingengine.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace renderer      { class LightSampler; }
namespace renderer      { class SPPMPassCallback; }

namespace renderer
{

//
// Stochastic Progressive Photon Mapping (SPPM) lighting engine factory.
//

class SPPMLightingEngineFactory
  : public ILightingEngineFactory
{
  public:
    // Constructor.
    SPPMLightingEngineFactory(
        const SPPMPassCallback&     pass_callback,
        const LightSampler&         light_sampler,
        const SPPMParameters&       params);

    // Delete this instance.
    virtual void release() APPLESEED_OVERRIDE;

    // Return a new SPPM lighting engine instance.
    virtual ILightingEngine* create() APPLESEED_OVERRIDE;

    // Get the metadata dictionary describing
    // the SPPM lighting engine params.
    static foundation::Dictionary get_params_metadata();

  private:
    const SPPMParameters            m_params;
    const SPPMPassCallback&         m_pass_callback;
    const LightSampler&             m_light_sampler;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMLIGHTINGENGINE_H
