
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Aytek Aman, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_BDPT_BDPTLIGHTINGENGINE_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_BDPT_BDPTLIGHTINGENGINE_H

// appleseed.renderer headers.
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/population.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace renderer      { class ForwardLightSampler; }
namespace renderer      { class Project; }

namespace renderer
{

//
// Bidirectional path tracing lighting engine factory.
//

class BDPTLightingEngineFactory
  : public ILightingEngineFactory
{
  public:
    // Constructor.
    BDPTLightingEngineFactory(
        const Project&              project,
        const ForwardLightSampler&  light_sampler,
        const ParamArray&           params);

    // Delete this instance.
    void release() override;

    // Return a new path tracing lighting engine instance.
    ILightingEngine* create() override;

    // Return the metadata of the BDPT lighting engine parameters.
    static foundation::Dictionary get_params_metadata();

  private:
    const Project&              m_project;
    const ForwardLightSampler&  m_light_sampler;
    ParamArray                  m_params;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_BDPT_BDPTLIGHTINGENGINE_H
