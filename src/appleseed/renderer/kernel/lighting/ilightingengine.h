
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/iunknown.h"

// Forward declarations.
namespace foundation    { class StatisticsVector; }
namespace renderer      { class AOVComponents; }
namespace renderer      { class PixelContext; }
namespace renderer      { class ShadingComponents; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingPoint; }
namespace renderer      { class ShadowCatcher; }

namespace renderer
{

//
// Lighting engine interface.
//

class ILightingEngine
  : public foundation::IUnknown
{
  public:
    // Print this component's settings to the renderer's global logger.
    virtual void print_settings() const = 0;

    // Compute the lighting at a given point of the scene.
    virtual void compute_lighting(
        SamplingContext&          sampling_context,
        const PixelContext&       pixel_context,
        const ShadingContext&     shading_context,
        const ShadingPoint&       shading_point,
        ShadingComponents&        radiance,           // output radiance, in W.sr^-1.m^-2
        AOVComponents&            aov_components,
        ShadowCatcher&            shadow_catcher) = 0;

    // Retrieve performance statistics.
    virtual foundation::StatisticsVector get_statistics() const = 0;
};


//
// Interface of a ILightingEngine factory.
//

class ILightingEngineFactory
  : public foundation::IUnknown
{
  public:
    // Return a new sample lighting engine instance.
    virtual ILightingEngine* create() = 0;
};

}   // namespace renderer
