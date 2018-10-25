
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_IGPULIGHTINGENGINE_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_IGPULIGHTINGENGINE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/iunknown.h"

// Forward declarations.
namespace foundation    { class StatisticsVector; }

namespace renderer
{

//
// GPU lighting engine interface.
//

class IGPULightingEngine
  : public foundation::IUnknown
{
  public:
    // Print this component's settings to the renderer's global logger.
    virtual void print_settings() const = 0;

    // Retrieve performance statistics.
    virtual foundation::StatisticsVector get_statistics() const = 0;
};


//
// Interface of a IGPULightingEngine factory.
//

class IGPULightingEngineFactory
  : public foundation::IUnknown
{
  public:
    // Return a new sample lighting engine instance.
    virtual IGPULightingEngine* create() = 0;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_IGPULIGHTINGENGINE_H
