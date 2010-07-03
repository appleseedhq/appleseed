
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_RENDERER_KERNEL_SHADING_SHADINGCONTEXT_H
#define APPLESEED_RENDERER_KERNEL_SHADING_SHADINGCONTEXT_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// Forward declarations.
namespace foundation    { class LightingConditions; }
namespace renderer      { class ILightingEngine; }
namespace renderer      { class Intersector; }
namespace renderer      { class TextureCache; }

namespace renderer
{

//
// Shading context.
//

class ShadingContext
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    ShadingContext(
        const Intersector&                      intersector,
        const SamplingContext&                  sampling_context,
        const foundation::LightingConditions&   lighting_conditions,
        TextureCache&                           texture_cache,
        ILightingEngine&                        lighting_engine);

    // Return the intersector.
    const Intersector& get_intersector() const;

    // Return the sampling context.
    const SamplingContext& get_sampling_context() const;

    // Return the lighting conditions.
    const foundation::LightingConditions& get_lighting_conditions() const;

    // Return the texture cache.
    TextureCache& get_texture_cache() const;

    // Return the light transport solver.
    ILightingEngine& get_lighting_engine() const;

  private:
    const Intersector&                          m_intersector;
    const SamplingContext&                      m_sampling_context;
    const foundation::LightingConditions&       m_lighting_conditions;
    TextureCache&                               m_texture_cache;
    ILightingEngine&                            m_lighting_engine;
};


//
// ShadingContext class implementation.
//

// Constructor.
inline ShadingContext::ShadingContext(
    const Intersector&                          intersector,
    const SamplingContext&                      sampling_context,
    const foundation::LightingConditions&       lighting_conditions,
    TextureCache&                               texture_cache,
    ILightingEngine&                            lighting_engine)
  : m_intersector(intersector)
  , m_sampling_context(sampling_context)
  , m_lighting_conditions(lighting_conditions)
  , m_texture_cache(texture_cache)
  , m_lighting_engine(lighting_engine)
{
}

// Return the intersector.
inline const Intersector& ShadingContext::get_intersector() const
{
    return m_intersector;
}

// Return the sampling context.
inline const SamplingContext& ShadingContext::get_sampling_context() const
{
    return m_sampling_context;
}

// Return the lighting conditions.
inline const foundation::LightingConditions& ShadingContext::get_lighting_conditions() const
{
    return m_lighting_conditions;
}

// Return the texture cache.
inline TextureCache& ShadingContext::get_texture_cache() const
{
    return m_texture_cache;
}

// Return the light transport solver.
inline ILightingEngine& ShadingContext::get_lighting_engine() const
{
    return m_lighting_engine;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_SHADINGCONTEXT_H
