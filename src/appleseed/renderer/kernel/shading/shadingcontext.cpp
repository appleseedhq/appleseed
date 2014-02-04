
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "shadingcontext.h"

namespace renderer
{

//
// ShadingContext class implementation.
//

ShadingContext::ShadingContext(
    const Intersector&          intersector,
    Tracer&                     tracer,
    TextureCache&               texture_cache,
#ifdef WITH_OSL
    OSL::ShadingSystem&         shading_system,
#endif
    ILightingEngine*            lighting_engine,
    const float                 transparency_threshold,
    const size_t                max_iterations)
  : m_intersector(intersector)
  , m_tracer(tracer)
  , m_texture_cache(texture_cache)
  , m_lighting_engine(lighting_engine)
  , m_transparency_threshold(transparency_threshold)
  , m_max_iterations(max_iterations)
#ifdef WITH_OSL
  , m_osl_shading_system(shading_system)
#endif
{
#ifdef WITH_OSL
    m_osl_thread_info = m_osl_shading_system.create_thread_info();
    m_osl_shading_context = m_osl_shading_system.get_context(m_osl_thread_info);
#endif
}

ShadingContext::~ShadingContext()
{
#ifdef WITH_OSL
    if (m_osl_shading_context)
        m_osl_shading_system.release_context(m_osl_shading_context);

    if (m_osl_thread_info)
        m_osl_shading_system.destroy_thread_info(m_osl_thread_info);
#endif
}

}       // namespace renderer
