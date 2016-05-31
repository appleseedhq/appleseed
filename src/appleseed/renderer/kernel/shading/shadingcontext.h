
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#ifdef APPLESEED_WITH_OSL
#include "renderer/kernel/shading/oslshadergroupexec.h"
#endif

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/vector.h"

// OpenImageIO headers.
#ifdef APPLESEED_WITH_OIIO
#include "foundation/platform/oiioheaderguards.h"
BEGIN_OIIO_INCLUDES
#include "OpenImageIO/texture.h"
END_OIIO_INCLUDES
#endif

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class ILightingEngine; }
namespace renderer  { class Intersector; }
namespace renderer  { class ShadingPoint; }
namespace renderer  { class TextureCache; }
namespace renderer  { class Tracer; }

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
        const Intersector&      intersector,
        Tracer&                 tracer,
        TextureCache&           texture_cache,
#ifdef APPLESEED_WITH_OIIO
        OIIO::TextureSystem&    oiio_texture_system,
#endif
#ifdef APPLESEED_WITH_OSL
        OSLShaderGroupExec&     osl_shadergroup_exec,
#endif
        const size_t            thread_index,
        ILightingEngine*        lighting_engine = 0,
        const float             transparency_threshold = 0.001f,
        const size_t            max_iterations = 1000);

    const Intersector& get_intersector() const;

    Tracer& get_tracer() const;

    TextureCache& get_texture_cache() const;

    ILightingEngine* get_lighting_engine() const;

    // Return the thread id
    size_t get_thread_index() const;

    // Return the minimum transmission value that defines transparency.
    float get_transparency_threshold() const;

    // Return the maximum number of iterations in ray/path tracing loops.
    size_t get_max_iterations() const;

#ifdef APPLESEED_WITH_OIIO
    OIIO::TextureSystem& get_oiio_texture_system() const;
#endif

#ifdef APPLESEED_WITH_OSL
    void execute_osl_shading(
        const ShaderGroup&  shader_group,
        const ShadingPoint& shading_point) const;

    void execute_osl_subsurface(
        const ShaderGroup&  shader_group,
        const ShadingPoint& shading_point) const;

    void execute_osl_transparency(
        const ShaderGroup&  shader_group,
        const ShadingPoint& shading_point,
        Alpha&              alpha,
        float*              holdout = 0) const;

    void execute_osl_emission(
        const ShaderGroup&  shader_group,
        const ShadingPoint& shading_point) const;

    // Execute the OSL shader group, use s to choose
    // one of the closures and set its shading basis in shading point.
    void execute_osl_bump(
        const ShaderGroup&          shader_group,
        const ShadingPoint&         shading_point,
        const foundation::Vector2d& s) const;

    void choose_osl_subsurface_normal(
        const ShadingPoint&             shading_point,
        const void*                     bssrdf_data,
        const double                    s) const;

    void execute_osl_background(
        const ShaderGroup&          shader_group,
        const foundation::Vector3d& outgoing,
        Spectrum&                   value) const;

#endif

  private:
    const Intersector&          m_intersector;
    Tracer&                     m_tracer;
    TextureCache&               m_texture_cache;
    ILightingEngine*            m_lighting_engine;
    const float                 m_transparency_threshold;
    const size_t                m_max_iterations;
    const size_t                m_thread_index;
#ifdef APPLESEED_WITH_OIIO
    OIIO::TextureSystem&        m_oiio_texture_system;
#endif
#ifdef APPLESEED_WITH_OSL
    OSLShaderGroupExec&         m_shadergroup_exec;
#endif
};

inline const Intersector& ShadingContext::get_intersector() const
{
    return m_intersector;
}

inline Tracer& ShadingContext::get_tracer() const
{
    return m_tracer;
}

inline TextureCache& ShadingContext::get_texture_cache() const
{
    return m_texture_cache;
}

inline ILightingEngine* ShadingContext::get_lighting_engine() const
{
    return m_lighting_engine;
}

inline size_t ShadingContext::get_thread_index() const
{
    return m_thread_index;
}

inline float ShadingContext::get_transparency_threshold() const
{
    return m_transparency_threshold;
}

inline size_t ShadingContext::get_max_iterations() const
{
    return m_max_iterations;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_SHADINGCONTEXT_H
