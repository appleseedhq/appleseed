
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/modeling/shadergroup/shadergroup.h"

using namespace foundation;

namespace renderer
{

//
// ShadingContext class implementation.
//

ShadingContext::ShadingContext(
    const Intersector&      intersector,
    Tracer&                 tracer,
    TextureCache&           texture_cache,
    OIIO::TextureSystem&    oiio_texture_system,
    OSLShaderGroupExec&     osl_shadergroup_exec,
    Arena&                  arena,
    const size_t            thread_index,
    ILightingEngine*        lighting_engine,
    const float             transparency_threshold,
    const size_t            max_iterations)
  : m_intersector(intersector)
  , m_tracer(tracer)
  , m_texture_cache(texture_cache)
  , m_oiio_texture_system(oiio_texture_system)
  , m_shadergroup_exec(osl_shadergroup_exec)
  , m_arena(arena)
  , m_thread_index(thread_index)
  , m_lighting_engine(lighting_engine)
  , m_transparency_threshold(transparency_threshold)
  , m_max_iterations(max_iterations)
{
}

OIIO::TextureSystem& ShadingContext::get_oiio_texture_system() const
{
    return m_oiio_texture_system;
}

OSL::ShadingSystem& ShadingContext::get_osl_shading_system() const
{
    return m_shadergroup_exec.m_osl_shading_system;
}

OSL::ShadingContext* ShadingContext::get_osl_shading_context() const
{
    return m_shadergroup_exec.m_osl_shading_context;
}

void ShadingContext::execute_osl_shading(
    const ShaderGroup&      shader_group,
    const ShadingPoint&     shading_point) const
{
    m_shadergroup_exec.execute_shading(
        shader_group,
        shading_point);
}

void ShadingContext::execute_osl_subsurface(
    const ShaderGroup&      shader_group,
    const ShadingPoint&     shading_point) const
{
    m_shadergroup_exec.execute_subsurface(
        shader_group,
        shading_point);
}

void ShadingContext::execute_osl_transparency(
    const ShaderGroup&      shader_group,
    const ShadingPoint&     shading_point,
    Alpha&                  alpha,
    float*                  holdout) const
{
    m_shadergroup_exec.execute_transparency(
        shader_group,
        shading_point,
        alpha,
        holdout);
}

void ShadingContext::execute_osl_emission(
    const ShaderGroup&      shader_group,
    const ShadingPoint&     shading_point) const
{
    m_shadergroup_exec.execute_emission(
        shader_group,
        shading_point);
}

void ShadingContext::execute_osl_bump(
    const ShaderGroup&      shader_group,
    const ShadingPoint&     shading_point,
    const Vector2f&         s) const
{
    m_shadergroup_exec.execute_bump(
        shader_group,
        shading_point,
        s);
}

void ShadingContext::execute_osl_background(
    const ShaderGroup&      shader_group,
    const Vector3f&         outgoing,
    Spectrum&               value) const
{
    value =
        m_shadergroup_exec.execute_background(
            shader_group,
            outgoing);
}

void ShadingContext::choose_bsdf_closure_shading_basis(
    const ShadingPoint&     shading_point,
    const Vector2f&         s) const
{
    m_shadergroup_exec.choose_bsdf_closure_shading_basis(shading_point, s);
}

}   // namespace renderer
