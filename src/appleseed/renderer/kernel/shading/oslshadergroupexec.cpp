
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
#include "oslshadergroupexec.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/shadergroup/shadergroup.h"

namespace renderer
{

//
// OSLShaderGroupExec class implementation.
//

OSLShaderGroupExec::OSLShaderGroupExec(OSL::ShadingSystem& shading_system)
  : m_osl_shading_system(shading_system)
  , m_osl_thread_info(shading_system.create_thread_info())
  , m_osl_shading_context(shading_system.get_context(m_osl_thread_info))
{
}

OSLShaderGroupExec::~OSLShaderGroupExec()
{
    if (m_osl_shading_context)
        m_osl_shading_system.release_context(m_osl_shading_context);

    if (m_osl_thread_info)
        m_osl_shading_system.destroy_thread_info(m_osl_thread_info);
}

void OSLShaderGroupExec::execute_shading(
    const ShaderGroup&          shader_group,
    const ShadingPoint&         shading_point) const
{
    assert(m_osl_shading_context);
    assert(m_osl_thread_info);

    m_osl_shading_system.execute(
        *m_osl_shading_context,
        *shader_group.shadergroup_ref(),
        shading_point.get_osl_shader_globals());
}

void OSLShaderGroupExec::execute_transparency(
    const ShaderGroup&  shader_group,
    const ShadingPoint& shading_point,
    Alpha&              alpha,
    float*              holdout) const
{
    // Switch temporary the ray type to Shadow.
    ShadingRay::TypeType saved_type = shading_point.m_ray.m_type;
    shading_point.m_ray.m_type = ShadingRay::ShadowRay;

    m_osl_shading_system.execute(
        *m_osl_shading_context,
        *shader_group.shadergroup_ref(),
        shading_point.get_osl_shader_globals());

    process_transparency_tree(shading_point.get_osl_shader_globals().Ci, alpha);

    if (holdout)
        *holdout = process_holdout_tree(shading_point.get_osl_shader_globals().Ci);
    
    // Restore the original ray type.
    shading_point.m_ray.m_type = saved_type;
}

}   // namespace renderer
