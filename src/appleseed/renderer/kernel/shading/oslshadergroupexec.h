
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"
#include "foundation/math/vector.h"

// OSL headers.
#include "foundation/platform/_beginoslheaders.h"
#include "OSL/oslexec.h"
#include "OSL/oslversion.h"
#include "foundation/platform/_endoslheaders.h"

// Forward declarations.
namespace foundation    { class Arena; }
namespace renderer      { class OSLShadingSystem; }
namespace renderer      { class ShaderGroup; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingPoint; }
namespace renderer      { class Tracer; }

namespace renderer
{

class OSLShaderGroupExec
  : public foundation::NonCopyable
{
  public:
    OSLShaderGroupExec(
        OSLShadingSystem&               shading_system,
        foundation::Arena&              arena);

    ~OSLShaderGroupExec();

    foundation::Color3f execute_background(
        const ShaderGroup&              shader_group,
        const foundation::Vector3f&     outgoing) const;

  private:
    friend class ShadingContext;
    friend class Tracer;

    OSLShadingSystem&                   m_osl_shading_system;
    foundation::Arena&                  m_arena;

    OSL::PerThreadInfo*                 m_osl_thread_info;
    OSL::ShadingContext*                m_osl_shading_context;
    char*                               m_osl_mem_pool;
    char*                               m_osl_mem_pool_start;
    mutable size_t                      m_osl_mem_used;

    void execute_shading(
        const ShaderGroup&              shader_group,
        const ShadingPoint&             shading_point) const;

    void execute_subsurface(
        const ShaderGroup&              shader_group,
        const ShadingPoint&             shading_point) const;

    void execute_transparency(
        const ShaderGroup&              shader_group,
        const ShadingPoint&             shading_point,
        Alpha&                          alpha) const;

    void execute_transparency_and_matte(
        const ShaderGroup&              shader_group,
        const ShadingPoint&             shading_point) const;

    void execute_shadow(
        const ShaderGroup&              shader_group,
        const ShadingPoint&             shading_point,
        Alpha&                          alpha) const;

    void execute_emission(
        const ShaderGroup&              shader_group,
        const ShadingPoint&             shading_point) const;

    void execute_bump(
        const ShaderGroup&              shader_group,
        const ShadingPoint&             shading_point,
        const foundation::Vector2f&     s) const;

    void execute_npr(
        const ShaderGroup&              shader_group,
        const ShadingPoint&             shading_point) const;

    void do_execute(
        const ShaderGroup&              shader_group,
        const ShadingPoint&             shading_point,
        const VisibilityFlags::Type     ray_flags) const;

    void choose_bsdf_closure_shading_basis(
        const ShadingPoint&             shading_point,
        const foundation::Vector2f&     s) const;
};

}   // namespace renderer
