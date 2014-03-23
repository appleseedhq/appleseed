
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

#ifndef APPLESEED_RENDERER_KERNEL_SHADING_OSLSHADERGROUPEXEC_H
#define APPLESEED_RENDERER_KERNEL_SHADING_OSLSHADERGROUPEXEC_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// OSL headers.
#include "OSL/oslexec.h"

// Forward declarations.
namespace renderer  { class ShaderGroup; }
namespace renderer  { class ShadingPoint; }

namespace renderer
{

//
// OSLShaderGroupExec class.
//

class OSLShaderGroupExec
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    explicit OSLShaderGroupExec(OSL::ShadingSystem& shading_system);

    // Destructor.
    ~OSLShaderGroupExec();

    void execute_shading(
        const ShaderGroup&      shader_group, 
        const ShadingPoint&     shading_point) const;

    void execute_transparency(
        const ShaderGroup&      shader_group,
        const ShadingPoint&     shading_point,
        Alpha&                  alpha,
        float*                  holdout = 0) const;

  private:
    OSL::ShadingSystem&         m_osl_shading_system;
    OSL::PerThreadInfo*         m_osl_thread_info;
    OSL::ShadingContext*        m_osl_shading_context;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_OSLSHADERGROUPEXEC_H
