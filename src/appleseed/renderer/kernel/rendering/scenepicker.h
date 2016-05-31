
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_SCENEPICKER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_SCENEPICKER_H

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class AssemblyInstance; }
namespace renderer  { class BSDF; }
namespace renderer  { class BSSRDF; }
namespace renderer  { class Camera; }
namespace renderer  { class EDF; }
namespace renderer  { class Material; }
namespace renderer  { class Object; }
namespace renderer  { class ObjectInstance; }
namespace renderer  { class SurfaceShader; }
namespace renderer  { class TraceContext; }

namespace renderer
{

class APPLESEED_DLLSYMBOL ScenePicker
{
  public:
    struct PickingResult
    {
        bool                            m_hit;
        foundation::Vector3d            m_point;                // world space
        double                          m_distance;             // world space
        ShadingPoint::PrimitiveType     m_primitive_type;
        foundation::Transformd          m_assembly_instance_transform;

        const Camera*                   m_camera;
        const AssemblyInstance*         m_assembly_instance;
        const Assembly*                 m_assembly;
        const ObjectInstance*           m_object_instance;
        const Object*                   m_object;
        const Material*                 m_material;
        const SurfaceShader*            m_surface_shader;
        const BSDF*                     m_bsdf;
        const BSSRDF*                   m_bssrdf;
        const EDF*                      m_edf;
    };

    explicit ScenePicker(const TraceContext& trace_context);

    ~ScenePicker();

    PickingResult pick(const foundation::Vector2d& ndc) const;

  private:
    struct Impl;
    Impl* impl;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_SCENEPICKER_H
