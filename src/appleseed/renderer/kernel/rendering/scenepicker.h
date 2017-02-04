
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_SCENEPICKER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_SCENEPICKER_H

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/scene/objectinstance.h"

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
namespace renderer  { class Project; }
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
        ShadingPoint::PrimitiveType     m_primitive_type;
        double                          m_distance;                     // world space

        foundation::Vector2f            m_bary;
        foundation::Vector2f            m_uv;
        foundation::Vector2f            m_duvdx;
        foundation::Vector2f            m_duvdy;
        foundation::Vector3d            m_point;                        // world space
        foundation::Vector3d            m_dpdu;                         // world space
        foundation::Vector3d            m_dpdv;                         // world space
        foundation::Vector3d            m_dndu;                         // world space
        foundation::Vector3d            m_dndv;                         // world space
        foundation::Vector3d            m_dpdx;                         // world space
        foundation::Vector3d            m_dpdy;                         // world space
        foundation::Vector3d            m_geometric_normal;             // world space
        foundation::Vector3d            m_original_shading_normal;      // world space
        ObjectInstance::Side            m_side;

        // Note: the (possibly perturbed) shading normal is excluded from picking results
        // because it requires the normal modifier which is only available during rendering.

        const Camera*                   m_camera;
        const AssemblyInstance*         m_assembly_instance;
        foundation::Transformd          m_assembly_instance_transform;
        const Assembly*                 m_assembly;
        const ObjectInstance*           m_object_instance;
        const Object*                   m_object;
        const Material*                 m_material;
        const SurfaceShader*            m_surface_shader;
        const BSDF*                     m_bsdf;
        const BSSRDF*                   m_bssrdf;
        const EDF*                      m_edf;
    };

    explicit ScenePicker(const Project& project);

    ~ScenePicker();

    PickingResult pick(const foundation::Vector2d& ndc) const;

  private:
    struct Impl;
    Impl* impl;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_SCENEPICKER_H
