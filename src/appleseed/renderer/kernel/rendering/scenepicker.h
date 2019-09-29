
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/scene/objectinstance.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
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
        foundation::Vector2d            m_ndc;

        bool                            m_hit;
        ShadingPoint::PrimitiveType     m_primitive_type;               // type of the hit primitive
        double                          m_distance;                     // world space distance from the ray origin to the intersection point

        foundation::Vector2f            m_bary;                         // barycentric coordinates of intersection point
        foundation::Vector2f            m_uv;                           // texture coordinates from UV set #0
        foundation::Vector2f            m_duvdx;                        // screen space partial derivative of the texture coords wrt. X
        foundation::Vector2f            m_duvdy;                        // screen space partial derivative of the texture coords wrt. Y
        foundation::Vector3d            m_point;                        // world space intersection point
        foundation::Vector3d            m_dpdu;                         // world space partial derivative of the intersection point wrt. U
        foundation::Vector3d            m_dpdv;                         // world space partial derivative of the intersection point wrt. V
        foundation::Vector3d            m_dndu;                         // world space partial derivative of the intersection normal wrt. U
        foundation::Vector3d            m_dndv;                         // world space partial derivative of the intersection normal wrt. V
        foundation::Vector3d            m_dpdx;                         // screen space partial derivative of the intersection point wrt. X
        foundation::Vector3d            m_dpdy;                         // screen space partial derivative of the intersection point wrt. Y
        foundation::Vector3d            m_geometric_normal;             // world space geometric normal, unit-length
        foundation::Vector3d            m_original_shading_normal;      // original world space shading normal, unit-length
        foundation::Basis3d             m_shading_basis;                // world space orthonormal basis around shading normal
        ObjectInstance::Side            m_side;                         // side of the surface that was hit

        const Camera*                   m_camera;
        const AssemblyInstance*         m_assembly_instance;
        foundation::Transformd          m_assembly_instance_transform;
        const Assembly*                 m_assembly;
        ObjectInstance*                 m_object_instance;
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

}   // namespace renderer
