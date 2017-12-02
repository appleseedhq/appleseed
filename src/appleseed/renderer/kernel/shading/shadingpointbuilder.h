
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

#ifndef APPLESEED_RENDERER_KERNEL_SHADING_SHADINGPOINTBUILDER_H
#define APPLESEED_RENDERER_KERNEL_SHADING_SHADINGPOINTBUILDER_H

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"

// Forward declarations.
namespace renderer  { class Scene; }
namespace renderer  { class ShadingRay; }

namespace renderer
{

//
// Utility class to initialize a renderer::ShadingPoint by hand.
// This is used to handcraft shading points in unit tests.
//

class ShadingPointBuilder
  : public foundation::NonCopyable
{
  public:
    explicit ShadingPointBuilder(ShadingPoint& shading_point);

    void set_scene(const Scene* scene);
    void set_ray(const ShadingRay& ray);
    void set_primitive_type(const ShadingPoint::PrimitiveType primitive_type);
    void set_distance(const double distance);
    void set_bary(const foundation::Vector2f& bary);
    void set_point(const foundation::Vector3d& point);
    void set_geometric_normal(const foundation::Vector3d& n);
    void set_side(const ObjectInstance::Side side);
    void set_shading_basis(const foundation::Basis3d& basis);
    void set_uvs(const foundation::Vector2f& uv);

  private:
    ShadingPoint& m_shading_point;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_SHADINGPOINTBUILDER_H
