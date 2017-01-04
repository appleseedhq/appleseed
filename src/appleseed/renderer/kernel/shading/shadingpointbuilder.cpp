
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
#include "shadingpointbuilder.h"

using namespace foundation;

namespace renderer
{

ShadingPointBuilder::ShadingPointBuilder(ShadingPoint& shading_point)
  : m_shading_point(shading_point)
{
}

void ShadingPointBuilder::set_scene(const Scene* scene)
{
    m_shading_point.m_scene = scene;
}

void ShadingPointBuilder::set_ray(const ShadingRay& ray)
{
    m_shading_point.m_ray = ray;
}

void ShadingPointBuilder::set_primitive_type(const ShadingPoint::PrimitiveType primitive_type)
{
    m_shading_point.m_primitive_type = primitive_type;
}

void ShadingPointBuilder::set_point(const Vector3d& point)
{
    m_shading_point.m_point = point;
    m_shading_point.m_members |= ShadingPoint::HasPoint;
}

void ShadingPointBuilder::set_geometric_normal(const Vector3d& n)
{
    m_shading_point.m_geometric_normal = n;
    m_shading_point.m_members |= ShadingPoint::HasGeometricNormal;
}

void ShadingPointBuilder::set_side(const ObjectInstance::Side side)
{
    m_shading_point.m_side = side;
}

void ShadingPointBuilder::set_shading_basis(const Basis3d& basis)
{
    m_shading_point.m_shading_basis = basis;
    m_shading_point.m_members |= ShadingPoint::HasShadingBasis;
}

void ShadingPointBuilder::set_uvs(const Vector2f& uv)
{
    m_shading_point.m_uv = uv;
    m_shading_point.m_members |= ShadingPoint::HasUV0;
}

}   // namespace renderer
