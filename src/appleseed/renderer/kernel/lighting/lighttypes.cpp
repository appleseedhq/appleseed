
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Petra Gospodnetic, The appleseedhq Organization
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
#include "lighttypes.h"

// appleseed.renderer headers.
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"

namespace renderer
{

//
// NonPhysicalLightSource class implementation.
//

NonPhysicalLightSource::NonPhysicalLightSource(NonPhysicalLightInfo* light)
  : m_light_info(light)
{
}

foundation::Vector3d NonPhysicalLightSource::get_position() const
{
    const Light* light = m_light_info->m_light;

    // Compute the exact position of the light.
    const foundation::Vector3d position =
        light->get_transform()
            .get_local_to_parent()
            .extract_translation();

    return position;
}

foundation::AABB3d NonPhysicalLightSource::get_bbox() const
{
    const foundation::Vector3d position = get_position();

    // Non physical light has no real size - hence we are assigning it some
    // arbitrary small value for the bbox.
    return foundation::AABB3d(
                foundation::Vector3d(position[0] - 0.001,
                                     position[1] - 0.001,
                                     position[2] - 0.001),
                foundation::Vector3d(position[0] + 0.001,
                                     position[1] + 0.001,
                                     position[2] + 0.001));
}

float NonPhysicalLightSource::get_importance() const
{
    Spectrum spectrum;
    m_light_info->m_light->get_inputs()
        .find("intensity")
        .source()->evaluate_uniform(spectrum);

    return foundation::average_value(spectrum);
}

int NonPhysicalLightSource::get_type() const
{
    return LightSource::NonPhysicalLightType;
}

void NonPhysicalLightSource::set_tree_index(const size_t node_index) const
{
    m_light_info->m_light_tree_node_index = node_index;
}

//
// EmittingTriangleLightSource class implementation.
//

EmittingTriangleLightSource::EmittingTriangleLightSource(EmittingTriangle* triangle)
  : m_triangle(triangle)
{
}

foundation::Vector3d EmittingTriangleLightSource::get_position() const
{
    // Return the centroid of the triangle as the position.
    return (m_triangle->m_v0 + m_triangle->m_v1 + m_triangle->m_v2) / 3;
}

foundation::AABB3d EmittingTriangleLightSource::get_bbox() const
{
    foundation::AABB3d bbox;

    bbox.invalidate();
    bbox.insert(m_triangle->m_v0);
    bbox.insert(m_triangle->m_v1);
    bbox.insert(m_triangle->m_v2);

    return bbox;
}

float EmittingTriangleLightSource::get_importance() const
{
    const EDF* edf = m_triangle->m_material->get_uncached_edf();
    
    return edf->get_max_contribution() * edf->get_uncached_importance_multiplier();
}

int EmittingTriangleLightSource::get_type() const
{
    return LightSource::EmittingTriangleType;
}

void EmittingTriangleLightSource::set_tree_index(const size_t node_index) const
{
    m_triangle->m_light_tree_node_index = node_index;
}

}   // namespace renderer
