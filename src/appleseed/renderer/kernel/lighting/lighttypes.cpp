
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
#include "lighttypes.h"

// appleseed. renderer headers.
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/light/light.h"

// appleseed.foundation headers.
#include "foundation/math/minmax.h"
#include "foundation/math/transform.h"

namespace renderer
{

LightSource::LightSource()
{
}

LightSource::~LightSource()
{
}

//
// Non-physical light source class implementation
//

NonPhysicalLightSource::NonPhysicalLightSource(const NonPhysicalLightInfo* light)
: m_light_info(light)
{
}

foundation::Vector3d NonPhysicalLightSource::get_position()  const
{
    const Light* light = m_light_info->m_light;

    // Compute the exact position of the light
    foundation::Vector3d position = light->get_transform()
                                          .get_local_to_parent()
                                          .extract_translation();
    
    return position;
}

foundation::AABB3d NonPhysicalLightSource::get_bbox() const
{
    const Light* light = m_light_info->m_light;
    foundation::Vector3d position = light->get_transform()
                                          .get_local_to_parent()
                                          .extract_translation();
    // Non physical light has no real size - hence we are fixing some small value for the bbox
    return foundation::AABB3d(
                foundation::Vector3d(position[0] - 0.001, position[1] - 0.001, position[2] - 0.001),
                foundation::Vector3d(position[0] + 0.001, position[1] + 0.001, position[2] + 0.001));
}

Spectrum NonPhysicalLightSource::get_intensity() const
{
    const Light* light = m_light_info->m_light;
    Spectrum intensity;
    light->get_inputs().find("intensity").source()->evaluate_uniform(intensity);
    return intensity;
}

//
// Emitting triangle light source class implementation
//

EmittingTriangleLightSource::EmittingTriangleLightSource(const EmittingTriangle* light)
: m_light(light)
{
}

foundation::Vector3d EmittingTriangleLightSource::get_position() const
{
    // Retrieve coordinates of each vertex in world space
    foundation::Vector3d vertex0 = m_light->m_v0;
    foundation::Vector3d vertex1 = m_light->m_v1;
    foundation::Vector3d vertex2 = m_light->m_v2;
    
    foundation::Vector3d centroid = (vertex0 + vertex1 + vertex2) / 3;

    return centroid;
}

foundation::AABB3d EmittingTriangleLightSource::get_bbox() const
{
    // Retrieve coordinates of each vertex in world space
    foundation::Vector3d vertex0 = m_light->m_v0;
    foundation::Vector3d vertex1 = m_light->m_v1;
    foundation::Vector3d vertex2 = m_light->m_v2;

    foundation::AABB3d bbox;
    bbox.invalidate();
    bbox.insert(vertex0);
    bbox.insert(vertex1);
    bbox.insert(vertex2);

    return bbox;
}

Spectrum EmittingTriangleLightSource::get_intensity() const
{
    Spectrum hard_coded_placeholder(foundation::Color3f(1.0f, 2.0f, 3.0f));
    return hard_coded_placeholder;
}

}   // namespace renderer