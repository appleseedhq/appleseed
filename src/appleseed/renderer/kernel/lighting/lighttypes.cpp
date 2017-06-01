
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
    // Non physical light has no real size - hence we are fixing some small value for bbox
    return foundation::AABB3d(
                foundation::Vector3d( 0.01,  0.01,  0.01),
                foundation::Vector3d(-0.01, -0.01, -0.01));
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

    foundation::Vector3d min = foundation::Vector3d(
                foundation::min(vertex0[0], vertex1[0], vertex2[0]),
                foundation::min(vertex0[1], vertex1[1], vertex2[1]),
                foundation::min(vertex0[2], vertex1[2], vertex2[2]));

    foundation::Vector3d max = foundation::Vector3d(
                foundation::max(vertex0[0], vertex1[0], vertex2[0]),
                foundation::max(vertex0[1], vertex1[1], vertex2[1]),
                foundation::max(vertex0[2], vertex1[2], vertex2[2]));

    return foundation::AABB3d(min, max);
}

}   // namespace renderer