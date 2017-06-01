
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
#include "lighttree.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// Standard headers.

namespace renderer
{

//
// LightTree class implementation.
//

LightTree::LightTree(const Scene& scene)
{

}

LightTree::~LightTree()
{
    RENDERER_LOG_INFO("Deleting the light tree...");
    for(foundation::const_each<LightSourcePointerVector> i = m_light_sources; i; ++i)
    {
        delete *i;
    }
}

void LightTree::build(
        const std::vector<NonPhysicalLightInfo>     non_physical_lights,
        const std::vector<EmittingTriangle>         emitting_triangles)
{

    RENDERER_LOG_INFO("Building a tree");
    // AABBVector light_bboxes;

    RENDERER_LOG_INFO("Collecting light sources...");
    // Collect all possible light sources into one vector
    for (foundation::const_each<NonPhysicalLightVector> i = non_physical_lights; i; ++i)
    {
        LightSource* light_source = new NonPhysicalLightSource(&*i);
        foundation::Vector3d position = light_source->get_position();
        m_light_sources.push_back(light_source);

        RENDERER_LOG_INFO("Non physical light at coordinates [%f %f %f]", position[0], position[1], position[2]);
    }

    for (foundation::const_each<EmittingTriangleVector> i = emitting_triangles; i; ++i)
    {
        LightSource* light_source = new EmittingTriangleLightSource(&*i);
        foundation::Vector3d position = light_source->get_position();
        m_light_sources.push_back(light_source);
        RENDERER_LOG_INFO("Emitting triangle centroid at coordinates [%f %f %f]", position[0], position[1], position[2]);
    }


}

}   // namespace renderer
