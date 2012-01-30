
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "triangletree.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// TriangleTree class implementation.
//

TriangleTree::Arguments::Arguments(
    const UniqueID                  triangle_tree_uid,
    const GAABB3&                   bbox,
    const Assembly&                 assembly,
    const RegionInfoVector& regions)
  : m_triangle_tree_uid(triangle_tree_uid)
  , m_bbox(bbox)
  , m_assembly(assembly)
  , m_regions(regions)
{
}

TriangleTree::TriangleTree(const Arguments& arguments)
  : m_triangle_tree_uid(arguments.m_triangle_tree_uid)
{
}

TriangleTree::~TriangleTree()
{
    // Log a progress message.
    RENDERER_LOG_INFO(
        "deleting triangle bvh tree #" FMT_UNIQUE_ID "...",
        m_triangle_tree_uid);
}


//
// TriangleTreeFactory class implementation.
//

TriangleTreeFactory::TriangleTreeFactory(
    const TriangleTree::Arguments&  arguments)
  : m_arguments(arguments)
{
}

auto_ptr<TriangleTree> TriangleTreeFactory::create()
{
    return auto_ptr<TriangleTree>(new TriangleTree(m_arguments));
}


//
// TriangleLeafVisitor class implementation.
//

bool TriangleLeafVisitor::visit(
    const vector<BVHTriangle>&      items,
    const vector<GAABB3>&           bboxes,
    const size_t                    begin,
    const size_t                    end,
    const ShadingRay::RayType&      ray,
    const ShadingRay::RayInfoType&  ray_info,
    const double                    tmin,
    const double                    tmax,
    double&                         distance)
{
    return false;
}


//
// TriangleLeafProbeVisitor class implementation.
//

bool TriangleLeafProbeVisitor::visit(
    const vector<BVHTriangle>&      items,
    const vector<GAABB3>&           bboxes,
    const size_t                    begin,
    const size_t                    end,
    const ShadingRay::RayType&      ray,
    const ShadingRay::RayInfoType&  ray_info,
    const double                    tmin,
    const double                    tmax,
    double&                         distance)
{
    return false;
}

}   // namespace renderer
