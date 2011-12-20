
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_INTERSECTIONSETTINGS_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_INTERSECTIONSETTINGS_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// appleseed.foundation headers.
#include "foundation/math/intersection.h"

namespace renderer
{

//
// Triangle types.
//

// Triangle format used for storage.
typedef foundation::TriangleMT<GScalar> GTriangleType;

// Triangle format used for intersection.
typedef foundation::TriangleMT<double> TriangleType;
typedef foundation::TriangleMTSupportPlane<double> TriangleSupportPlaneType;


//
// Region BSP tree statistics.
//

// Maximum region duplication rate.
const double RegionTreeMaxDuplication = 2.0;

// Maximum number of regions per leaf.
const size_t RegionTreeMaxLeafSize = 64;

// Maximum depth of the tree.
const size_t RegionTreeMaxDepth = 16;

// Size of the region tree access cache.
const size_t RegionTreeAccessCacheSize = 16;


//
// Triangle BSP tree statistics.
//

// If defined, leaves of the triangle tree get split along their
// longest dimension only. If left undefined, all three dimensions
// are considered for splitting, resulting in better trees (faster
// intersection, but slower construction).
#undef RENDERER_TRIANGLE_TREE_SPLIT_LONGEST_AXIS

// Maximum triangle duplication rate.
const double TriangleTreeMaxDuplication = 2.0;

// Maximum number of triangles per leaf.
const size_t TriangleTreeMaxLeafSize = 1;

// Maximum depth of the tree.
const size_t TriangleTreeMaxDepth = 64;

// Number of bins used in the construction of the approximate SAH function.
const size_t TriangleTreeApproxSAHBinCount = 32;

// Multiplier for the cost of keeping a leaf unsplit.
const GScalar TriangleTreeLeafCostMultiplier = GScalar(0.99);

// Leaf size threshold for O1 optimization level (approximate SAH).
const size_t TriangleTreeO1Threshold = 1024 * 1024;

// Leaf size threshold for O2 optimization level (exact SAH).
const size_t TriangleTreeO2Threshold = 32;

// Depth of a subtree in the van Emde Boas node layout.
const size_t TriangleTreeSubtreeDepth = 3;

// Minimum size in bytes of one page of leaves.
const size_t TriangleTreeMinLeafPageSize = 1024 * 1024;

// Size of the triangle tree access cache.
const size_t TriangleTreeAccessCacheSize = 16;

// Enable/disable construction tracing.
const bool TriangleTreeTraceConstruction = false;

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_INTERSECTIONSETTINGS_H
