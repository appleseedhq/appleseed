
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

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/tessellation/statictessellation.h"
#include "renderer/modeling/object/triangle.h"

// appleseed.foundation headers.
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

using namespace foundation;
using namespace renderer;

TEST_SUITE(MotionVectorsExploration)
{
    TEST_CASE(AddMotionVectorsToStaticTessellationAsVertexAttributes)
    {
        typedef StaticTessellation<Triangle> TessellationType;

        //
        // Part 1: create a tessellation with motion vectors.
        //

        TessellationType tess;

        // Insert a single triangle into the tessellation. The tessellation is defined for t=0.
        tess.m_vertices.push_back(GVector3(0.0f, 0.0f, 0.0f));
        tess.m_vertices.push_back(GVector3(1.0f, 0.0f, 0.0f));
        tess.m_vertices.push_back(GVector3(0.0f, 1.0f, 0.0f));
        tess.m_primitives.push_back(Triangle(0, 1, 2));

        // We'll have one motion segment (a single motion vector per vertex) defining the motion from t=0 to t=1.
        tess.set_motion_segment_count(1);

        // Assign motion vectors.
        tess.set_motion_vector(0, 0, GVector3(0.5f, 0.5f, 0.0f));
        tess.set_motion_vector(1, 0, GVector3(0.5f, 0.0f, 0.0f));
        tess.set_motion_vector(2, 0, GVector3(0.0f, 0.5f, 0.0f));

        //
        // Part 2: reconstruct the tessellation key frames.
        //

        // Retrieve the number of vertices.
        const size_t vertex_count = tess.m_vertices.size();
        EXPECT_EQ(3, vertex_count);

        // Retrieve the number of motion segments.
        const size_t motion_segment_count = tess.get_motion_segment_count();
        EXPECT_EQ(1, motion_segment_count);

        // Enumerate the key frames.
        TessellationType::VectorArray v = tess.m_vertices;
        for (size_t j = 0; j < motion_segment_count; ++j)
        {
            for (size_t i = 0; i < vertex_count; ++i)
                v[j] += tess.get_motion_vector(i, j);
        }
    }
}
