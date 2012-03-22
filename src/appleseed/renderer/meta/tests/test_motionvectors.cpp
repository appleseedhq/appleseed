
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
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/numerictype.h"
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

        TessellationType tess;

        //
        // Part 1: create a tessellation with motion vectors.
        //

        {
            // Insert a single triangle into the tessellation. The tessellation is defined for t=0.
            tess.m_vertices.push_back(GVector3(0.0f, 0.0f, 0.0f));
            tess.m_vertices.push_back(GVector3(1.0f, 0.0f, 0.0f));
            tess.m_vertices.push_back(GVector3(0.0f, 1.0f, 0.0f));
            tess.m_primitives.push_back(Triangle(0, 1, 2));

            // Create a tessellation attribute to store motion times.
            const AttributeSet::ChannelID motion_times =
                tess.m_tess_attributes.create_channel("motion_times", NumericTypeDouble, 1);

            // We'll have a single motion vector per vertex defining the motion from t=0 to t=1.
            tess.m_tess_attributes.push_attribute(motion_times, 1.0);

            // Create a vertex attribute to store motion vectors.
            const AttributeSet::ChannelID motion_vectors =
                tess.m_vertex_attributes.create_channel("motion_vectors", NumericType::id<GVector3::ValueType>(), 3 * 1);

            // Insert motion vectors.
            tess.m_vertex_attributes.push_attribute(motion_vectors, GVector3(0.5f, 0.5f, 0.0f));
            tess.m_vertex_attributes.push_attribute(motion_vectors, GVector3(0.5f, 0.0f, 0.0f));
            tess.m_vertex_attributes.push_attribute(motion_vectors, GVector3(0.0f, 0.5f, 0.0f));
        }

        //
        // Part 2: reconstruct the tessellation key frames.
        //

        {
            // Retrieve the motion times channel ID.
            const AttributeSet::ChannelID motion_times = tess.m_tess_attributes.find_channel("motion_times");
            ASSERT_NEQ(AttributeSet::InvalidChannelID, motion_times);

            // Retrieve the motion vectors channel ID.
            const AttributeSet::ChannelID motion_vectors = tess.m_vertex_attributes.find_channel("motion_vectors");
            ASSERT_NEQ(AttributeSet::InvalidChannelID, motion_vectors);

            // Retrieve the number of motion vectors.
            const size_t motion_vector_count = tess.m_tess_attributes.get_attribute_count(motion_times);
            ASSERT_EQ(1, motion_vector_count);

            // Enumerate the key frames.
            const size_t vertex_count = tess.m_vertices.size();
            TessellationType::VectorArray v = tess.m_vertices;
            for (size_t i = 0; i < motion_vector_count; ++i)
            {
                for (size_t j = 0; j < vertex_count; ++j)
                {
                    GVector3 mv;
                    tess.m_vertex_attributes.get_attribute(motion_vectors, i * vertex_count + j, &mv);
                    v[j] += mv;
                }
            }
        }
    }
}
