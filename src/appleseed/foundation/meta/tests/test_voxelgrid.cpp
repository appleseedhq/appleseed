
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

// appleseed.foundation headers.
#include "foundation/math/voxelgrid.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
#include <cstdlib>

using namespace foundation;
using namespace std;

FOUNDATION_TEST_SUITE(Foundation_Math_VoxelGrid3)
{
    struct Fixture
    {
        VoxelGrid3<float> m_grid;

        Fixture()
          : m_grid(3, 3, 3, 4)
        {
            for (int z = 0; z < 3; ++z)
            {
                for (int y = 0; y < 3; ++y)
                {
                    for (int x = 0; x < 3; ++x)
                    {
                        float* values = m_grid.voxel(x, y, z);
                        values[0] = static_cast<float>(1 - abs(x - 1));
                        values[1] = static_cast<float>(1 - abs(y - 1));
                        values[2] = static_cast<float>(1 - abs(z - 1));
                        values[3] = 1.0f;
                    }
                }
            }
        }
    };

    FOUNDATION_TEST_CASE_WITH_FIXTURE(TrilinearLookup_Corner, Fixture)
    {
        float values[4];
        m_grid.trilinear_lookup(Vector3f(0.0f, 0.0f, 0.0f), values);

        FOUNDATION_EXPECT_FEQ(0.0f, values[0]);
        FOUNDATION_EXPECT_FEQ(0.0f, values[1]);
        FOUNDATION_EXPECT_FEQ(0.0f, values[2]);
        FOUNDATION_EXPECT_FEQ(1.0f, values[3]);
    }

    FOUNDATION_TEST_CASE_WITH_FIXTURE(TrilinearLookup_Middle, Fixture)
    {
        float values[4];
        m_grid.trilinear_lookup(Vector3f(0.5f, 0.5f, 0.5f), values);

        FOUNDATION_EXPECT_FEQ(0.5f, values[0]);
        FOUNDATION_EXPECT_FEQ(0.5f, values[1]);
        FOUNDATION_EXPECT_FEQ(0.5f, values[2]);
        FOUNDATION_EXPECT_FEQ(1.0f, values[3]);
    }

    FOUNDATION_TEST_CASE_WITH_FIXTURE(TrilinearLookup_Center, Fixture)
    {
        float values[4];
        m_grid.trilinear_lookup(Vector3f(1.5f, 1.5f, 1.5f), values);

        FOUNDATION_EXPECT_FEQ(1.0f, values[0]);
        FOUNDATION_EXPECT_FEQ(1.0f, values[1]);
        FOUNDATION_EXPECT_FEQ(1.0f, values[2]);
        FOUNDATION_EXPECT_FEQ(1.0f, values[3]);
    }
}
