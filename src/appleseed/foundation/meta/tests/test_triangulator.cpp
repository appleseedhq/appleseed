
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "foundation/math/triangulator.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Math_Triangulator)
{
    struct Fixture
    {
        typedef double ValueType;
        typedef Vector<ValueType, 2> Vector2Type;
        typedef Vector<ValueType, 3> Vector3Type;
        typedef Triangulator<ValueType> TriangulatorType;
    };

    TEST_CASE_F(ComputePolygonOrientation_GivenLowestLeftmostTriangleIsValid_ReturnsCorrectOrientation, Fixture)
    {
        TriangulatorType::Polygon2 polygon;
        polygon.push_back(Vector2Type(0.0, 0.0));
        polygon.push_back(Vector2Type(0.0, 1.0));
        polygon.push_back(Vector2Type(1.0, 1.0));
        polygon.push_back(Vector2Type(1.0, 0.0));

        TriangulatorType triangulator;
        const TriangulatorType::Orientation orientation =
            triangulator.compute_polygon_orientation(polygon);

        EXPECT_EQ(TriangulatorType::CW, orientation);
    }

    TEST_CASE_F(ComputePolygonOrientation_GivenLowestLeftmostTriangleIsDenerate_ReturnsCorrectOrientation, Fixture)
    {
        TriangulatorType::Polygon2 polygon;
        polygon.push_back(Vector2Type(0.0, 0.0));
        polygon.push_back(Vector2Type(0.0, 0.0));
        polygon.push_back(Vector2Type(0.0, 1.0));
        polygon.push_back(Vector2Type(1.0, 1.0));

        TriangulatorType triangulator;
        const TriangulatorType::Orientation orientation =
            triangulator.compute_polygon_orientation(polygon);

        EXPECT_EQ(TriangulatorType::CW, orientation);
    }

    TEST_CASE_F(Triangulate_GivenSelfCrossingTriangle_ReturnsFalse, Fixture)
    {
        TriangulatorType::Polygon3 polygon;
        polygon.push_back(Vector3Type(0.0, 0.0, 1.0));
        polygon.push_back(Vector3Type(3.0, 2.0, 2.0));
        polygon.push_back(Vector3Type(3.0, 3.0, 3.0));
        polygon.push_back(Vector3Type(1.0, 0.0, 0.0));

        TriangulatorType triangulator;
        TriangulatorType::IndexArray triangles;

        const bool success = triangulator.triangulate(polygon, triangles);

        EXPECT_FALSE(success);
    }
}
