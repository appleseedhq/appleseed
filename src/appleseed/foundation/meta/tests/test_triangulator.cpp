
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
        typedef TriangulatorType::Polygon2 Polygon2;
        typedef TriangulatorType::Polygon3 Polygon3;
    };

    TEST_CASE_F(ComputePolygonOrientation_GivenLowestLeftmostTriangleIsValid_ReturnsCorrectOrientation, Fixture)
    {
        Polygon2 polygon;
        polygon.push_back(Vector2Type(0.0, 0.0));
        polygon.push_back(Vector2Type(0.0, 1.0));
        polygon.push_back(Vector2Type(1.0, 1.0));
        polygon.push_back(Vector2Type(1.0, 0.0));

        TriangulatorType triangulator;
        const TriangulatorType::Orientation orientation =
            triangulator.compute_polygon_orientation(polygon);

        EXPECT_EQ(TriangulatorType::CW, orientation);
    }

    TEST_CASE_F(ComputePolygonOrientation_GivenLowestLeftmostTriangleIsDegenerate_ReturnsCorrectOrientation, Fixture)
    {
        Polygon2 polygon;
        polygon.push_back(Vector2Type(0.0, 1.0));
        polygon.push_back(Vector2Type(1.0, 1.0));
        polygon.push_back(Vector2Type(0.0, 0.0));
        polygon.push_back(Vector2Type(0.0, 0.0));

        TriangulatorType triangulator;
        const TriangulatorType::Orientation orientation =
            triangulator.compute_polygon_orientation(polygon);

        EXPECT_EQ(TriangulatorType::CW, orientation);
    }

    TEST_CASE_F(Triangulate_GivenQuadWithCoincidentVertices_KeepDegenerateTrianglesIsFalse_ReturnsOneTriangle, Fixture)
    {
        Polygon3 polygon;
        polygon.push_back(Vector3Type(0.0, 0.0, 0.0));
        polygon.push_back(Vector3Type(0.0, 1.0, 0.0));
        polygon.push_back(Vector3Type(1.0, 1.0, 0.0));
        polygon.push_back(Vector3Type(0.0, 0.0, 0.0));

        TriangulatorType triangulator;
        TriangulatorType::IndexArray triangles;

        const bool success = triangulator.triangulate(polygon, triangles);

        ASSERT_TRUE(success);
        EXPECT_EQ(1 * 3, triangles.size());
    }

    TEST_CASE_F(Triangulate_GivenQuadWithCoincidentVertices_KeepDegenerateTrianglesIsTrue_ReturnsTwoTriangles, Fixture)
    {
        Polygon3 polygon;
        polygon.push_back(Vector3Type(0.0, 0.0, 0.0));
        polygon.push_back(Vector3Type(0.0, 1.0, 0.0));
        polygon.push_back(Vector3Type(1.0, 1.0, 0.0));
        polygon.push_back(Vector3Type(0.0, 0.0, 0.0));

        TriangulatorType triangulator(TriangulatorType::KeepDegenerateTriangles);
        TriangulatorType::IndexArray triangles;

        const bool success = triangulator.triangulate(polygon, triangles);

        ASSERT_TRUE(success);
        EXPECT_EQ(2 * 3, triangles.size());
    }

    TEST_CASE_F(Triangulate_GivenSelfCrossingTriangle_ReturnsFalse, Fixture)
    {
        Polygon3 polygon;
        polygon.push_back(Vector3Type(0.0, 0.0, 1.0));
        polygon.push_back(Vector3Type(3.0, 2.0, 2.0));
        polygon.push_back(Vector3Type(3.0, 3.0, 3.0));
        polygon.push_back(Vector3Type(1.0, 0.0, 0.0));

        TriangulatorType triangulator;
        TriangulatorType::IndexArray triangles;

        const bool success = triangulator.triangulate(polygon, triangles);

        EXPECT_FALSE(success);
    }

    TEST_CASE_F(Triangulate_GivenComplexConcavePolygon_ReturnsTrue, Fixture)
    {
        Polygon3 polygon;
        polygon.push_back(Vector3Type(0.137498, -1.09128, 0.0));
        polygon.push_back(Vector3Type(0.124257, -1.10419, 0.0));
        polygon.push_back(Vector3Type(0.124257, -1.31878, 0.0));
        polygon.push_back(Vector3Type(0.240957, -1.30956, 0.0));
        polygon.push_back(Vector3Type(0.240957, 1.3116, 0.0));
        polygon.push_back(Vector3Type(0.124256, 1.30447, 0.0));
        polygon.push_back(Vector3Type(0.124256, 1.08131, 0.0));
        polygon.push_back(Vector3Type(0.137498, 1.0684, 0.0));
        polygon.push_back(Vector3Type(0.160558, 1.02428, 0.0));
        polygon.push_back(Vector3Type(0.168503, 0.975371, 0.0));
        polygon.push_back(Vector3Type(0.168503, -0.998254, 0.0));
        polygon.push_back(Vector3Type(0.160558, -1.04716, 0.0));

        TriangulatorType triangulator;
        TriangulatorType::IndexArray triangles;

        const bool success = triangulator.triangulate(polygon, triangles);

        EXPECT_TRUE(success);
    }
}
