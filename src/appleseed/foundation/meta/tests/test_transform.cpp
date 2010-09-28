
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
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

TEST_SUITE(Foundation_Math_Transform)
{
    using namespace foundation;

    struct FixtureTransformByIdentity
    {
        const Transform<double> transform;

        FixtureTransformByIdentity()
          : transform(Matrix4d::identity())
        {
        }
    };

    TEST_CASE_WITH_FIXTURE(TransformPointByIdentity, FixtureTransformByIdentity)
    {
        const Vector3d v(1.0, 2.0, 3.0);

        EXPECT_FEQ(v, transform.transform_point_to_parent(v));
        EXPECT_FEQ(v, transform.transform_point_to_local(v));
    }

    TEST_CASE_WITH_FIXTURE(TransformVectorByIdentity, FixtureTransformByIdentity)
    {
        const Vector3d v(1.0, 2.0, 3.0);

        EXPECT_FEQ(v, transform.transform_vector_to_parent(v));
        EXPECT_FEQ(v, transform.transform_vector_to_local(v));
    }

    TEST_CASE_WITH_FIXTURE(TransformNormalByIdentity, FixtureTransformByIdentity)
    {
        const Vector3d v(1.0, 2.0, 3.0);

        EXPECT_FEQ(v, transform.transform_normal_to_parent(v));
        EXPECT_FEQ(v, transform.transform_normal_to_local(v));
    }

    TEST_CASE_WITH_FIXTURE(TransformRayByIdentity, FixtureTransformByIdentity)
    {
        const Ray3d r(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0),
            0.1,
            1000.0);

        EXPECT_FEQ(r, transform.transform_to_parent(r));
        EXPECT_FEQ(r, transform.transform_to_local(r));
    }

    struct FixtureTransformByTranslation
    {
        const Transform<double> transform;

        FixtureTransformByTranslation()
          : transform(Matrix4d::translation(Vector3d(10.0, 20.0, 30.0)))
        {
        }
    };

    TEST_CASE_WITH_FIXTURE(TransformPointByTranslation, FixtureTransformByTranslation)
    {
        const Vector3d v(1.0, 2.0, 3.0);

        EXPECT_FEQ(Vector3d(11.0, 22.0, 33.0), transform.transform_point_to_parent(v));
        EXPECT_FEQ(Vector3d(-9.0, -18.0, -27.0), transform.transform_point_to_local(v));
    }

    TEST_CASE_WITH_FIXTURE(TransformVectorByTranslation, FixtureTransformByTranslation)
    {
        const Vector3d v(1.0, 2.0, 3.0);

        EXPECT_FEQ(v, transform.transform_vector_to_parent(v));
        EXPECT_FEQ(v, transform.transform_vector_to_local(v));
    }

    TEST_CASE_WITH_FIXTURE(TransformNormalByTranslation, FixtureTransformByTranslation)
    {
        const Vector3d v(1.0, 2.0, 3.0);

        EXPECT_FEQ(v, transform.transform_normal_to_parent(v));
        EXPECT_FEQ(v, transform.transform_normal_to_local(v));
    }

    TEST_CASE_WITH_FIXTURE(TransformRayByTranslation, FixtureTransformByTranslation)
    {
        const Ray3d r(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0),
            0.1,
            1000.0);

        EXPECT_FEQ(Ray3d(Vector3d(11.0, 22.0, 33.0), r.m_dir, r.m_tmin, r.m_tmax), transform.transform_to_parent(r));
        EXPECT_FEQ(Ray3d(Vector3d(-9.0, -18.0, -27.0), r.m_dir, r.m_tmin, r.m_tmax), transform.transform_to_local(r));
    }

    struct FixtureTransformByRotation
    {
        const Transform<double> transform;

        FixtureTransformByRotation()
          : transform(Matrix4d::rotation_z(deg_to_rad(90.0)))
        {
        }
    };

    TEST_CASE_WITH_FIXTURE(TransformPointByRotation, FixtureTransformByRotation)
    {
        const Vector3d v(2.0, 2.0, 3.0);

        EXPECT_FEQ(Vector3d(-2.0, 2.0, 3.0), transform.transform_point_to_parent(v));
        EXPECT_FEQ(Vector3d(2.0, -2.0, 3.0), transform.transform_point_to_local(v));
    }

    TEST_CASE_WITH_FIXTURE(TransformVectorByRotation, FixtureTransformByRotation)
    {
        const Vector3d v(2.0, 2.0, 3.0);

        EXPECT_FEQ(Vector3d(-2.0, 2.0, 3.0), transform.transform_vector_to_parent(v));
        EXPECT_FEQ(Vector3d(2.0, -2.0, 3.0), transform.transform_vector_to_local(v));
    }

    TEST_CASE_WITH_FIXTURE(TransformNormalByRotation, FixtureTransformByRotation)
    {
        const Vector3d v(2.0, 2.0, 3.0);

        EXPECT_FEQ(Vector3d(-2.0, 2.0, 3.0), transform.transform_normal_to_parent(v));
        EXPECT_FEQ(Vector3d(2.0, -2.0, 3.0), transform.transform_normal_to_local(v));
    }

    TEST_CASE_WITH_FIXTURE(TransformRayByRotation, FixtureTransformByRotation)
    {
        const Ray3d r(
            Vector3d(2.0, 2.0, 3.0),
            Vector3d(4.0, 4.0, 6.0),
            0.1,
            1000.0);

        EXPECT_FEQ(Ray3d(Vector3d(-2.0, 2.0, 3.0), Vector3d(-4.0, 4.0, 6.0), r.m_tmin, r.m_tmax), transform.transform_to_parent(r));
        EXPECT_FEQ(Ray3d(Vector3d(2.0, -2.0, 3.0), Vector3d(4.0, -4.0, 6.0), r.m_tmin, r.m_tmax), transform.transform_to_local(r));
    }

    TEST_CASE(MultiplicationOperator_GivenTwoIdentityTransforms_ReturnsIdentityTransform)
    {
        typedef Transform<double> Transform;

        const Transform a(Matrix4d::identity());
        const Transform b(Matrix4d::identity());

        const Transform result = a * b;

        EXPECT_EQ(Matrix4d::identity(), result.get_local_to_parent());
    }

    TEST_CASE(MultiplicationOperator_GivenIdentityTransformAndRotationTransform_ReturnsRotationTransform)
    {
        typedef Transform<double> Transform;

        const Matrix4d rotation(Matrix4d::rotation_z(deg_to_rad(90.0)));

        const Transform a(Matrix4d::identity());
        const Transform b(rotation);

        const Transform result = a * b;

        EXPECT_EQ(rotation, result.get_local_to_parent());
    }

    TEST_CASE(MultiplicationOperator_GivenRotationTransformAndIdentityTransform_ReturnsRotationTransform)
    {
        typedef Transform<double> Transform;

        const Matrix4d rotation(Matrix4d::rotation_z(deg_to_rad(90.0)));

        const Transform a(rotation);
        const Transform b(Matrix4d::identity());

        const Transform result = a * b;

        EXPECT_EQ(rotation, result.get_local_to_parent());
    }
}
