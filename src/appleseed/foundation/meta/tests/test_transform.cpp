
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

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"
#include "foundation/utility/vpythonfile.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

TEST_SUITE(Foundation_Math_Transform)
{
    TEST_CASE(Identity_ReturnsIdentityTransform)
    {
        static const double ExpectedValues[] =
        {
             1.0, 0.0, 0.0, 0.0,
             0.0, 1.0, 0.0, 0.0,
             0.0, 0.0, 1.0, 0.0,
             0.0, 0.0, 0.0, 1.0
        };

        const Transformd transform(Transformd::identity());

        EXPECT_SEQUENCE_EQ(16, ExpectedValues, &transform.get_local_to_parent()[0]);
        EXPECT_SEQUENCE_EQ(16, ExpectedValues, &transform.get_parent_to_local()[0]);
    }

    struct FixtureTransformByIdentity
    {
        const Transformd transform;

        FixtureTransformByIdentity()
          : transform(Transformd::identity())
        {
        }
    };

    TEST_CASE_F(TransformPointByIdentity, FixtureTransformByIdentity)
    {
        const Vector3d v(1.0, 2.0, 3.0);

        EXPECT_FEQ(v, transform.point_to_parent(v));
        EXPECT_FEQ(v, transform.point_to_local(v));
    }

    TEST_CASE_F(TransformVectorByIdentity, FixtureTransformByIdentity)
    {
        const Vector3d v(1.0, 2.0, 3.0);

        EXPECT_FEQ(v, transform.vector_to_parent(v));
        EXPECT_FEQ(v, transform.vector_to_local(v));
    }

    TEST_CASE_F(TransformNormalByIdentity, FixtureTransformByIdentity)
    {
        const Vector3d v(1.0, 2.0, 3.0);

        EXPECT_FEQ(v, transform.normal_to_parent(v));
        EXPECT_FEQ(v, transform.normal_to_local(v));
    }

    TEST_CASE_F(TransformRayByIdentity, FixtureTransformByIdentity)
    {
        const Ray3d r(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0),
            0.1,
            1000.0);

        EXPECT_FEQ(r, transform.to_parent(r));
        EXPECT_FEQ(r, transform.to_local(r));
    }

    struct FixtureTransformByTranslation
    {
        const Transformd transform;

        FixtureTransformByTranslation()
          : transform(
              Transformd::from_local_to_parent(
                  Matrix4d::make_translation(Vector3d(10.0, 20.0, 30.0))))
        {
        }
    };

    TEST_CASE_F(TransformPointByTranslation, FixtureTransformByTranslation)
    {
        const Vector3d v(1.0, 2.0, 3.0);

        EXPECT_FEQ(Vector3d(11.0, 22.0, 33.0), transform.point_to_parent(v));
        EXPECT_FEQ(Vector3d(-9.0, -18.0, -27.0), transform.point_to_local(v));
    }

    TEST_CASE_F(TransformVectorByTranslation, FixtureTransformByTranslation)
    {
        const Vector3d v(1.0, 2.0, 3.0);

        EXPECT_FEQ(v, transform.vector_to_parent(v));
        EXPECT_FEQ(v, transform.vector_to_local(v));
    }

    TEST_CASE_F(TransformNormalByTranslation, FixtureTransformByTranslation)
    {
        const Vector3d v(1.0, 2.0, 3.0);

        EXPECT_FEQ(v, transform.normal_to_parent(v));
        EXPECT_FEQ(v, transform.normal_to_local(v));
    }

    TEST_CASE_F(TransformRayByTranslation, FixtureTransformByTranslation)
    {
        const Ray3d r(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0),
            0.1,
            1000.0);

        EXPECT_FEQ(Ray3d(Vector3d(11.0, 22.0, 33.0), r.m_dir, r.m_tmin, r.m_tmax), transform.to_parent(r));
        EXPECT_FEQ(Ray3d(Vector3d(-9.0, -18.0, -27.0), r.m_dir, r.m_tmin, r.m_tmax), transform.to_local(r));
    }

    struct FixtureTransformByRotation
    {
        const Transformd transform;

        FixtureTransformByRotation()
          : transform(Transformd::from_local_to_parent(Matrix4d::make_rotation_z(deg_to_rad(90.0))))
        {
        }
    };

    TEST_CASE_F(TransformPointByRotation, FixtureTransformByRotation)
    {
        const Vector3d v(2.0, 2.0, 3.0);

        EXPECT_FEQ(Vector3d(-2.0, 2.0, 3.0), transform.point_to_parent(v));
        EXPECT_FEQ(Vector3d(2.0, -2.0, 3.0), transform.point_to_local(v));
    }

    TEST_CASE_F(TransformVectorByRotation, FixtureTransformByRotation)
    {
        const Vector3d v(2.0, 2.0, 3.0);

        EXPECT_FEQ(Vector3d(-2.0, 2.0, 3.0), transform.vector_to_parent(v));
        EXPECT_FEQ(Vector3d(2.0, -2.0, 3.0), transform.vector_to_local(v));
    }

    TEST_CASE_F(TransformNormalByRotation, FixtureTransformByRotation)
    {
        const Vector3d v(2.0, 2.0, 3.0);

        EXPECT_FEQ(Vector3d(-2.0, 2.0, 3.0), transform.normal_to_parent(v));
        EXPECT_FEQ(Vector3d(2.0, -2.0, 3.0), transform.normal_to_local(v));
    }

    TEST_CASE_F(TransformRayByRotation, FixtureTransformByRotation)
    {
        const Ray3d r(
            Vector3d(2.0, 2.0, 3.0),
            Vector3d(4.0, 4.0, 6.0),
            0.1,
            1000.0);

        EXPECT_FEQ(Ray3d(Vector3d(-2.0, 2.0, 3.0), Vector3d(-4.0, 4.0, 6.0), r.m_tmin, r.m_tmax), transform.to_parent(r));
        EXPECT_FEQ(Ray3d(Vector3d(2.0, -2.0, 3.0), Vector3d(4.0, -4.0, 6.0), r.m_tmin, r.m_tmax), transform.to_local(r));
    }

    TEST_CASE(MultiplicationOperator_GivenTwoIdentityTransforms_ReturnsIdentityTransform)
    {
        const Transformd a(Transformd::identity());
        const Transformd b(Transformd::identity());

        const Transformd result = a * b;

        EXPECT_EQ(Matrix4d::identity(), result.get_local_to_parent());
    }

    TEST_CASE(MultiplicationOperator_GivenIdentityTransformAndRotationTransform_ReturnsRotationTransform)
    {
        const Matrix4d rotation(Matrix4d::make_rotation_z(deg_to_rad(90.0)));

        const Transformd a(Transformd::identity());
        const Transformd b(Transformd::from_local_to_parent(rotation));

        const Transformd result = a * b;

        EXPECT_EQ(rotation, result.get_local_to_parent());
    }

    TEST_CASE(MultiplicationOperator_GivenRotationTransformAndIdentityTransform_ReturnsRotationTransform)
    {
        const Matrix4d rotation(Matrix4d::make_rotation_z(deg_to_rad(90.0)));

        const Transformd a(Transformd::from_local_to_parent(rotation));
        const Transformd b(Transformd::identity());

        const Transformd result = a * b;

        EXPECT_EQ(rotation, result.get_local_to_parent());
    }
}

TEST_SUITE(Foundation_Math_TransformInterpolator)
{
    TEST_CASE(Evaluate_GivenScalingComponents_ReturnsValidScalingInterpolationTransform)
    {
        const Transformd from(Transformd::identity());
        const Transformd to(Transformd::from_local_to_parent(Matrix4d::make_scaling(Vector3d(3.0, 5.0, 0.6))));
        const TransformInterpolatord interpolator(from, to);

        Transformd result;
        interpolator.evaluate(0.5, result);

        EXPECT_FEQ(Matrix4d::identity(), result.get_local_to_parent() * result.get_parent_to_local());
        EXPECT_FEQ(Vector3d(2.0, 3.0, 0.8), result.get_local_to_parent().extract_matrix3().extract_scaling());
    }

    TEST_CASE(Evaluate_GivenIdenticalFromAndToMatricesWithMirroring_ReturnsInputTransform)
    {
        static const double Values[] =
        {
            -1.0, 0.0, 0.0, 4.0,
             0.0, 1.0, 0.0, 6.0,
             0.0, 0.0, 1.0, 8.0,
             0.0, 0.0, 0.0, 1.0
        };

        const auto transform(Transformd::from_local_to_parent(Matrix4d::from_array(Values)));
        const TransformInterpolatord interpolator(transform, transform);

        Transformd result;
        interpolator.evaluate(0.5, result);

        EXPECT_FEQ(transform, result);
    }

    TEST_CASE(Evaluate_GivenRealWorldMatricesWithMirroring_ReturnsValidInterpolationTransform)
    {
        static const double FromValues[] =
        {
            -0.99702130594062099,   0.077087478205260004, -0.0025112020321310003,   0.76365516185966298,
             0.077096829499781999,  0.99701625983943298,  -0.0039071886356200000, 146.27250157945070,
            -0.0022025165107730001, 0.0040890970283240001, 0.99998953373952104,     8.9871358181588690,
             0.00000000000000000,   0.00000000000000000,   0.00000000000000000,     1.0000000000000000
        };

        static const double ToValues[] =
        {
            -0.99665231953250000,   0.081673207457143002, -0.0036906925368350003,   0.65271732495429602,
             0.081677812416244000,  0.99665822601435605,  -0.0011578872622540000, 145.34519371726373,
            -0.0035839072864200005, 0.0014554931967890000, 0.99999257746504999,     9.0022056937678983,
             0.00000000000000000,   0.00000000000000000,   0.00000000000000000,     1.0000000000000000
        };

        const TransformInterpolatord interpolator(
            Transformd::from_local_to_parent(Matrix4d::from_array(FromValues)),
            Transformd::from_local_to_parent(Matrix4d::from_array(ToValues)));

        Transformd result;
        interpolator.evaluate(0.024320000000000008, result);

        EXPECT_FEQ_EPS(
            Matrix4d::identity(),
            result.get_local_to_parent() * result.get_parent_to_local(),
            1.0e-9);
    }

    TEST_CASE(VisualizeTransform)
    {
        const Transformd from(Transformd::identity());

        const Transformd to(
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(1.0, 0.0, 0.0)) *
                Matrix4d::make_rotation_x(Pi<double>())));

        const TransformInterpolatord interpolator(from, to);

        VPythonFile file("unit tests/outputs/test_transform_transforminterpolator.py");

        const size_t FrameCount = 20;

        for (size_t i = 0; i < FrameCount; ++i)
        {
            const double t = static_cast<double>(i) / (FrameCount - 1);

            Transformd transform;
            interpolator.evaluate(t, transform);

            const Vector3d origin = transform.get_parent_origin();
            const Vector3d axis = transform.get_parent_y();

            file.draw_arrow(origin, origin + axis);
        }
    }
}
