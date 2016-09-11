
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/matrix.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"
#include "foundation/utility/vpythonfile.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Utility_TransformSequence)
{
    TEST_CASE(Clear_GivenSequenceWithOneTransform_RemovesTransform)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, Transformd::identity());

        sequence.clear();

        EXPECT_TRUE(sequence.empty());
    }

    TEST_CASE(SetTransform_GivenTimeAtWhichNoTransformExists_AddsTransform)
    {
        const Transformd ExpectedTransform(
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))));

        TransformSequence sequence;
        sequence.set_transform(1.0, ExpectedTransform);

        double time;
        Transformd transform;
        sequence.get_transform(0, time, transform);

        EXPECT_EQ(1.0, time);
        EXPECT_EQ(ExpectedTransform, transform);
    }

    TEST_CASE(SetTransform_GivenTimeOfExistingTransform_ReplacesTransform)
    {
        const Transformd OldTransform(
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))));
        const Transformd NewTransform(
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(4.0, 5.0, 6.0))));

        TransformSequence sequence;
        sequence.set_transform(1.0, OldTransform);
        sequence.set_transform(1.0, NewTransform);

        double time;
        Transformd transform;
        sequence.get_transform(0, time, transform);

        EXPECT_EQ(1.0, time);
        EXPECT_EQ(NewTransform, transform);
    }

    TEST_CASE(GetEarliestTransform_EmptySequence_ReturnsIdentity)
    {
        const TransformSequence sequence;

        EXPECT_EQ(Transformd::identity(), sequence.get_earliest_transform());
    }

    TEST_CASE(GetEarliestTransform_EarliestTransformIsFirst)
    {
        const Transformd Transform1(
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))));
        const Transformd Transform2(
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(4.0, 5.0, 6.0))));

        TransformSequence sequence;
        sequence.set_transform(1.0, Transform1);
        sequence.set_transform(2.0, Transform2);

        EXPECT_EQ(Transform1, sequence.get_earliest_transform());
    }

    TEST_CASE(GetEarliestTransform_EarliestTransformIsLast)
    {
        const Transformd Transform1(
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))));
        const Transformd Transform2(
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(4.0, 5.0, 6.0))));

        TransformSequence sequence;
        sequence.set_transform(2.0, Transform2);
        sequence.set_transform(1.0, Transform1);

        EXPECT_EQ(Transform1, sequence.get_earliest_transform());
    }

    TEST_CASE(Empty_GivenDefaultConstructedSequence_ReturnsTrue)
    {
        TransformSequence sequence;

        EXPECT_TRUE(sequence.empty());
    }

    TEST_CASE(Empty_AfterSettingOneTransform_ReturnsFalse)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, Transformd::identity());

        EXPECT_FALSE(sequence.empty());
    }

    TEST_CASE(Size_GivenDefaultConstructedSequence_ReturnsZero)
    {
        TransformSequence sequence;

        EXPECT_EQ(0, sequence.size());
    }

    TEST_CASE(Size_AfterSettingOneTransform_ReturnsOne)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, Transformd::identity());

        EXPECT_EQ(1, sequence.size());
    }

    TEST_CASE(Size_AfterSettingTwoTransformsAtDistinctTimes_ReturnsTwo)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, Transformd::identity());
        sequence.set_transform(3.0, Transformd::identity());

        EXPECT_EQ(2, sequence.size());
    }

    TEST_CASE(Size_AfterSettingTwoTransformsAtSameTime_ReturnsOne)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, Transformd::identity());
        sequence.set_transform(1.0, Transformd::identity());

        EXPECT_EQ(1, sequence.size());
    }

    static const Transformd A = Transformd::identity();
    static const Transformd B =
        Transformd::from_local_to_parent(
            Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0)));

    double get_time(const TransformSequence& sequence, const size_t index)
    {
        double time;
        Transformd transform;
        sequence.get_transform(index, time, transform);
        return time;
    }

    Transformd get_transform(const TransformSequence& sequence, const size_t index)
    {
        double time;
        Transformd transform;
        sequence.get_transform(index, time, transform);
        return transform;
    }

    TEST_CASE(Optimize_GivenEmptySequence_DoesNothing)
    {
        TransformSequence sequence;

        sequence.optimize();

        EXPECT_TRUE(sequence.empty());
    }

    TEST_CASE(Optimize_GivenA_DoesNothing)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, A);

        sequence.optimize();

        ASSERT_EQ(1, sequence.size());
        EXPECT_EQ(A, get_transform(sequence, 0));
    }

    TEST_CASE(Optimize_GivenAB_DoesNothing)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, A);
        sequence.set_transform(2.0, B);

        sequence.optimize();

        ASSERT_EQ(2, sequence.size());
        EXPECT_EQ(A, get_transform(sequence, 0));
        EXPECT_EQ(B, get_transform(sequence, 1));
    }

    TEST_CASE(Optimize_GivenAA_CollapsesToA)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, A);
        sequence.set_transform(2.0, A);

        sequence.optimize();

        ASSERT_EQ(1, sequence.size());
        EXPECT_EQ(A, get_transform(sequence, 0));
        EXPECT_EQ(2.0, get_time(sequence, 0));
    }

    TEST_CASE(Optimize_GivenAAB_CollapsesToAB)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, A);
        sequence.set_transform(2.0, A);
        sequence.set_transform(3.0, B);

        sequence.optimize();

        ASSERT_EQ(2, sequence.size());
        EXPECT_EQ(A, get_transform(sequence, 0));
        EXPECT_EQ(2.0, get_time(sequence, 0));
        EXPECT_EQ(B, get_transform(sequence, 1));
        EXPECT_EQ(3.0, get_time(sequence, 1));
    }

    TEST_CASE(Optimize_GivenBAA_CollapsesToBA)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, B);
        sequence.set_transform(2.0, A);
        sequence.set_transform(3.0, A);

        sequence.optimize();

        ASSERT_EQ(2, sequence.size());
        EXPECT_EQ(B, get_transform(sequence, 0));
        EXPECT_EQ(1.0, get_time(sequence, 0));
        EXPECT_EQ(A, get_transform(sequence, 1));
        EXPECT_EQ(2.0, get_time(sequence, 1));
    }

    TEST_CASE(Optimize_GivenABBA_DoesNothing)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, A);
        sequence.set_transform(2.0, B);
        sequence.set_transform(3.0, B);
        sequence.set_transform(4.0, A);

        sequence.optimize();

        ASSERT_EQ(4, sequence.size());
        EXPECT_EQ(A, get_transform(sequence, 0));
        EXPECT_EQ(B, get_transform(sequence, 1));
        EXPECT_EQ(B, get_transform(sequence, 2));
        EXPECT_EQ(A, get_transform(sequence, 3));
    }

    TEST_CASE(Prepare_GivenEmptySequence_ReturnsTrue)
    {
        TransformSequence sequence;

        const bool success = sequence.prepare();

        EXPECT_TRUE(success);
    }

    TEST_CASE(Evaluate_GivenNoTransform_ReturnsIdentityTransformRegardlessOfTime)
    {
        const Transformd ExpectedTransform(Transformd::identity());

        TransformSequence sequence;
        sequence.prepare();

        EXPECT_EQ(ExpectedTransform, sequence.evaluate(0.0));
        EXPECT_EQ(ExpectedTransform, sequence.evaluate(1.0));
        EXPECT_EQ(ExpectedTransform, sequence.evaluate(2.0));
    }

    TEST_CASE(Evaluate_GivenSingleTransform_ReturnsTransformRegardlessOfTime)
    {
        const Transformd ExpectedTransform(
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))));

        TransformSequence sequence;
        sequence.set_transform(1.0, ExpectedTransform);
        sequence.prepare();

        EXPECT_EQ(ExpectedTransform, sequence.evaluate(0.0));
        EXPECT_EQ(ExpectedTransform, sequence.evaluate(1.0));
        EXPECT_EQ(ExpectedTransform, sequence.evaluate(2.0));
    }

    struct TwoTransformsFixture
    {
        const Transformd    m_expected_first_transform;
        const Transformd    m_expected_second_transform;
        TransformSequence   m_sequence;

        TwoTransformsFixture()
          : m_expected_first_transform(
              Transformd::from_local_to_parent(
                  Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))))
          , m_expected_second_transform(
              Transformd::from_local_to_parent(
                  Matrix4d::make_translation(Vector3d(4.0, 5.0, 6.0))))
        {
            m_sequence.set_transform(1.0, m_expected_first_transform);
            m_sequence.set_transform(3.0, m_expected_second_transform);
            m_sequence.prepare();
        }
    };

    TEST_CASE_F(CopyConstructor_CopiesTransformInterpolators, TwoTransformsFixture)
    {
        TransformSequence copy(m_sequence);

        EXPECT_EQ(m_expected_first_transform, copy.evaluate(0.0));
    }

    TEST_CASE_F(Evaluate_GivenTwoTransforms_WhenTimeBeforeFirstTransform_ReturnsFirstTransform, TwoTransformsFixture)
    {
        EXPECT_EQ(m_expected_first_transform, m_sequence.evaluate(0.0));
    }

    TEST_CASE_F(Evaluate_GivenTwoTransforms_WhenTimeAfterLastTransform_ReturnsLastTransform, TwoTransformsFixture)
    {
        EXPECT_EQ(m_expected_second_transform, m_sequence.evaluate(4.0));
    }

    TEST_CASE_F(Evaluate_GivenTwoTransforms_WhenTimeBetweenTwoTransforms_ReturnsInterpolatedTransform, TwoTransformsFixture)
    {
        const TransformInterpolatord interpolator(
            m_expected_first_transform,
            m_expected_second_transform);

        Transformd expected;
        interpolator.evaluate(0.5, expected);

        EXPECT_FEQ(expected, m_sequence.evaluate(2.0));
    }

    TEST_CASE(Evaluate_GivenTwoTransformsSetInReverseOrder_ReturnsCorrectlyInterpolatedTransform)
    {
        const Transformd ExpectedFirstTransform(
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))));
        const Transformd ExpectedSecondTransform(
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(4.0, 5.0, 6.0))));

        TransformSequence sequence;
        sequence.set_transform(3.0, ExpectedSecondTransform);
        sequence.set_transform(1.0, ExpectedFirstTransform);
        sequence.prepare();

        const TransformInterpolatord interpolator(
            ExpectedFirstTransform,
            ExpectedSecondTransform);

        Transformd expected;
        interpolator.evaluate(0.5, expected);

        EXPECT_FEQ(expected, sequence.evaluate(2.0));
    }

    TEST_CASE(CompositionOperator_GivenTwoEmptyTransformSequences_ReturnsEmptyTransformSequence)
    {
        TransformSequence seq1, seq2;

        const TransformSequence result = seq1 * seq2;

        EXPECT_TRUE(result.empty());
    }

    TEST_CASE(CompositionOperator_GivenEmptyAndNonEmptyTransformSequences_ReturnsNonEmptyTransformSequence)
    {
        TransformSequence seq1;
        seq1.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))));

        TransformSequence seq2;

        const TransformSequence result = seq1 * seq2;

        ASSERT_EQ(1, result.size());

        double time;
        Transformd transform;
        result.get_transform(0, time, transform);

        EXPECT_EQ(1.0, time);
        EXPECT_EQ(
            Transformd::from_local_to_parent(Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))),
            transform);
    }

    TEST_CASE(CompositionOperator_GivenTwoNonEmptyTransformSequencesWithCoincidentTimes_ReturnsTransformSequenceWithSameNumberOfTransforms)
    {
        TransformSequence seq1;
        seq1.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(1.0, 0, 0), HalfPi)));

        TransformSequence seq2;
        seq2.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))));

        const TransformSequence result = seq1 * seq2;

        ASSERT_EQ(1, result.size());

        double time;
        Transformd transform;
        result.get_transform(0, time, transform);

        EXPECT_EQ(1.0, time);
        EXPECT_EQ(
              Transformd::from_local_to_parent(Matrix4d::make_rotation(Vector3d(1.0, 0, 0), HalfPi))
            * Transformd::from_local_to_parent(Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))),
            transform);
    }

    TEST_CASE(CompositionOperator_GivenTwoNonEmptyTransformSequencesWithDistinctTimes_ReturnsTransformSequenceWithCumulatedNumberOfTransforms)
    {
        TransformSequence seq1;
        seq1.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(1.0, 0, 0), HalfPi)));

        TransformSequence seq2;
        seq2.set_transform(
            2.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))));

        const TransformSequence result = seq1 * seq2;

        ASSERT_EQ(2, result.size());

        {
            double time;
            Transformd transform;
            result.get_transform(0, time, transform);

            EXPECT_EQ(1.0, time);
            EXPECT_EQ(
                  Transformd::from_local_to_parent(Matrix4d::make_rotation(Vector3d(1.0, 0, 0), HalfPi))
                * Transformd::from_local_to_parent(Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))),
                transform);
        }

        {
            double time;
            Transformd transform;
            result.get_transform(1, time, transform);

            EXPECT_EQ(2.0, time);
            EXPECT_EQ(
                  Transformd::from_local_to_parent(Matrix4d::make_rotation(Vector3d(1.0, 0, 0), HalfPi))
                * Transformd::from_local_to_parent(Matrix4d::make_translation(Vector3d(1.0, 2.0, 3.0))),
                transform);
        }
    }

    TEST_CASE(ToParent_SmallPositiveRotation)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0)));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), Pi / 8)));
        sequence.prepare();

        const AABB3d bbox(Vector3d(1.0, 1.0, 0.0), Vector3d(1.0, 1.0, 0.0));
        const AABB3d motion_bbox = sequence.to_parent(bbox);

        EXPECT_FEQ_EPS(AABB3d(Vector3d(0.54119610014619690, 1.0, 0.0), Vector3d(1.0, 1.3065629648763766, 0.0)), motion_bbox, 1.0e-3);
    }

    TEST_CASE(ToParent_SmallNegativeRotation)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0)));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), -Pi / 8)));
        sequence.prepare();

        const AABB3d bbox(Vector3d(1.0, 1.0, 0.0), Vector3d(1.0, 1.0, 0.0));
        const AABB3d motion_bbox = sequence.to_parent(bbox);

        EXPECT_FEQ_EPS(AABB3d(Vector3d(1.0, 0.54119610014619690, 0.0), Vector3d(1.3065629648763766, 1.0, 0.0)), motion_bbox, 1.0e-3);
    }

    TEST_CASE(ToParent_LargePositiveRotation)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0)));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), Pi - Pi / 8)));
        sequence.prepare();

        const AABB3d bbox(Vector3d(1.0, 1.0, 0.0), Vector3d(1.0, 1.0, 0.0));
        const AABB3d motion_bbox = sequence.to_parent(bbox);

        EXPECT_FEQ_EPS(AABB3d(Vector3d(-1.4142135623730949, -0.54119610014619690, 0.0), Vector3d(1.0, 1.4142135623730949, 0.0)), motion_bbox, 1.0e-3);
    }

    TEST_CASE(ToParent_LargeNegativeRotation)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0)));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), -Pi + Pi / 8)));
        sequence.prepare();

        const AABB3d bbox(Vector3d(1.0, 1.0, 0.0), Vector3d(1.0, 1.0, 0.0));
        const AABB3d motion_bbox = sequence.to_parent(bbox);

        EXPECT_FEQ_EPS(AABB3d(Vector3d(-0.54119610014619690, -1.4142135623730951, 0.0), Vector3d(1.4142135623730949, 1.0, 0.0)), motion_bbox, 1.0e-3);
    }

    void visualize(
        const char*                 filename,
        const TransformSequence&    sequence,
        const AABB3d&               bbox)
    {
        const double Width = 0.01;
        const size_t Steps = 10;

        const AABB3d motion_bbox = sequence.to_parent(bbox);

        VPythonFile file(filename);

        file.draw_axes(Width);

        for (size_t i = 0; i < Steps; ++i)
        {
            const double t = fit<size_t, double>(i, 0, Steps - 1, 0.0, 1.0);
            const Transformd transform = sequence.evaluate(t);
            const char* color = i == 0 ? "color.yellow" : i == Steps - 1 ? "color.white" : "color.blue";

            file.draw_aabb(transform.to_parent(bbox), color, Width);
            file.draw_arrow(Vector3d(0.0), transform.point_to_parent(bbox.center()), color, Width);
        }

        file.draw_aabb(motion_bbox, "color.magenta", Width);
    }

    TEST_CASE(ToParent_GivenPoint_VisualizeSmallPositiveRotation)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0)));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), Pi / 8)));
        sequence.prepare();

        const AABB3d bbox(Vector3d(1.0, 1.0, 0.0), Vector3d(1.0, 1.0, 0.0));

        visualize(
            "unit tests/outputs/test_transformsequence_toparent_givenpoint_smallpositiverotation.py",
            sequence,
            bbox);
    }

    TEST_CASE(ToParent_GivenPoint_VisualizeSmallNegativeRotation)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0)));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), -Pi / 8)));
        sequence.prepare();

        const AABB3d bbox(Vector3d(1.0, 1.0, 0.0), Vector3d(1.0, 1.0, 0.0));

        visualize(
            "unit tests/outputs/test_transformsequence_toparent_givenpoint_smallnegativerotation.py",
            sequence,
            bbox);
    }

    TEST_CASE(ToParent_GivenPoint_VisualizeLargePositiveRotation)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0)));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), Pi - Pi / 8)));
        sequence.prepare();

        const AABB3d bbox(Vector3d(-1.0, -1.0, 0.0), Vector3d(-1.0, -1.0, 0.0));

        visualize(
            "unit tests/outputs/test_transformsequence_toparent_givenpoint_largepositiverotation.py",
            sequence,
            bbox);
    }

    TEST_CASE(ToParent_GivenPoint_VisualizeLargeNegativeRotation)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0)));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), -Pi + Pi / 8)));
        sequence.prepare();

        const AABB3d bbox(Vector3d(-1.0, -1.0, 0.0), Vector3d(-1.0, -1.0, 0.0));

        visualize(
            "unit tests/outputs/test_transformsequence_toparent_givenpoint_largenegativerotation.py",
            sequence,
            bbox);
    }

    TEST_CASE(ToParent_GivenAABB_VisualizeLargePositiveRotationWithUnitScaling)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0)));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), Pi - Pi / 8)));
        sequence.prepare();

        const AABB3d bbox(Vector3d(-2.0, -2.0, -0.5), Vector3d(-1.0, -1.0, 0.5));

        visualize(
            "unit tests/outputs/test_transformsequence_toparent_givenaabb_largepositiverotationwithunitscaling.py",
            sequence,
            bbox);
    }

    TEST_CASE(ToParent_GivenAABB_VisualizeLargePositiveRotationWithConstantScaling)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0) *
                Matrix4d::make_scaling(Vector3d(0.2))));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), Pi - Pi / 8) *
                Matrix4d::make_scaling(Vector3d(0.2))));
        sequence.prepare();

        const AABB3d bbox(Vector3d(-20.0, -20.0, -5.0), Vector3d(-10.0, -10.0, 5.0));

        visualize(
            "unit tests/outputs/test_transformsequence_toparent_givenaabb_largepositiverotationwithconstantscaling.py",
            sequence,
            bbox);
    }

    TEST_CASE(ToParent_GivenAABB_VisualizeLargePositiveRotationWithVaryingScaling)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0) *
                Matrix4d::make_scaling(Vector3d(0.1))));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), Pi - Pi / 8) *
                Matrix4d::make_scaling(Vector3d(0.2))));
        sequence.prepare();

        const AABB3d bbox(Vector3d(-20.0, -20.0, -5.0), Vector3d(-10.0, -10.0, 5.0));

        visualize(
            "unit tests/outputs/test_transformsequence_toparent_givenaabb_largepositiverotationwithvaryingscaling.py",
            sequence,
            bbox);
    }

    TEST_CASE(ToParent_GivenAABB_VisualizeLargePositiveRotationWithVaryingScalingAndTranslation)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0) *
                Matrix4d::make_scaling(Vector3d(0.1))));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(20.0, 0.0, 0.0)) *
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), Pi - Pi / 8) *
                Matrix4d::make_scaling(Vector3d(0.2))));
        sequence.prepare();

        const AABB3d bbox(Vector3d(-20.0, -20.0, -5.0), Vector3d(-10.0, -10.0, 5.0));

        visualize(
            "unit tests/outputs/test_transformsequence_toparent_givenaabb_largepositiverotationwithvaryingscalingandtranslation.py",
            sequence,
            bbox);
    }

    TEST_CASE(ToParent_GivenAABB_VisualizeLargeNegativeRotationWithUnitScaling)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0)));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), -Pi + Pi / 8)));
        sequence.prepare();

        const AABB3d bbox(Vector3d(-2.0, -2.0, -0.5), Vector3d(-1.0, -1.0, 0.5));

        visualize(
            "unit tests/outputs/test_transformsequence_toparent_givenaabb_largenegativerotationwithunitscaling.py",
            sequence,
            bbox);
    }

    TEST_CASE(ToParent_GivenAABB_VisualizeLargeNegativeRotationWithConstantScaling)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0) *
                Matrix4d::make_scaling(Vector3d(0.2))));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), -Pi + Pi / 8) *
                Matrix4d::make_scaling(Vector3d(0.2))));
        sequence.prepare();

        const AABB3d bbox(Vector3d(-20.0, -20.0, -5.0), Vector3d(-10.0, -10.0, 5.0));

        visualize(
            "unit tests/outputs/test_transformsequence_toparent_givenaabb_largenegativerotationwithconstantscaling.py",
            sequence,
            bbox);
    }

    TEST_CASE(ToParent_GivenAABB_VisualizeLargeNegativeRotationWithVaryingScaling)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), 0.0) *
                Matrix4d::make_scaling(Vector3d(0.1))));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(Vector3d(0.0, 0.0, 1.0), -Pi + Pi / 8) *
                Matrix4d::make_scaling(Vector3d(0.2))));
        sequence.prepare();

        const AABB3d bbox(Vector3d(-20.0, -20.0, -5.0), Vector3d(-10.0, -10.0, 5.0));

        visualize(
            "unit tests/outputs/test_transformsequence_toparent_givenaabb_largenegativerotationwithvaryingscaling.py",
            sequence,
            bbox);
    }

    TEST_CASE(ToParent_GivenAABB_VisualizeVaryingScalingOnly)
    {
        TransformSequence sequence;
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_scaling(Vector3d(0.1))));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(
                Matrix4d::make_scaling(Vector3d(0.2))));
        sequence.prepare();

        const AABB3d bbox(Vector3d(-20.0, -20.0, -5.0), Vector3d(-10.0, -10.0, 5.0));

        visualize(
            "unit tests/outputs/test_transformsequence_toparent_givenaabb_scalingonly.py",
            sequence,
            bbox);
    }

    TEST_CASE(TestSwapsHandednessNegativeScale1Axis)
    {
        TransformSequence sequence;

        Matrix4d m =
              Matrix4d::make_rotation_x(Pi / 4.0)
            * Matrix4d::make_scaling(Vector3d(1.0, 1.0, 0.5));
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(m));

        m =
              Matrix4d::make_rotation_x(Pi / 2.0)
            * Matrix4d::make_scaling(Vector3d(2.0, -3.0, 0.5));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(m));

        sequence.prepare();
        EXPECT_TRUE(sequence.can_swap_handedness());

        const Transformd xform = sequence.evaluate(0.5);
        EXPECT_EQ(xform.swaps_handedness(), sequence.swaps_handedness(xform));
    }

    TEST_CASE(TestSwapsHandednessNegativeScale2Axes)
    {
        TransformSequence sequence;

        Matrix4d m =
              Matrix4d::make_rotation_y(Pi / 2.0)
            * Matrix4d::make_scaling(Vector3d(-1.0, -2.0, 0.5));
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(m));

        m =
              Matrix4d::make_rotation_z(Pi / 3.0)
            * Matrix4d::make_scaling(Vector3d(-1.0, -1.0, 1.5));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(m));

        sequence.prepare();
        EXPECT_FALSE(sequence.can_swap_handedness());

        const Transformd xform = sequence.evaluate(0.5);
        EXPECT_EQ(xform.swaps_handedness(), sequence.swaps_handedness(xform));
    }

    TEST_CASE(TestSwapsHandednessPositiveAndNegativeScales)
    {
        TransformSequence sequence;

        Matrix4d m =
              Matrix4d::make_rotation_y(Pi / 2.0)
            * Matrix4d::make_scaling(Vector3d(1.0, 2.0, 0.5));
        sequence.set_transform(
            0.0,
            Transformd::from_local_to_parent(m));

        m =
              Matrix4d::make_rotation_z(Pi / 3.0)
            * Matrix4d::make_scaling(Vector3d(-1.0, 1.0, 1.5));
        sequence.set_transform(
            1.0,
            Transformd::from_local_to_parent(m));

        sequence.prepare();
        EXPECT_TRUE(sequence.can_swap_handedness());

        const Transformd xform = sequence.evaluate(0.5);
        EXPECT_EQ(xform.swaps_handedness(), sequence.swaps_handedness(xform));
    }
}
