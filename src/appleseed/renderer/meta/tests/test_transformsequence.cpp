
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
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Utility_TransformSequence)
{
    TEST_CASE(Empty_GivenDefaultConstructedSequence_ReturnsTrue)
    {
        TransformSequence sequence;

        EXPECT_TRUE(sequence.empty());
    }

    TEST_CASE(Empty_AfterSettingOneTransform_ReturnsFalse)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, Transformd(Matrix4d::translation(Vector3d(1.0, 2.0, 3.0))));

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
        sequence.set_transform(1.0, Transformd(Matrix4d::translation(Vector3d(1.0, 2.0, 3.0))));

        EXPECT_EQ(1, sequence.size());
    }

    TEST_CASE(Size_AfterSettingTwoTransformsAtDistinctTimes_ReturnsTwo)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, Transformd(Matrix4d::translation(Vector3d(1.0, 2.0, 3.0))));
        sequence.set_transform(3.0, Transformd(Matrix4d::translation(Vector3d(4.0, 5.0, 6.0))));

        EXPECT_EQ(2, sequence.size());
    }

    TEST_CASE(Size_AfterSettingTwoTransformsAtSameTime_ReturnsOne)
    {
        TransformSequence sequence;
        sequence.set_transform(1.0, Transformd(Matrix4d::translation(Vector3d(1.0, 2.0, 3.0))));
        sequence.set_transform(1.0, Transformd(Matrix4d::translation(Vector3d(4.0, 5.0, 6.0))));

        EXPECT_EQ(1, sequence.size());
    }

    TEST_CASE(SetTransform_GivenNewTime_AddsTransform)
    {
        const Transformd ExpectedTransform(Matrix4d::translation(Vector3d(1.0, 2.0, 3.0)));

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
        const Transformd OldTransform(Matrix4d::translation(Vector3d(1.0, 2.0, 3.0)));
        const Transformd NewTransform(Matrix4d::translation(Vector3d(4.0, 5.0, 6.0)));

        TransformSequence sequence;
        sequence.set_transform(1.0, OldTransform);
        sequence.set_transform(1.0, NewTransform);

        double time;
        Transformd transform;
        sequence.get_transform(0, time, transform);

        EXPECT_EQ(1.0, time);
        EXPECT_EQ(NewTransform, transform);
    }

    TEST_CASE(Evaluate_GivenSingleTransform_ReturnsTransformRegardlessOfTime)
    {
        const Transformd ExpectedTransform(Matrix4d::translation(Vector3d(1.0, 2.0, 3.0)));

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
          : m_expected_first_transform(Matrix4d::translation(Vector3d(1.0, 2.0, 3.0)))
          , m_expected_second_transform(Matrix4d::translation(Vector3d(4.0, 5.0, 6.0)))
        {
            m_sequence.set_transform(1.0, m_expected_first_transform);
            m_sequence.set_transform(3.0, m_expected_second_transform);
            m_sequence.prepare();
        }
    };

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

        EXPECT_FEQ(interpolator.evaluate(0.5), m_sequence.evaluate(2.0));
    }

    TEST_CASE(Evaluate_GivenTwoTransformsSetInReverseOrder_ReturnsCorrectlyInterpolatedTransform)
    {
        const Transformd ExpectedFirstTransform(Matrix4d::translation(Vector3d(1.0, 2.0, 3.0)));
        const Transformd ExpectedSecondTransform(Matrix4d::translation(Vector3d(4.0, 5.0, 6.0)));

        TransformSequence sequence;
        sequence.set_transform(3.0, ExpectedSecondTransform);
        sequence.set_transform(1.0, ExpectedFirstTransform);
        sequence.prepare();

        const TransformInterpolatord interpolator(
            ExpectedFirstTransform,
            ExpectedSecondTransform);

        EXPECT_FEQ(interpolator.evaluate(0.5), sequence.evaluate(2.0));
    }
}
