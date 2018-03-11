
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

// appleseed.renderer headers.
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/matrix.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/benchmark.h"

using namespace foundation;
using namespace renderer;

BENCHMARK_SUITE(Renderer_Utility_TransformSequence)
{
    struct Fixture
    {
        const AABB3d        m_bbox;
        TransformSequence   m_sequence;
        AABB3d              m_motion_bbox;

        Fixture()
          : m_bbox(Vector3d(-20.0, -20.0, -5.0), Vector3d(-10.0, -10.0, 5.0))
        {
            const Vector3d axis = normalize(Vector3d(0.1, 0.2, 1.0));
            m_sequence.set_transform(
                0.0f,
                Transformd::from_local_to_parent(
                    Matrix4d::make_rotation(axis, 0.0) *
                    Matrix4d::make_scaling(Vector3d(0.1))));
            m_sequence.set_transform(
                1.0f,
                Transformd::from_local_to_parent(
                    Matrix4d::make_rotation(axis, Pi<double>() - Pi<double>() / 8) *
                    Matrix4d::make_scaling(Vector3d(0.2))));
            m_sequence.prepare();
        }
    };

    BENCHMARK_CASE_F(ToParent, Fixture)
    {
        m_motion_bbox = m_sequence.to_parent(m_bbox);
    }
}
