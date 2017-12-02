
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
#include "foundation/math/fp.h"
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Math_Ray)
{
    TEST_CASE(ConversionConstructor_PreservesValues)
    {
        const Ray3f ray(Vector3f(1.0f, 2.0f, 3.0f), Vector3f(4.0f, 5.0f, 6.0f), 7.0f, 8.0f);

        const Ray3d copy(ray);

        EXPECT_EQ(Vector3d(ray.m_org), copy.m_org);
        EXPECT_EQ(Vector3d(ray.m_dir), copy.m_dir);
        EXPECT_EQ(static_cast<double>(ray.m_tmin), copy.m_tmin);
        EXPECT_EQ(static_cast<double>(ray.m_tmax), copy.m_tmax);
    }
}

TEST_SUITE(Foundation_Math_RayInfo)
{
#pragma warning (push)
#pragma warning (disable : 4723)    // potential division by 0

    TEST_CASE(Constructor_ComputesDirectionReciprocal)
    {
        const Ray3d ray(Vector3d(0.0), Vector3d(-2.0, 0.0, 2.0));

        const RayInfo3d ray_info(ray);

        EXPECT_FEQ(-0.5, ray_info.m_rcp_dir[0]);
        EXPECT_TRUE(FP<double>::is_pos_inf(ray_info.m_rcp_dir[1]));
        EXPECT_FEQ(0.5, ray_info.m_rcp_dir[2]);
    }

    TEST_CASE(Constructor_ComputesDirectionSign)
    {
        const Ray3d ray(Vector3d(0.0), Vector3d(-2.0, 0.0, 2.0));

        const RayInfo3d ray_info(ray);

        EXPECT_EQ(0, ray_info.m_sgn_dir[0]);
        EXPECT_EQ(1, ray_info.m_sgn_dir[1]);
        EXPECT_EQ(1, ray_info.m_sgn_dir[2]);
    }

#pragma warning (pop)

    TEST_CASE(ConversionConstructor_PreservesValues)
    {
        const Ray3f ray(Vector3f(1.0f, 2.0f, 3.0f), Vector3f(4.0f, 5.0f, 6.0f), 7.0f, 8.0f);
        const RayInfo3f ray_info(ray);

        const RayInfo3d copy(ray_info);

        EXPECT_EQ(static_cast<double>(ray_info.m_rcp_dir[0]), copy.m_rcp_dir[0]);
        EXPECT_EQ(static_cast<double>(ray_info.m_rcp_dir[1]), copy.m_rcp_dir[1]);
        EXPECT_EQ(static_cast<double>(ray_info.m_rcp_dir[2]), copy.m_rcp_dir[2]);
        EXPECT_EQ(ray_info.m_sgn_dir[0], copy.m_sgn_dir[0]);
        EXPECT_EQ(ray_info.m_sgn_dir[1], copy.m_sgn_dir[1]);
        EXPECT_EQ(ray_info.m_sgn_dir[2], copy.m_sgn_dir[2]);
    }
}
