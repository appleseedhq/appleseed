
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

// appleseed.foundation headers.
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

BENCHMARK_SUITE(Foundation_Math_RayInfo)
{
    struct Fixture
    {
        Ray3d               m_ray;
        volatile double     m_rcp_dir[3];
        volatile size_t     m_sgn_dir[3];

        Fixture()
          : m_ray(Vector3d(0.0, 0.0, -2.0), Vector3d(0.1, -0.1, 1.0))
        {
        }
    };

    BENCHMARK_CASE_F(RayInfo3d, Fixture)
    {
        const RayInfo3d ray_info(m_ray);

        m_rcp_dir[0] = ray_info.m_rcp_dir[0];
        m_rcp_dir[1] = ray_info.m_rcp_dir[1];
        m_rcp_dir[2] = ray_info.m_rcp_dir[2];

        m_sgn_dir[0] = ray_info.m_sgn_dir[0];
        m_sgn_dir[1] = ray_info.m_sgn_dir[1];
        m_sgn_dir[2] = ray_info.m_sgn_dir[2];
    }
}
