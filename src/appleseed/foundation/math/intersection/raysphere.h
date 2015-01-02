
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

#ifndef APPLESEED_FOUNDATION_MATH_INTERSECTION_RAYSPHERE_H
#define APPLESEED_FOUNDATION_MATH_INTERSECTION_RAYSPHERE_H

// appleseed.foundation headers.
#include "foundation/math/ray.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation
{

/*
        // compute the intersection between a ray and the sphere
        double intersect_sphere(const Ray3d& r)
        {
            const Vector3d v = m_sphere_center - r.m_org;
            const double   a = dot(v, r.m_dir);
            const double   b = a * a - dot(v, v) + double(1.0);

            if (b < 0.0)
                return m_huge;        // no intersection

            const double c  = sqrt(b);
            double t = a - c;

            if (t < 0.0)
            {
                t = a + c;
                if (t < 0.0)
                    return m_huge;    // no intersection
            }

            return t;
        }
*/

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_INTERSECTION_RAYSPHERE_H
