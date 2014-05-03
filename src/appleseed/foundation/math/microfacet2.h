
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_MICROFACET2_H
#define APPLESEED_FOUNDATION_MATH_MICROFACET2_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>

namespace foundation
{

template <typename T>
struct MDF : NonCopyable
{
    virtual ~MDF() {}
};

template <typename T>
struct BeckmannMDF2
  : public MDF<T>
{
};

template <typename T>
struct GGXMDF2
  : public MDF<T>
{
    virtual T D(
        const Vector<T,3>& h,
        const T ax,
        const T ay) const
    {
       const T ax2 = square(ax);

       const T cos_theta_m = h[2];
       const T cos_theta_m2 = square(cos_theta_m);
       const T cos_theta_m4 = square(cos_theta_m2);
       
       if (ax != ay)
       {
           const T ay2 = ay * ay;
           const T sin_theta_m = std::sqrt(std::max(T(1) - cos_theta_m2, T(0)));
           const T inv_sin_theta_m = sin_theta_m > T(0) ? T(1) / sin_theta_m : T(0);
           const T cos_phi2 = square(h.x * inv_sin_theta_m);
           const T sin_phi2 = square(h.y * inv_sin_theta_m);
           const T tan_theta_m2 = (sin_theta_m * sin_theta_m) / cos_theta_m2;
           const T tmp = 1 + tan_theta_m2 * (cos_phi2 / ax2 + sin_phi2 / ay2);
           return T(1) / (Pi * ax * ay * cos_theta_m4 * square(tmp));
       }

       float tan_theta_m2 = (1 - cos_theta_m2) / cos_theta_m2;
       return ax2 / (Pi * cos_theta_m4 * square(ax2 + tan_theta_m2));
    }
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_MICROFACET_H
