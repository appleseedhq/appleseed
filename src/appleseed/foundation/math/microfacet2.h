
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

    inline Vector<T,3> sample(
        const Vector<T,2>&  s,
        // more params here...
        const T             ax,
        const T             ay,
        const T             exponent) const
    {
        assert(s[0] >= T(0.0) && s[0] < T(1.0));
        assert(s[1] >= T(0.0) && s[1] < T(1.0));

        return do_sample(s, ax, ay, e);
    }

    virtual T evaluateD(
        // more params here...
        const T             ax,
        const T             ay,
        const T             exponent) const = 0;

    virtual T evaluateG(
        // more params here...
        const T             ax,
        const T             ay,
        const T             exponent) const = 0;

    virtual T evaluate_pdf(
        // more params here...
        const T             ax,
        const T             ay,
        const T             exponent) const = 0;

  private:

    virtual Vector<T,3> do_sample(
        const Vector<T,2>&  s,
        // more params here...
        const T             ax,
        const T             ay,
        const T             exponent) const = 0;
};

// Isotropic Blinn here...
template <typename T>
struct BlinnMDF2
  : public MDF<T>
{
  private:
    virtual Vector<T,3> do_sample(
        const Vector<T,2>&  s,
        // more params here...
        const T             ax,
        const T             ay,
        const T             e) const OVERRIDE
    {
        const T cos_alpha = std::pow(T(1.0) - s[0], T(1.0) / (e + T(2.0)));
        const T sin_alpha = std::sqrt(T(1.0) - cos_alpha * cos_alpha);
        const T phi = TwoPi * s[1];
        return Vector<T, 3>::unit_vector(cos_alpha, sin_alpha, std::cos(phi), std::sin(phi));
    }
};

// Anisotropic Beckmann here...
template <typename T>
struct BeckmannMDF2
  : public MDF<T>
{
};

// Anisotropic GTR with exponent == 1 here...
template <typename T>
struct BerryMDF2
  : public MDF<T>
{
};

// Anisotropic GTR with exponent == 2 here...
template <typename T>
struct GGXMDF2
  : public MDF<T>
{
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_MICROFACET_H
