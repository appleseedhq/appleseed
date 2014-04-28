
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
        const T             ax,
        const T             ay,
        const T             e) const
    {
        assert(s[0] >= T(0.0) && s[0] < T(1.0));
        assert(s[1] >= T(0.0) && s[1] < T(1.0));

        return do_sample(s, ax, ay, e);
    }

    virtual T evaluateD(
        const T ax,
        const T ay,
        const T e) const = 0;

    virtual T evaluateG(
        const T ax,
        const T ay,
        const T e) const = 0;

    virtual T evaluate_pdf(
        const T ax,
        const T ay,
        const T e) const = 0;

  private:

    virtual Vector<T,3> do_sample(
        const Vector<T,2>&  s,
        const T             ax,
        const T             ay,
        const T             e) const = 0;
};

// Isotropic Blinn here...
template <typename T>
struct BlinnMDF2
  : public MDF<T>
{
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
