
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

// Isotropic Blinn here...
template <typename T>
struct BlinnMDF2
{
  public:
    // Sample the product of the MDF and |H.n|.
    Vector<T, 3> sample(
        const Vector<T,2>& s,
        const T e,
        const T unused  = T(0),
        const T unused2 = T(0)) const
    {
        assert(s[0] >= T(0.0) && s[0] < T(1.0));
        assert(s[1] >= T(0.0) && s[1] < T(1.0));

        return Vector<T,3>();
    }

    T evaluateD(
        const T e,
        const T unused  = T(0),
        const T unused2 = T(0)) const
    {
        return T(0);
    }

    T evaluateG(
        const T e,
        const T unused  = T(0),
        const T unused2 = T(0)) const
    {
        return 0.0f;
    }

    T evaluate_pdf(
        const T e,
        const T unused  = T(0),
        const T unused2 = T(0)) const
    {
        return 0.0f;
    }
};

// Anisotropic Beckmann here...
template<typename T>
struct BeckmannMDF2
{
};

// Anisotropic GTR with exponent == 1 here...
template<typename T>
struct BerryMDF2
{
};

// Anisotropic GTR with exponent == 2 here...
template<typename T>
struct GGXMDF2
{
};

// AnyMDF, a polymorphic MDF wrapper.
// It's here, instead of in microfacetbrdf2.cpp 
// because this will also be used by a future microfacetbtdf.
template<typename T>
class AnyMDF : NonCopyable
{
  public:

    template<typename MDF>
    explicit AnyMDF(const MDF& mdf)
    {
        m_model = new Model<MDF>(mdf);
    }

    ~AnyMDF()
    {
        delete m_model;
    }

    inline Vector<T, 3> sample(
        const Vector<T,2>& s,
        const T ax,
        const T ay,
        const T e) const
    {
        return m_model->sample(s, ax, ay, e);
    }

    inline T evaluate(
        const T ax,
        const T ay,
        const T e) const
    {
        return m_model->evaluate(ax, ay, e);
    }

    inline T evaluate_pdf(
        const T ax,
        const T ay,
        const T e) const
    {
        return m_model->evaluate_pdf(ax, ay, e);
    }

  private:

    struct Interface
    {
        virtual ~Interface() {}

        virtual Vector<T, 3> sample(
            const Vector<T,2>& s,
            const T ax,
            const T ay,
            const T e) const = 0;

        virtual T evaluate(
            const T ax,
            const T ay,
            const T e) const = 0;

        virtual T evaluate_pdf(
            const T ax,
            const T ay,
            const T e) const = 0;
    };

    template<typename MDF>
    struct Model : public Interface
    {
        explicit Model(const MDF& mdf) : m_mdf(mdf) {}

        virtual Vector<T, 3> sample(
            const Vector<T,2>& s,
            const T ax,
            const T ay,
            const T e) const OVERRIDE
        {
            return m_model->sample(s, ax, ay, e);
        }

        virtual T evaluate(
            const T ax,
            const T ay,
            const T e) const OVERRIDE
        {
            return 
                m_model->evaluateD(ax, ay, e) *
                m_model->evaluateG(ax, ay, e);
        }

        virtual T evaluate_pdf(
            const T ax,
            const T ay,
            const T e) const OVERRIDE
        {
            return m_model->evaluate_pdf(ax, ay, e);
        }

        MDF m_mdf; 
    };

    Interface *m_model;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_MICROFACET_H
