
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

/*
template<typename T>
T microfacet_attenuation_function(
    const T        cos_on,
    const T        cos_in,
    const T        cos_hn,
    const T        cos_oh)
{
    const T rcp_cos_oh = 1.0 / cos_oh;
    const T a = 2.0 * cos_hn * cos_on * rcp_cos_oh;
    const T b = 2.0 * cos_hn * cos_in * rcp_cos_oh;
    return std::min(a, b, T(1));
}
*/

template <typename T>
class BlinnMDF2
{
  public:
    // Sample the product of the MDF and |H.n|.
    Vector<T, 3> sample(
        const Vector<T, 2>& s,
        const T e,
        const T unused  = T(0),
        const T unused2 = T(0)) const
    {
        return Vector<T,3>();
    }

    // This would compute D*G (TODO: more params are needed for this).
    T evaluate(
        const T cos_alpha,
        const T e,
        const T unused  = T(0),
        const T unused2 = T(0)) const
    {
        return 0.0f;
    }

    T evaluate_pdf(
        const T cos_alpha,
        const T e,
        const T unused  = T(0),
        const T unused2 = T(0)) const
    {
        return 0.0f;
    }
};

template<typename T>
class BeckmannMDF2
{
};

template<typename T>
class BerryMDF
{
};

template<typename T>
class GGXMDF2
{
};

template<typename T>
class WardMDF2
{
};

//
// AnyMDF, a polymorphic MDF wrapper.
//

template<typename T>
class AnyMDF : NonCopyable
{
  public:
    
    AnyMDF() : m_model(0) {}
    
    template<typename MDF>
    explicit AnyMDF(const MDF& mdf)
    {
        m_model = new Model<MDF>(mdf);
    }
    
    ~AnyMDF()
    {
        delete m_model;
    }
    
  private:
    
    struct Interface
    {
        virtual ~Interface() {}
    };
    
    template<typename MDF>
    struct Model : public Interface
    {
        explicit Model(const MDF& mdf) : m_mdf(mdf) {}

        MDF m_mdf; 
    };

    Interface *m_model;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_MICROFACET_H
