
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

template <typename T>
class BlinnMDF2
{
  public:
    // Sample the product of the MDF and |H.n|.
    Vector<T, 3> sample(
        const T e, 
        const T unused, 
        const Vector<T, 2>& s) const
    {
        assert(s[0] >= T(0.0) && s[0] < T(1.0));
        assert(s[1] >= T(0.0) && s[1] < T(1.0));

        const T cos_alpha = std::pow(T(1.0) - s[0], T(1.0) / (e + T(2.0)));
        const T sin_alpha = std::sqrt(T(1.0) - cos_alpha * cos_alpha);
        const T phi = TwoPi * s[1];

        return Vector<T, 3>::unit_vector(cos_alpha, sin_alpha, std::cos(phi), std::sin(phi));        
    }

    // This would compute D*G (TODO: more params are needed for this).
    T evaluate(
        const T e, 
        const T unused, 
        const T cos_alpha) const
    {
        assert(cos_alpha >= T(0.0));

        return (e + T(2.0)) * RcpTwoPi * std::pow(cos_alpha, e);        
    }

    T evaluate_pdf(
        const T e, 
        const T unused,
        const T cos_alpha) const
    {
        assert(cos_alpha >= T(0.0));

        return (e + T(2.0)) * RcpTwoPi * std::pow(cos_alpha, e + T(1.0));
    }
};

//
// AnyMDF, a polymorphic MDF wrapper.
//

template<typename T>
class AnyMDF : NonCopyable
{
  public:
    
    template<typename MDF>
    AnyMDF(const MDF& mdf)
    {
        m_model = new Model<MDF>(mdf);
    }
    
    ~AnyMDF()
    {
        delete m_model;
    }
    
    Vector<T, 3> sample(
        const T ax, 
        const T ay, 
        const Vector<T, 2>& s) const
    {
        return m_model->sample(ax, ay, s);
    }

    T evaluate(
        const T ax, 
        const T ay,
        const T cos_alpha) const
    {
        return m_model->evaluate(ax, ay, cos_alpha);
    }

    T evaluate_pdf(
        const T ax, 
        const T ay,
        const T cos_alpha) const
    {
        return m_model->evaluate_pdf(ax, ay, cos_alpha);
    }
    
  private:
    
    struct Interface
    {
        virtual ~Interface() {}
        
        virtual Vector<T, 3> sample(
            const T ax, 
            const T ay,
            const Vector<T, 2>& s) const = 0;
        
        virtual T evaluate(
            const T ax, 
            const T ay,
            const T cos_alpha) const = 0;
        
        virtual T evaluate_pdf(
            const T ax, 
            const T ay,
            const T cos_alpha) const = 0;
    };
    
    template<typename MDF>
    struct Model : public Interface
    {
       Model(const MDF& mdf) : m_mdf(mdf) {}

       virtual Vector<T, 3> sample(
            const T ax, 
            const T ay,
            const Vector<T, 2>& s) const OVERRIDE
       {
           return m_mdf.sample(ax, ay, s);
       }
       
       virtual T evaluate(
            const T ax, 
            const T ay,
            const T cos_alpha) const OVERRIDE
       {
           return m_mdf.evaluate(ax, ay, cos_alpha);
       }

       virtual T evaluate_pdf(
            const T ax, 
            const T ay, 
            const T cos_alpha) const OVERRIDE
       {
           return m_mdf.evaluate_pdf(ax, ay, cos_alpha);
       }

       MDF m_mdf; 
    };

    Interface *m_model;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_MICROFACET_H
