
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_MATH_MICROFACET_H
#define APPLESEED_FOUNDATION_MATH_MICROFACET_H

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cmath>

namespace foundation
{

//
// Blinn-Phong Microfacet Distribution Function.
//
// References:
//
//   [1] Physically Based Rendering, first edition, pp. 444-447 and 681-684
//
//   [2] Microfacet Models for Refraction through Rough Surfaces
//       http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
//

template <typename T>
class BlinnMDF
{
  public:
    // e is the cosine exponent.
    explicit BlinnMDF(const T e);

    // Sample the product of the MDF and |H.n|.
    Vector<T, 3> sample(const Vector<T, 2>& s) const;

    T evaluate(const T cos_alpha) const;

    T evaluate_pdf(const T cos_alpha) const;

  private:
    const T m_e;
};


//
// Beckmann Microfacet Distribution Function.
//
// References:
//
//   [1] http://en.wikipedia.org/wiki/Specular_highlight#Beckmann_distribution
//
//   [2] A Microfacet Based Coupled Specular-Matte BRDF Model with Importance Sampling
//       http://sirkan.iit.bme.hu/~szirmay/scook.pdf
//

template <typename T>
class BeckmannMDF
{
  public:
    // m is the RMS slope of the surface microfacets (the roughness of the material).
    explicit BeckmannMDF(const T m);

    // Sample the product of the MDF and |H.n|.
    Vector<T, 3> sample(const Vector<T, 2>& s) const;

    T evaluate(const T cos_alpha) const;

    T evaluate_pdf(const T cos_alpha) const;

  private:
    const T m_m;
};


//
// Ward Microfacet Distribution Function.
//
// Reference:
//
//   A Microfacet Based Coupled Specular-Matte BRDF Model with Importance Sampling
//   http://sirkan.iit.bme.hu/~szirmay/scook.pdf
//
// Note: not a true MDF as it integrates to values less than 1!
//

template <typename T>
class WardMDF
{
  public:
    // m is the RMS slope of the surface microfacets (the roughness of the material).
    explicit WardMDF(const T m);

    // Sample the product of the MDF and |H.n|.
    Vector<T, 3> sample(const Vector<T, 2>& s) const;

    T evaluate(const T cos_alpha) const;

    T evaluate_pdf(const T cos_alpha) const;

  private:
    const T m_m;
};


//
// GGX Microfacet Distribution Function.
//
// Reference:
//
//   Microfacet Models for Refraction through Rough Surfaces
//   http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
//

template <typename T>
class GGXMDF
{
  public:
    // alpha_g is the peak width.
    explicit GGXMDF(const T alpha_g);

    // Sample the product of the MDF and |H.n|.
    Vector<T, 3> sample(const Vector<T, 2>& s) const;

    T evaluate(const T cos_alpha) const;

    T evaluate_pdf(const T cos_alpha) const;

  private:
    const T m_alpha_g;
};


//
// BlinnMDF class implementation.
//

template <typename T>
inline BlinnMDF<T>::BlinnMDF(const T e)
  : m_e(e)
{
}

template <typename T>
inline Vector<T, 3> BlinnMDF<T>::sample(const Vector<T, 2>& s) const
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T alpha = std::acos(std::pow(T(1.0) - s[0], T(1.0) / (m_e + T(2.0))));
    const T phi = TwoPi * s[1];

    return Vector<T, 3>::unit_vector(alpha, phi);
}

template <typename T>
inline T BlinnMDF<T>::evaluate(const T cos_alpha) const
{
    assert(cos_alpha >= T(0.0));

    return (m_e + T(2.0)) * RcpTwoPi * std::pow(cos_alpha, m_e);
}

template <typename T>
inline T BlinnMDF<T>::evaluate_pdf(const T cos_alpha) const
{
    assert(cos_alpha >= T(0.0));

    return (m_e + T(2.0)) * RcpTwoPi * std::pow(cos_alpha, m_e + T(1.0));
}


//
// BeckmannMDF class implementation.
//

template <typename T>
inline BeckmannMDF<T>::BeckmannMDF(const T m)
  : m_m(m)
{
}

template <typename T>
inline Vector<T, 3> BeckmannMDF<T>::sample(const Vector<T, 2>& s) const
{
    // Same sampling procedure as for the Ward distribution.

    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const double alpha = std::atan(m_m * std::sqrt(-std::log(T(1.0) - s[0])));
    const double phi = TwoPi * s[1];

    return Vector<T, 3>::unit_vector(alpha, phi);
}

template <typename T>
inline T BeckmannMDF<T>::evaluate(const T cos_alpha) const
{
    assert(cos_alpha >= T(0.0));

    if (cos_alpha == T(0.0))
        return T(0.0);

    const T cos_alpha_2 = cos_alpha * cos_alpha;
    const T cos_alpha_4 = cos_alpha_2 * cos_alpha_2;
    const T tan_alpha_2 = (T(1.0) - cos_alpha_2) / cos_alpha_2;
    const T m_2 = m_m * m_m;

    // Note: in [2] there's a missing Pi factor in the denominator.
    return std::exp(-tan_alpha_2 / m_2) / (m_2 * T(Pi) * cos_alpha_4);
}

template <typename T>
inline T BeckmannMDF<T>::evaluate_pdf(const T cos_alpha) const
{
    // Same PDF as for the Ward distribution.

    assert(cos_alpha >= T(0.0));

    if (cos_alpha == T(0.0))
        return T(0.0);

    const T cos_alpha_2 = cos_alpha * cos_alpha;
    const T cos_alpha_3 = cos_alpha * cos_alpha_2;
    const T tan_alpha_2 = (T(1.0) - cos_alpha_2) / cos_alpha_2;
    const T m_2 = m_m * m_m;

    return std::exp(-tan_alpha_2 / m_2) / (m_2 * T(Pi) * cos_alpha_3);
}


//
// WardMDF class implementation.
//

template <typename T>
inline WardMDF<T>::WardMDF(const T m)
  : m_m(m)
{
}

template <typename T>
inline Vector<T, 3> WardMDF<T>::sample(const Vector<T, 2>& s) const
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const double alpha = std::atan(m_m * std::sqrt(-std::log(T(1.0) - s[0])));
    const double phi = TwoPi * s[1];

    return Vector<T, 3>::unit_vector(alpha, phi);
}

template <typename T>
inline T WardMDF<T>::evaluate(const T cos_alpha) const
{
    assert(cos_alpha >= T(0.0));

    if (cos_alpha == T(0.0))
        return T(0.0);

    const T cos_alpha_2 = cos_alpha * cos_alpha;
    const T cos_alpha_3 = cos_alpha * cos_alpha_2;
    const T tan_alpha_2 = (T(1.0) - cos_alpha_2) / cos_alpha_2;
    const T m_2 = m_m * m_m;

    return std::exp(-tan_alpha_2 / m_2) / (m_2 * T(Pi) * cos_alpha_3);
}

template <typename T>
inline T WardMDF<T>::evaluate_pdf(const T cos_alpha) const
{
    return evaluate(cos_alpha);
}


//
// GGXMDF class implementation.
//

template <typename T>
inline GGXMDF<T>::GGXMDF(const T alpha_g)
  : m_alpha_g(alpha_g)
{
}

template <typename T>
inline Vector<T, 3> GGXMDF<T>::sample(const Vector<T, 2>& s) const
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T alpha = std::atan(m_alpha_g * std::sqrt(s[0] / (T(1.0) - s[0])));
    const T phi = TwoPi * s[1];

    return Vector<T, 3>::unit_vector(alpha, phi);
}

template <typename T>
inline T GGXMDF<T>::evaluate(const T cos_alpha) const
{
    assert(cos_alpha >= T(0.0));

    const T alpha_g_2 = m_alpha_g * m_alpha_g;

    if (cos_alpha == T(0.0))
        return alpha_g_2 * RcpPi;

    const T cos_alpha_2 = cos_alpha * cos_alpha;
    const T cos_alpha_4 = cos_alpha_2 * cos_alpha_2;
    const T tan_alpha_2 = (T(1.0) - cos_alpha_2) / cos_alpha_2;

    return alpha_g_2 / (Pi * cos_alpha_4 * square(alpha_g_2 + tan_alpha_2));
}

template <typename T>
inline T GGXMDF<T>::evaluate_pdf(const T cos_alpha) const
{
    assert(cos_alpha >= T(0.0));

    if (cos_alpha == T(0.0))
        return T(0.0);

    const T cos_alpha_2 = cos_alpha * cos_alpha;
    const T cos_alpha_3 = cos_alpha * cos_alpha_2;
    const T tan_alpha_2 = (T(1.0) - cos_alpha_2) / cos_alpha_2;
    const T alpha_g_2 = m_alpha_g * m_alpha_g;

    return alpha_g_2 / (Pi * cos_alpha_3 * square(alpha_g_2 + tan_alpha_2));
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_MICROFACET_H
