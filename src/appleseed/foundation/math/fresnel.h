
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_MATH_FRESNEL_H
#define APPLESEED_FOUNDATION_MATH_FRESNEL_H

// Standard headers.
#include <cassert>

namespace foundation
{

//
// Implementation of the Fresnel equations.
//
// Reference:
//
//   http://en.wikipedia.org/wiki/Fresnel_equations
//

template <typename Spectrum, typename T>
Spectrum fresnel_reflection_s_polarization(
    const Spectrum&     ior_i,
    const Spectrum&     ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t);

template <typename Spectrum, typename T>
Spectrum fresnel_reflection_p_polarization(
    const Spectrum&     ior_i,
    const Spectrum&     ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t);

template <typename Spectrum, typename T>
Spectrum fresnel_reflection_no_polarization(
    const Spectrum&     ior_i,
    const Spectrum&     ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t);


//
// Compute the Fresnel reflection factor using Schlick's approximation:
//
//   R(theta) = R0 + (1 - R0) * (1 - cos(theta)) ^ 5
//
// where theta is the incident angle and R0 is the reflectance at normal incidence.
//
// References:
//
//   http://en.wikipedia.org/wiki/Schlick's_approximation
//   http://citeseer.ist.psu.edu/schlick94inexpensive.html
//

template <typename Spectrum, typename T>
Spectrum schlick_fresnel_reflection(
    const Spectrum&     normal_reflectance,     // reflectance at normal incidence
    const T             cos_theta);             // cos(incident direction, normal)


//
// Implementation.
//

template <typename Spectrum, typename T>
Spectrum fresnel_reflection_s_polarization(
    const Spectrum&     ior_i,
    const Spectrum&     ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t)
{
    assert(cos_theta_i >= T(0.0));
    assert(cos_theta_t >= T(0.0));

    typedef typename Spectrum::ValueType ValueType;

    Spectrum k1 = ior_i;
    k1 *= static_cast<ValueType>(cos_theta_i);

    Spectrum k2 = ior_t;
    k2 *= static_cast<ValueType>(cos_theta_t);

    Spectrum rs_den = k1;
    rs_den += k2;

    Spectrum rs = k1;
    rs -= k2;
    rs /= rs_den;

    rs *= rs;

    return rs;
}

template <typename Spectrum, typename T>
Spectrum fresnel_reflection_p_polarization(
    const Spectrum&     ior_i,
    const Spectrum&     ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t)
{
    assert(cos_theta_i >= T(0.0));
    assert(cos_theta_t >= T(0.0));

    typedef typename Spectrum::ValueType ValueType;

    Spectrum k1 = ior_i;
    k1 *= static_cast<ValueType>(cos_theta_t);

    Spectrum k2 = ior_t;
    k2 *= static_cast<ValueType>(cos_theta_i);

    Spectrum rs_den = k1;
    rs_den += k2;

    Spectrum rs = k1;
    rs -= k2;
    rs /= rs_den;

    rs *= rs;

    return rs;
}

template <typename Spectrum, typename T>
Spectrum fresnel_reflection_no_polarization(
    const Spectrum&     ior_i,
    const Spectrum&     ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t)
{
    assert(cos_theta_i >= T(0.0));
    assert(cos_theta_t >= T(0.0));

    typedef typename Spectrum::ValueType ValueType;

    Spectrum r =
        fresnel_reflection_s_polarization(
            ior_i,
            ior_t,
            cos_theta_i,
            cos_theta_t);

    r +=
        fresnel_reflection_p_polarization(
            ior_i,
            ior_t,
            cos_theta_i,
            cos_theta_t);

    r *= ValueType(0.5);

    return r;
}

template <typename Spectrum, typename T>
Spectrum schlick_fresnel_reflection(
    const Spectrum&     normal_reflectance,
    const T             cos_theta)
{
    typedef typename Spectrum::ValueType ValueType;

    const T k = T(1.0) - cos_theta;
    T k5 = k * k;
    k5 *= k5;
    k5 *= k;

    Spectrum r = normal_reflectance;
    r *= static_cast<ValueType>(T(1.0) - k5);
    r += Spectrum(static_cast<ValueType>(k5));

    return r;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_FRESNEL_H
