
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

#ifndef APPLESEED_FOUNDATION_MATH_FRESNEL_H
#define APPLESEED_FOUNDATION_MATH_FRESNEL_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <cassert>
#include <cmath>

namespace foundation
{

//
// Implementation of the Fresnel equations.
//
// Reference:
//
//   http://en.wikipedia.org/wiki/Fresnel_equations
//

// Compute the Fresnel reflectance for a dielectric for parallel polarized light (intensity form).
template <typename Spectrum, typename T>
Spectrum fresnel_dielectric_p_polarization(
    const Spectrum&     ior_i,
    const Spectrum&     ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t);

// Compute the Fresnel reflectance for a dielectric for perpendicular polarized light (intensity form).
template <typename Spectrum, typename T>
Spectrum fresnel_dielectric_s_polarization(
    const Spectrum&     ior_i,
    const Spectrum&     ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t);

// Compute the Fresnel reflectance for a dielectric for unpolarized light.
template <typename Spectrum, typename T>
Spectrum fresnel_dielectric_unpolarized(
    const Spectrum&     ior_i,
    const Spectrum&     ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t);


//
// Compute the Fresnel reflection factor for a dielectric using Schlick's approximation:
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
Spectrum fresnel_dielectric_schlick(
    const Spectrum&     normal_reflectance,     // reflectance at normal incidence
    const T             cos_theta,              // cos(incident direction, normal)
    const T             multiplier = T(1.0));   // reflectance multiplier at tangent incidence

//
// Compute the Fresnel reflection factor for a dielectric.
// Adapted from OSL's test render sample.
// TODO: this probably needs a better name
//

template <typename T>
T fresnel_dielectric(const T cosi, T eta);

//
// Implementation.
//

namespace impl
{
    template <typename T> struct GetValueType   { typedef typename T::ValueType ValueType; };
    template <> struct GetValueType<float>      { typedef float ValueType; };
    template <> struct GetValueType<double>     { typedef double ValueType; };
}

template <typename Spectrum, typename T>
Spectrum fresnel_dielectric_p_polarization(
    const Spectrum&     ior_i,
    const Spectrum&     ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t)
{
    typedef typename impl::GetValueType<Spectrum>::ValueType ValueType;

    assert(cos_theta_i >= T(0.0) && cos_theta_i <= T(1.0));
    assert(cos_theta_t >= T(0.0) && cos_theta_t <= T(1.0));

    Spectrum k1 = ior_i;
    k1 *= static_cast<ValueType>(cos_theta_t);

    Spectrum k2 = ior_t;
    k2 *= static_cast<ValueType>(cos_theta_i);

    Spectrum den = k1;
    den += k2;

    Spectrum fr = k1;
    fr -= k2;
    fr /= den;
    fr *= fr;

    return fr;
}

template <typename Spectrum, typename T>
Spectrum fresnel_dielectric_s_polarization(
    const Spectrum&     ior_i,
    const Spectrum&     ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t)
{
    typedef typename impl::GetValueType<Spectrum>::ValueType ValueType;

    assert(cos_theta_i >= T(0.0) && cos_theta_i <= T(1.0));
    assert(cos_theta_t >= T(0.0) && cos_theta_t <= T(1.0));

    Spectrum k1 = ior_i;
    k1 *= static_cast<ValueType>(cos_theta_i);

    Spectrum k2 = ior_t;
    k2 *= static_cast<ValueType>(cos_theta_t);

    Spectrum den = k1;
    den += k2;

    Spectrum fr = k1;
    fr -= k2;
    fr /= den;
    fr *= fr;

    return fr;
}

template <typename Spectrum, typename T>
Spectrum fresnel_dielectric_unpolarized(
    const Spectrum&     ior_i,
    const Spectrum&     ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t)
{
    typedef typename impl::GetValueType<Spectrum>::ValueType ValueType;

    assert(cos_theta_i >= T(0.0) && cos_theta_i <= T(1.0));
    assert(cos_theta_t >= T(0.0) && cos_theta_t <= T(1.0));

    Spectrum fr =
        fresnel_dielectric_p_polarization(
            ior_i,
            ior_t,
            cos_theta_i,
            cos_theta_t);

    fr +=
        fresnel_dielectric_s_polarization(
            ior_i,
            ior_t,
            cos_theta_i,
            cos_theta_t);

    fr *= ValueType(0.5);

    return fr;
}

template <typename Spectrum, typename T>
Spectrum fresnel_dielectric_schlick(
    const Spectrum&     normal_reflectance,
    const T             cos_theta,
    const T             multiplier)
{
    typedef typename impl::GetValueType<Spectrum>::ValueType ValueType;

    assert(cos_theta >= T(0.0) && cos_theta <= T(1.0));

    const T k1 = T(1.0) - cos_theta;
    const T k2 = k1 * k1;
    const T k5 = k2 * k2 * k1;

    Spectrum fr = normal_reflectance;
    fr *= static_cast<ValueType>(T(1.0) - k5);
    fr += Spectrum(static_cast<ValueType>(k5 * multiplier));

    return fr;
}

template <typename T>
T fresnel_dielectric(const T cosi, T eta)
{
    // compute fresnel reflectance without explicitly computing the refracted direction
    if (cosi < T(0.0))
        eta = T(1.0) / eta;

    const T c = std::abs(cosi);
    T g = eta * eta - T(1.0) + c * c;

    if (g > T(0.0))
    {
        g = std::sqrt(g);
        const T A = (g - c) / (g + c);
        const T B = (c * (g + c) - T(1.0)) / (c * (g - c) + T(1.0));
        const T F = T(0.5) * square(A) * (1 + square(B));
        assert(F >= T(0.0));
        return F;
    }

    return T(1.0); // TIR (no refracted component)    
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_FRESNEL_H
