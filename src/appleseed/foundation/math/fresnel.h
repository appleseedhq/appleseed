
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

#ifndef APPLESEED_FOUNDATION_MATH_FRESNEL_H
#define APPLESEED_FOUNDATION_MATH_FRESNEL_H

// Standard headers.
#include <cassert>

namespace foundation
{

//
// Fresnel equations for dielectric materials.
//
// Be careful: The equations here are following the intensity form, NOT the amplitude form.
// In other words, we compute reflectances, not amplitude reflection coefficients. PBRT [2]
// uses the amplitude formalism.
//
// References:
//
//   [1] http://en.wikipedia.org/wiki/Fresnel_equations
//
//   [2] Physically Based Rendering, first edition, pp. 419-422
//

// Compute the Fresnel reflectance for a dielectric for parallel-polarized light.
template <typename SpectrumType, typename T>
void fresnel_dielectric_p_polarization(
    SpectrumType&       reflectance,
    const SpectrumType& ior_i,
    const SpectrumType& ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t);

// Compute the Fresnel reflectance for a dielectric for perpendicular-polarized light.
template <typename SpectrumType, typename T>
void fresnel_dielectric_s_polarization(
    SpectrumType&       reflectance,
    const SpectrumType& ior_i,
    const SpectrumType& ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t);

// Compute the Fresnel reflectance for a dielectric for unpolarized light.
template <typename SpectrumType, typename T>
void fresnel_dielectric_unpolarized(
    SpectrumType&       reflectance,
    const SpectrumType& ior_i,
    const SpectrumType& ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t);


//
// Compute the Fresnel reflectance for a dielectric material using Schlick's approximation:
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

template <typename SpectrumType, typename T>
void fresnel_dielectric_schlick(
    SpectrumType&       reflectance,
    const SpectrumType& ior_i,
    const SpectrumType& ior_t,
    const T             cos_theta,              // cos(incident direction, normal)
    const T             multiplier = T(1.0));   // reflectance multiplier at tangent incidence

template <typename SpectrumType, typename T>
void fresnel_dielectric_schlick(
    SpectrumType&       reflectance,
    const SpectrumType& normal_reflectance,     // reflectance at normal incidence
    const T             cos_theta,              // cos(incident direction, normal)
    const T             multiplier = T(1.0));   // reflectance multiplier at tangent incidence


//
// Implementation.
//

namespace impl
{
    template <typename T> struct GetValueType   { typedef typename T::ValueType ValueType; };
    template <> struct GetValueType<float>      { typedef float ValueType; };
    template <> struct GetValueType<double>     { typedef double ValueType; };
}

template <typename SpectrumType, typename T>
void fresnel_dielectric_p_polarization(
    SpectrumType&       reflectance,
    const SpectrumType& ior_i,
    const SpectrumType& ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t)
{
    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    assert(cos_theta_i >= T(0.0) && cos_theta_i <= T(1.0));
    assert(cos_theta_t >= T(0.0) && cos_theta_t <= T(1.0));

    SpectrumType k1(ior_t);
    k1 *= static_cast<ValueType>(cos_theta_i);

    SpectrumType k2(ior_i);
    k2 *= static_cast<ValueType>(cos_theta_t);

    SpectrumType den = k1;
    den += k2;

    reflectance = k1;
    reflectance -= k2;
    reflectance /= den;
    reflectance *= reflectance;
}

template <typename SpectrumType, typename T>
void fresnel_dielectric_s_polarization(
    SpectrumType&       reflectance,
    const SpectrumType& ior_i,
    const SpectrumType& ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t)
{
    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    assert(cos_theta_i >= T(0.0) && cos_theta_i <= T(1.0));
    assert(cos_theta_t >= T(0.0) && cos_theta_t <= T(1.0));

    SpectrumType k1(ior_i);
    k1 *= static_cast<ValueType>(cos_theta_i);

    SpectrumType k2(ior_t);
    k2 *= static_cast<ValueType>(cos_theta_t);

    SpectrumType den = k1;
    den += k2;

    reflectance = k1;
    reflectance -= k2;
    reflectance /= den;
    reflectance *= reflectance;
}

template <typename SpectrumType, typename T>
void fresnel_dielectric_unpolarized(
    SpectrumType&       reflectance,
    const SpectrumType& ior_i,
    const SpectrumType& ior_t,
    const T             cos_theta_i,
    const T             cos_theta_t)
{
    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    assert(cos_theta_i >= T(0.0) && cos_theta_i <= T(1.0));
    assert(cos_theta_t >= T(0.0) && cos_theta_t <= T(1.0));

    fresnel_dielectric_p_polarization(
        reflectance,
        ior_i,
        ior_t,
        cos_theta_i,
        cos_theta_t);

    SpectrumType rs;
    fresnel_dielectric_s_polarization(
        rs,
        ior_i,
        ior_t,
        cos_theta_i,
        cos_theta_t);

    reflectance += rs;
    reflectance *= ValueType(0.5);
}

template <typename SpectrumType, typename T>
void fresnel_dielectric_schlick(
    SpectrumType&       reflectance,
    const SpectrumType& ior_i,
    const SpectrumType& ior_t,
    const T             cos_theta,
    const T             multiplier)
{
    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    assert(cos_theta >= T(0.0) && cos_theta <= T(1.0));

    SpectrumType den = ior_i;
    den += ior_t;

    SpectrumType normal_reflectance = ior_i;
    normal_reflectance -= ior_t;
    normal_reflectance /= den;

    fresnel_dielectric_schlick(
        reflectance,
        normal_reflectance,
        cos_theta,
        multiplier);
}

template <typename SpectrumType, typename T>
void fresnel_dielectric_schlick(
    SpectrumType&       reflectance,
    const SpectrumType& normal_reflectance,
    const T             cos_theta,
    const T             multiplier)
{
    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    assert(cos_theta >= T(0.0) && cos_theta <= T(1.0));

    const T k1 = T(1.0) - cos_theta;
    const T k2 = k1 * k1;
    const T k5 = k2 * k2 * k1;

    reflectance = normal_reflectance;
    reflectance *= static_cast<ValueType>(T(1.0) - k5);
    reflectance += SpectrumType(static_cast<ValueType>(k5 * multiplier));
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_FRESNEL_H
