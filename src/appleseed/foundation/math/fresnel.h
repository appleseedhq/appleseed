
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

#pragma once

// appleseed.foundation headers.
#include "foundation/math/scalar.h"

// Standard headers.
#include <cassert>
#include <cmath>

namespace foundation
{

//
// Fresnel equations for dielectric materials.
//
// The equations here follow the intensity form, NOT the amplitude form.
// In other words, we compute reflectances, not amplitude reflection coefficients.
// PBRT [2] uses the amplitude formalism.
//
// References:
//
//   [1] http://en.wikipedia.org/wiki/Fresnel_equations
//   [2] Physically Based Rendering, first edition, pp. 419-422
//
// Conventions:
//
//   eta = eta_i / eta_t
//   cos_theta_i = cos(incident direction, normal)
//

// Compute the Fresnel reflectance for a dielectric for parallel-polarized light.
template <typename SpectrumType, typename T>
void fresnel_reflectance_dielectric_p(
    SpectrumType&           reflectance,
    const SpectrumType&     eta,
    const T                 cos_theta_i,
    const T                 cos_theta_t);

// Compute the Fresnel reflectance for a dielectric for perpendicular-polarized light.
template <typename SpectrumType, typename T>
void fresnel_reflectance_dielectric_s(
    SpectrumType&           reflectance,
    const SpectrumType&     eta,
    const T                 cos_theta_i,
    const T                 cos_theta_t);

// Compute the Fresnel reflectance for a dielectric for unpolarized light.
template <typename SpectrumType, typename T>
void fresnel_reflectance_dielectric(
    SpectrumType&           reflectance,
    const SpectrumType&     eta,
    const T                 cos_theta_i,
    const T                 cos_theta_t);

// Compute the Fresnel reflectance for a dielectric for unpolarized light.
template <typename SpectrumType, typename T>
void fresnel_reflectance_dielectric(
    SpectrumType&           reflectance,
    const SpectrumType&     eta,
    const T                 cos_theta_i);

// Compute the Fresnel transmittance for a dielectric for unpolarized light.
template <typename SpectrumType, typename T>
void fresnel_transmittance_dielectric(
    SpectrumType&           transmittance,
    const SpectrumType&     eta,
    const T                 cos_theta_i);


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
void fresnel_reflectance_dielectric_schlick(
    SpectrumType&           reflectance,
    const SpectrumType&     normal_reflectance,     // reflectance at normal incidence
    const T                 cos_theta_i,
    const T                 multiplier = T(1.0));   // reflectance multiplier at tangent incidence


//
// Compute the reflectance at normal incidence at the interface between two dieletric materials.
//
// Reference:
//
//   http://www.kayelaby.npl.co.uk/general_physics/2_5/2_5_9.html
//

template <typename SpectrumType>
void normal_reflectance_dielectric(
    SpectrumType&           normal_reflectance,
    const SpectrumType&     eta);


//
// Fresnel equations for conductor materials.
//
// References:
//
//   [1] Physically Based Rendering, first edition.
//
//   [2] Artist Friendly Metallic Fresnel.
//       Ole Gulbrandsen
//       http://jcgt.org/published/0003/04/03/paper.pdf
//
//   [3] Memo on Fresnel equations.
//       https://seblagarde.wordpress.com/2013/04/29/memo-on-fresnel-equations
//
//   [4] Fresnel Equations Considered Harmful.
//       http://renderwonk.com/publications/mam2019/naty_mam2019.pdf
//

template <typename SpectrumType, typename T>
void fresnel_reflectance_lazanyi_schlick(
    SpectrumType&           reflectance,
    const SpectrumType&     normal_reflectance,
    const T                 cos_theta_i,
    const SpectrumType&     edge_tint);

template <typename SpectrumType, typename T>
void fresnel_lazanyi_schlick_a(
    SpectrumType&           a,
    const SpectrumType&     normal_reflectance,
    const SpectrumType&     edge_tint,
    const T                 edge_tint_weight);

// Compute the Fresnel reflectance for a conductor for unpolarized light.
template <typename SpectrumType, typename T>
void fresnel_reflectance_conductor(
    SpectrumType&           reflectance,
    const SpectrumType&     nt,                  // conductor refractive index
    const SpectrumType&     kt,                  // conductor extinction coefficient
    const T                 ni,                  // incident refractive index
    const T                 cos_theta_i);

// Compute the Fresnel reflectance for a conductor for unpolarized light.
// Assumes the incident refractive index is 1.0 (air).
template <typename SpectrumType, typename T>
void fresnel_reflectance_conductor(
    SpectrumType&           reflectance,
    const SpectrumType&     nt,                  // conductor refractive index
    const SpectrumType&     kt,                  // conductor extinction coefficient
    const T                 cos_theta_i);

// Compute the refractive index and extinction coefficients for a conductor
// using the reparameterization defined in [2].
template <typename SpectrumType>
inline void artist_friendly_fresnel_conductor_reparameterization(
    const SpectrumType&     normal_reflectance,
    const SpectrumType&     edge_tint,
    SpectrumType&           n,                  // refractive index
    SpectrumType&           k);                 // extinction coefficient

// Compute the normal reflectivity and edge tint for a conductor
// using the inverse of the reparameterization defined in [2].
template <typename SpectrumType>
inline void artist_friendly_fresnel_conductor_inverse_reparameterization(
    const SpectrumType&     n,                  // refractive index
    const SpectrumType&     k,                  // extinction coefficient
    SpectrumType&           normal_reflectance,
    SpectrumType&           edge_tint);

// Compute the Fresnel reflectance for a conductor for unpolarized light.
// This approximation assumes an air - conductor interface.
template <typename SpectrumType, typename T>
void artist_friendly_fresnel_reflectance_conductor(
    SpectrumType&           reflectance,
    const SpectrumType&     normal_reflectance,
    const SpectrumType&     edge_tint,
    const T                 cos_theta_i);


//
// Fresnel integrals.
//
// References:
//
//   A Better Dipole
//   http://www.eugenedeon.com/wp-content/uploads/2014/04/betterdipole.pdf
//
//   Towards Realistic Image Synthesis of Scattering Materials (eq. 5.27 page 41)
//   http://www.cs.jhu.edu/~misha/Fall11/Donner.Thesis.pdf
//
//   Light Transport in Tissue (appendix A2, page 167)
//   http://omlc.org/~prahl/pubs/pdf/prahl88.pdf
//
//   Fresnel Reflection of Diffusely Incident Light
//   http://nvlpubs.nist.gov/nistpubs/jres/29/jresv29n5p329_A1b.pdf
//
//   Diffuse Fresnel Reflectance
//   http://photorealizer.blogspot.fr/2012/05/diffuse-fresnel-reflectance.html
//
//   Revisiting Physically Based Shading at Imageworks
//   http://blog.selfshadow.com/publications/s2017-shading-course/imageworks/s2017_pbs_imageworks_slides.pdf
//

// Compute 2 * C1(eta) where C1(eta) is the first moment of the Fresnel equation.
template <typename T>
T fresnel_first_moment_x2(const T eta);

// Compute 3 * C2(eta) where C2(eta) is the second moment of the Fresnel equation.
template <typename T>
T fresnel_second_moment_x3(const T eta);

// Compute the internal diffuse reflectance of a dielectric.
template <typename T>
T fresnel_internal_diffuse_reflectance(const T eta);

// Compute the average Fresnel reflectance for a dielectric for unpolarized light.
template <typename T>
T average_fresnel_reflectance_dielectric(const T eta);

// Compute the average Fresnel reflectance for a conductor for unpolarized light.
// This approximation assumes an air - conductor interface.
template <typename SpectrumType>
void average_artist_friendly_fresnel_reflectance_conductor(
    const SpectrumType&     normal_reflectance,
    const SpectrumType&     edge_tint,
    SpectrumType&           reflectance);


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
void fresnel_reflectance_dielectric_p(
    SpectrumType&           reflectance,
    const SpectrumType&     eta,
    const T                 cos_theta_i,
    const T                 cos_theta_t)
{
    //
    //                 /  ior_i * cos_theta_t - ior_t * cos_theta_i  \ 2
    // reflectance  =  |  -----------------------------------------  |
    //                 \  ior_i * cos_theta_t + ior_t * cos_theta_i  /
    //
    //                 /  eta * cos_theta_t - cos_theta_i  \ 2
    //              =  |  -------------------------------  |
    //                 \  eta * cos_theta_t + cos_theta_i  /
    //
    //                 /  cos_theta_i - eta * cos_theta_t  \ 2
    //              =  |  -------------------------------  |
    //                 \  cos_theta_i + eta * cos_theta_t  /
    //

    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    assert(cos_theta_i >= T(0.0) && cos_theta_i <= T(1.0));
    assert(cos_theta_t >= T(0.0) && cos_theta_t <= T(1.0));
    assert(cos_theta_i >  T(0.0) || cos_theta_t >  T(0.0));

    SpectrumType k(eta);
    k *= static_cast<ValueType>(cos_theta_t);

    SpectrumType den(static_cast<ValueType>(cos_theta_i));
    reflectance = den;
    reflectance -= k;
    den += k;
    reflectance /= den;
    reflectance *= reflectance;
}

template <typename SpectrumType, typename T>
void fresnel_reflectance_dielectric_s(
    SpectrumType&           reflectance,
    const SpectrumType&     eta,
    const T                 cos_theta_i,
    const T                 cos_theta_t)
{
    //
    //                 /  ior_i * cos_theta_i - ior_t * cos_theta_t  \ 2
    // reflectance  =  |  -----------------------------------------  |
    //                 \  ior_i * cos_theta_i + ior_t * cos_theta_t  /
    //
    //                 /  eta * cos_theta_i - cos_theta_t  \ 2
    //              =  |  -------------------------------  |
    //                 \  eta * cos_theta_i + cos_theta_t  /
    //
    //                 /  cos_theta_t - eta * cos_theta_i  \ 2
    //              =  |  -------------------------------  |
    //                 \  cos_theta_t + eta * cos_theta_i  /
    //

    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    assert(cos_theta_i >= T(0.0) && cos_theta_i <= T(1.0));
    assert(cos_theta_t >= T(0.0) && cos_theta_t <= T(1.0));
    assert(cos_theta_i >  T(0.0) || cos_theta_t >  T(0.0));

    SpectrumType k(eta);
    k *= static_cast<ValueType>(cos_theta_i);

    SpectrumType den(static_cast<ValueType>(cos_theta_t));
    reflectance = den;
    reflectance -= k;
    den += k;
    reflectance /= den;
    reflectance *= reflectance;
}

template <typename SpectrumType, typename T>
void fresnel_reflectance_dielectric(
    SpectrumType&           reflectance,
    const SpectrumType&     eta,
    const T                 cos_theta_i,
    const T                 cos_theta_t)
{
    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    assert(cos_theta_i >= T(0.0) && cos_theta_i <= T(1.0));
    assert(cos_theta_t >= T(0.0) && cos_theta_t <= T(1.0));

    if (cos_theta_i == T(0.0) && cos_theta_t == T(0.0))
    {
        reflectance = SpectrumType(1.0);
        return;
    }

    fresnel_reflectance_dielectric_p(
        reflectance,
        eta,
        cos_theta_i,
        cos_theta_t);

    SpectrumType rs;
    fresnel_reflectance_dielectric_s(
        rs,
        eta,
        cos_theta_i,
        cos_theta_t);

    reflectance += rs;
    reflectance *= ValueType(0.5);
}

template <typename SpectrumType, typename T>
void fresnel_reflectance_dielectric(
    SpectrumType&           reflectance,
    const SpectrumType&     eta,
    const T                 cos_theta_i)
{
    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    const T sin_theta_i2 = T(1.0) - square(cos_theta_i);
    const T sin_theta_t2 = sin_theta_i2 * square(eta);
    const T cos_theta_t2 = T(1.0) - sin_theta_t2;

    if (cos_theta_t2 < T(0.0))
    {
        // Total internal reflection.
        reflectance = ValueType(1.0);
    }
    else
    {
        const T cos_theta_t = std::sqrt(cos_theta_t2);
        fresnel_reflectance_dielectric(reflectance, eta, cos_theta_i, cos_theta_t);
    }
}

template <typename SpectrumType, typename T>
void fresnel_transmittance_dielectric(
    SpectrumType&           transmittance,
    const SpectrumType&     eta,
    const T                 cos_theta_i)
{
    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    const T sin_theta_i2 = T(1.0) - square(cos_theta_i);
    const T sin_theta_t2 = sin_theta_i2 * square(eta);
    const T cos_theta_t2 = T(1.0) - sin_theta_t2;

    if (cos_theta_t2 < T(0.0))
    {
        // Total internal reflection.
        transmittance = ValueType(0.0);
    }
    else
    {
        const T cos_theta_t = std::sqrt(cos_theta_t2);

        T reflectance;
        fresnel_reflectance_dielectric(reflectance, eta, cos_theta_i, cos_theta_t);

        transmittance = SpectrumType(ValueType(1.0));
        transmittance -= reflectance;
    }
}

template <typename SpectrumType, typename T>
void fresnel_reflectance_dielectric_schlick(
    SpectrumType&           reflectance,
    const SpectrumType&     normal_reflectance,
    const T                 cos_theta_i,
    const T                 multiplier)
{
    //
    // reflectance = normal_reflectance + (1 - normal_reflectance) * [1 - cos(theta)]^5
    //

    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    assert(cos_theta_i >= T(0.0) && cos_theta_i <= T(1.0));

    const T k1 = T(1.0) - cos_theta_i;
    const T k2 = k1 * k1;
    const T k5 = k2 * k2 * k1;

    reflectance = normal_reflectance;
    reflectance *= static_cast<ValueType>(T(1.0) - k5);
    reflectance += SpectrumType(static_cast<ValueType>(k5 * multiplier));
}

template <typename SpectrumType>
void normal_reflectance_dielectric(
    SpectrumType&           normal_reflectance,
    const SpectrumType&     eta)
{
    //
    //                        /  ior_i - ior_t  \ 2     /  eta - 1  \ 2     /  1 - eta  \ 2
    // normal_reflectance  =  |  -------------  |    =  |  -------  |    =  |  -------  |
    //                        \  ior_i + ior_t  /       \  eta + 1  /       \  1 + eta  /
    //

    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    SpectrumType den(ValueType(1.0));
    normal_reflectance = den;
    normal_reflectance -= eta;
    den += eta;
    normal_reflectance /= den;
    normal_reflectance *= normal_reflectance;
}

template <typename SpectrumType, typename T>
void fresnel_reflectance_lazanyi_schlick(
    SpectrumType&           reflectance,
    const SpectrumType&     normal_reflectance,
    const T                 cos_theta_i,
    const SpectrumType&     a)
{
    //
    // F = r + (1 - r) * [1 - cos(theta)]^5 - a * cos(theta) * [1 - cos(theta)]^6
    //

    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    assert(cos_theta_i >= T(0.0) && cos_theta_i <= T(1.0));

    const T k1 = T(1.0) - cos_theta_i;
    const T k2 = k1 * k1;
    const T k5 = k2 * k2 * k1;
    const T k6 = k5 * k1;

    reflectance = SpectrumType(T(1.0));
    reflectance -= normal_reflectance;
    reflectance *= static_cast<ValueType>(k5);
    reflectance += normal_reflectance;

    SpectrumType h(a);
    h *= cos_theta_i * k6;
    reflectance -= h;

    clamp(reflectance, T(0.0), T(1.0));
}

template <typename SpectrumType, typename T>
void fresnel_lazanyi_schlick_a(
    SpectrumType&           a,
    const SpectrumType&     normal_reflectance,
    const SpectrumType&     edge_tint,
    const T                 edge_tint_weight)
{
    //
    //      r + (1 - r) * [1 - cos(theta_max)]^5 - h
    // a = ------------------------------------------
    //      cos(theta_max) * [1 - cos(theta_max)]^6
    //

    const T cos_theta_max = std::cos(T(1.4259339988793673));
    const T k1 = T(1.0) - cos_theta_max;
    const T k2 = k1 * k1;
    const T k4 = k2  * k2 ;
    const T k5 = k4 * k1;
    const T k6 = k5 * k1;

    a = SpectrumType(T(1.0));
    a -= normal_reflectance;
    a *= k5;
    a += normal_reflectance;
    a -= edge_tint;
    a *= edge_tint_weight / (cos_theta_max * k6 );
}

template <typename SpectrumType, typename T>
inline void fresnel_reflectance_conductor(
    SpectrumType&           reflectance,
    const SpectrumType&     nt,
    const SpectrumType&     kt,
    const T                 ni,
    const T                 cos_theta_i)
{
    const T cos_theta = std::abs(cos_theta_i);
    const T cos_theta2 = square(cos_theta);
    const T sin_theta2 = T(1.0) - cos_theta2;
    const T sin_theta4 = square(sin_theta2);

    const T rcp_ni2 = T(1.0) / square(ni);

    SpectrumType nt2 = nt * nt;
    nt2 *= rcp_ni2;

    SpectrumType kt2 = kt * kt;
    kt2 *= rcp_ni2;

    SpectrumType t0 = nt2 - kt2;
    t0 -= SpectrumType(sin_theta2);

    SpectrumType a2plusb2 = t0 * t0;
    SpectrumType tmp = nt2 * kt2;
    tmp *= T(4.0);
    a2plusb2 += tmp;
    a2plusb2 = sqrt(a2plusb2);

    const SpectrumType t1 = a2plusb2 + SpectrumType(cos_theta2);

    tmp = a2plusb2 + t0;
    tmp *= T(0.5);
    const SpectrumType a = sqrt(tmp);

    SpectrumType t2 = a;
    t2 *= T(2.0) * cos_theta;

    tmp = t1 + t2;
    reflectance = t1 - t2;
    reflectance /= tmp;

    SpectrumType t3 = a2plusb2;
    t3 *= cos_theta2;
    t3 += SpectrumType(sin_theta4);

    SpectrumType t4 = t2;
    t4 *= sin_theta2;

    tmp = t3 + t4;
    SpectrumType Rp = t3 - t4;
    Rp /= tmp;
    Rp *= reflectance;

    reflectance += Rp;
    reflectance *= T(0.5);
}

template <typename SpectrumType, typename T>
inline void fresnel_reflectance_conductor(
    SpectrumType&           reflectance,
    const SpectrumType&     nt,
    const SpectrumType&     kt,
    const T                 cos_theta_i)
{
    const T cos_theta = std::abs(cos_theta_i);
    const T cos_theta2 = square(cos_theta);
    const T sin_theta2 = T(1.0) - cos_theta2;
    const T sin_theta4 = square(sin_theta2);

    const SpectrumType nt2 = nt * nt;
    const SpectrumType kt2 = kt * kt;

    SpectrumType t0 = nt2 - kt2;
    t0 -= SpectrumType(sin_theta2);

    SpectrumType a2plusb2 = t0 * t0;
    SpectrumType tmp = nt2 * kt2;
    tmp *= T(4.0);
    a2plusb2 += tmp;
    a2plusb2 = sqrt(a2plusb2);

    const SpectrumType t1 = a2plusb2 + SpectrumType(cos_theta2);

    tmp = a2plusb2 + t0;
    tmp *= T(0.5);
    const SpectrumType a = sqrt(tmp);

    SpectrumType t2 = a;
    t2 *= T(2.0) * cos_theta;

    tmp = t1 + t2;
    reflectance = t1 - t2;
    reflectance /= tmp;

    SpectrumType t3 = a2plusb2;
    t3 *= cos_theta2;
    t3 += SpectrumType(sin_theta4);

    SpectrumType t4 = t2;
    t4 *= sin_theta2;

    tmp = t3 + t4;
    SpectrumType Rp = t3 - t4;
    Rp /= tmp;
    Rp *= reflectance;

    reflectance += Rp;
    reflectance *= T(0.5);
}

template <typename SpectrumType>
inline void artist_friendly_fresnel_conductor_reparameterization(
    const SpectrumType&     normal_reflectance,
    const SpectrumType&     edge_tint,
    SpectrumType&           n,
    SpectrumType&           k)
{
    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    const SpectrumType one(1.0);
    const SpectrumType r = clamp(normal_reflectance, ValueType(0.0), ValueType(0.99));
    const SpectrumType sqrt_r = sqrt(r);

    const SpectrumType one_minus_r = one - r;

    const SpectrumType nmin = one_minus_r / (one + r);
    const SpectrumType nmax = (one + sqrt_r) / (one - sqrt_r);

    const SpectrumType g = clamp(edge_tint, ValueType(0.0), ValueType(1.0));
    n = lerp(nmax, nmin, g);

    SpectrumType square_n_plus_one(n + one);
    square_n_plus_one *= square_n_plus_one;

    SpectrumType square_n_minus_one(n - one);
    square_n_minus_one *= square_n_minus_one;

    k = (square_n_plus_one * r - square_n_minus_one);
    k /= one_minus_r;
    k = sqrt(k);
}

template <typename SpectrumType>
inline void artist_friendly_fresnel_conductor_inverse_reparameterization(
    const SpectrumType&     n,
    const SpectrumType&     k,
    SpectrumType&           normal_reflectance,
    SpectrumType&           edge_tint)
{
    const SpectrumType one(1.0);

    SpectrumType square_n_plus_one(n + one);
    square_n_plus_one *= square_n_plus_one;

    SpectrumType square_n_minus_one(n - one);
    square_n_minus_one *= square_n_minus_one;

    const SpectrumType k2 = k * k;

    normal_reflectance =
        (square_n_minus_one + k2) /
        (square_n_plus_one + k2);

    const SpectrumType one_plus_r(one + normal_reflectance);
    const SpectrumType one_minus_r(one - normal_reflectance);

    const SpectrumType sqrt_r = sqrt(normal_reflectance);
    const SpectrumType one_plus_sqrt_r(one + sqrt_r);
    const SpectrumType one_minus_sqrt_r(one - sqrt_r);

    const SpectrumType tmp(one_plus_sqrt_r / one_minus_sqrt_r);
    edge_tint = (tmp - n) / (tmp - (one_minus_r / one_plus_r));
}

template <typename SpectrumType, typename T>
inline void artist_friendly_fresnel_reflectance_conductor(
    SpectrumType&           reflectance,
    const SpectrumType&     normal_reflectance,
    const SpectrumType&     edge_tint,
    const T                 cos_theta_i)
{
    SpectrumType n, k;
    artist_friendly_fresnel_conductor_reparameterization(
        normal_reflectance,
        edge_tint,
        n,
        k);

    fresnel_reflectance_conductor(reflectance, n, k, cos_theta_i);
}

template <typename T>
inline T fresnel_first_moment_x2(const T eta)
{
    return
        eta < T(1.0)
            ? T(0.919317) + eta * (T(-3.4793) + eta * (T(6.75335) + eta * (T(-7.80989) + eta * (T(4.98554) - eta * T(1.36881)))))
            : T(-9.23372) + eta * (T(22.2272) + eta * (T(-20.9292) + eta * (T(10.2291) + eta * (T(-2.54396) + eta * T(0.254913)))));
}

template <typename T>
inline T fresnel_second_moment_x3(const T eta)
{
    const T rcp_eta = T(1.0) / eta;
    return
        eta < T(1.0)
            ? T(0.828421) + eta * (T(-2.62051) + eta * (T(3.36231) + eta * (T(-1.95284) + eta * (T(0.236494) + eta * T(0.145787)))))
            : T(-1641.1) + (((T(135.926) * rcp_eta) - T(656.175)) * rcp_eta + T(1376.53)) * rcp_eta
              + eta * (T(1213.67) + eta * (T(-568.556) + eta * (T(164.798) + eta * (T(-27.0181) + eta * T(1.91826)))));
}

template <typename T>
inline T fresnel_internal_diffuse_reflectance(const T eta)
{
    const T rcp_eta = T(1.0) / eta;
    const T rcp_eta2 = rcp_eta * rcp_eta;
    return
        eta < T(1.0)
            ? T(-0.4399) + T(0.7099) * rcp_eta - T(0.3319) * rcp_eta2 + T(0.0636) * rcp_eta * rcp_eta2
            : T(-1.4399) * rcp_eta2 + T(0.7099) * rcp_eta + T(0.6681) + T(0.0636) * eta;
}

template <typename T>
T average_fresnel_reflectance_dielectric(const T eta)
{
    if (eta >= T(1.0))
        return (eta - T(1.0)) / (T(4.08567) + T(1.00071) * eta);

    const T eta2 = eta * eta;
    const T eta3 = eta * eta2;
    return T(0.997118) + T(0.1014) * eta - T(0.965241) * eta2 - T(0.130607) * eta3;
}

template <typename SpectrumType>
inline void average_artist_friendly_fresnel_reflectance_conductor(
    const SpectrumType&     normal_reflectance,
    const SpectrumType&     edge_tint,
    SpectrumType&           reflectance)
{
    typedef typename impl::GetValueType<SpectrumType>::ValueType ValueType;

    SpectrumType edge_tint2 = edge_tint * edge_tint;
    SpectrumType edge_tint3 = edge_tint * edge_tint2;

    reflectance  = SpectrumType(ValueType(0.087237));
    reflectance += edge_tint  * ValueType(0.0230685);
    reflectance -= edge_tint2 * ValueType(0.0864902);
    reflectance += edge_tint3 * ValueType(0.0774594);

    SpectrumType normal_reflectance2 = normal_reflectance * normal_reflectance;
    SpectrumType normal_reflectance3 = normal_reflectance * normal_reflectance2;

    reflectance += normal_reflectance  * ValueType(0.782654);
    reflectance -= normal_reflectance2 * ValueType(0.136432);
    reflectance += normal_reflectance3 * ValueType(0.278708);

    reflectance += normal_reflectance  * edge_tint  * ValueType(0.1974400);
    reflectance += normal_reflectance  * edge_tint2 * ValueType(0.0360605);
    reflectance -= normal_reflectance2 * edge_tint  * ValueType(0.2586000);
}

}   // namespace foundation
