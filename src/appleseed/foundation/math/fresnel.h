
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
// Fresnel integrals.
//
// References:
//
//   Light Transport in Tissue (appendix A2, page 167)
//   http://omlc.org/~prahl/pubs/pdf/prahl88.pdf
//
//   Fresnel Reflection of Diffusely Incident Light 
//   http://nvlpubs.nist.gov/nistpubs/jres/29/jresv29n5p329_A1b.pdf
//
//   Towards Realistic Image Synthesis of Scattering Materials (eq. 5.27 page 41)
//   http://www.cs.jhu.edu/~misha/Fall11/Donner.Thesis.pdf
//
//   A Better Dipole
//   http://www.eugenedeon.com/wp-content/uploads/2014/04/betterdipole.pdf
//
//   Diffuse Fresnel Reflectance
//   http://photorealizer.blogspot.fr/2012/05/diffuse-fresnel-reflectance.html
//

// Compute the first moment of the Fresnel equation.
template <typename T>
T fresnel_first_moment(const T eta);

// Compute the second moment of the Fresnel equation.
template <typename T>
T fresnel_second_moment(const T eta);

// Compute the internal diffuse reflectance of a dielectric.
template <typename T>
T fresnel_internal_diffuse_reflectance(const T eta);


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

template <typename T>
inline T fresnel_first_moment(const T eta)
{
    return
        eta < T(1.0)
            ? T(0.919317) + eta * (T(-3.4793) + eta * (T(6.75335) + eta * (T(-7.80989) + eta * (T(4.98554) - eta * T(1.36881)))))
            : T(-9.23372) + eta * (T(22.2272) + eta * (T(-20.9292) + eta * (T(10.2291) + eta * (T(-2.54396) + eta * T(0.254913)))));
}

template <typename T>
inline T fresnel_second_moment(const T eta)
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

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_FRESNEL_H
