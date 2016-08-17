
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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

//
// MDF: Base class for microfacet distribution functions.
//

template <typename T>
class MDF
  : public NonCopyable
{
  public:
    typedef T ValueType;

    MDF() {}

    virtual ~MDF() {}

    virtual T D(
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const = 0;

    virtual T G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const = 0;

    virtual T G1(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  m,
        const T              alpha_x,
        const T              alpha_y) const = 0;

    virtual Vector<T, 3> sample(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  s,
        const T              alpha_x,
        const T              alpha_y) const = 0;

    virtual T pdf(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const = 0;

  protected:
    static T cos_theta(const Vector<T, 3>& v)
    {
        return v.y;
    }

    static T sin_theta(const Vector<T, 3>& v)
    {
        return std::sqrt(std::max(T(0.0), T(1.0) - square(cos_theta(v))));
    }

    static void sample_phi(
        const T s,
        T&      cos_phi,
        T&      sin_phi)
    {
        const T phi = T(TwoPi) * s;
        cos_phi = std::cos(phi);
        sin_phi = std::sin(phi);
    }

    static void sample_phi(
        const T s,
        const T alpha_x,
        const T alpha_y,
        T&      cos_phi,
        T&      sin_phi)
    {
        Vector<T, 2> sin_cos_phi(
            std::cos(T(TwoPi) * s) * alpha_x,
            std::sin(T(TwoPi) * s) * alpha_y);
        sin_cos_phi = normalize(sin_cos_phi);
        cos_phi = sin_cos_phi[0];
        sin_phi = sin_cos_phi[1];
    }

    static T stretched_roughness(
        const Vector<T, 3>& h,
        const T             sin_theta,
        const T             alpha_x,
        const T             alpha_y)
    {
        if (alpha_x == alpha_y || sin_theta == T(0.0))
            return T(1.0) / square(alpha_x);

        const T cos_phi_2_ax_2 = square(h.x / (sin_theta * alpha_x));
        const T sin_phi_2_ay_2 = square(h.z / (sin_theta * alpha_y));
        return cos_phi_2_ax_2 + sin_phi_2_ay_2;
    }

    static T projected_roughness(
        const Vector<T, 3>& h,
        const T             sin_theta,
        const T             alpha_x,
        const T             alpha_y)
    {
        if (alpha_x == alpha_y || sin_theta == T(0.0))
            return alpha_x;

        const T cos_phi_2_ax_2 = square((h.x * alpha_x) / sin_theta);
        const T sin_phi_2_ay_2 = square((h.z * alpha_y) / sin_theta);
        return std::sqrt(cos_phi_2_ax_2 + sin_phi_2_ay_2);
    }

    static Vector<T, 3> v_cavity_choose_microfacet_normal(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              s)
    {
        // Preconditions.
        assert(is_normalized(v));
        assert(v.y >= T(0.0));

        const Vector<T, 3> hm(-h[0], h[1], -h[2]);
        const T dot_vh  = std::max(dot(v, h), T(0.0));
        const T dot_vhm = std::max(dot(v, hm), T(0.0));

        if (dot_vhm == T(0.0))
            return h;

        const T w = dot_vhm / (dot_vh + dot_vhm);
        return s < w ? hm : h;
    }

    template <typename Distribution>
    static Vector<T, 3> sample_visible_normals(
        const Distribution&  mdf,
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  s,
        const T              alpha_x,
        const T              alpha_y)
    {
        // Preconditions.
        assert(is_normalized(v));

        // Stretch incident.
        const T sign_cos_vn = v.y < T(0.0) ? T(-1.0) : T(1.0);
        Vector<T, 3> stretched(
            sign_cos_vn * v[0] * alpha_x,
            sign_cos_vn * v[1],
            sign_cos_vn * v[2] * alpha_y);

        stretched = normalize(stretched);

        const T cos_theta = stretched[1];
        const T phi =
            stretched[1] < T(0.99999) ? std::atan2(stretched[2], stretched[0]) : T(0.0);

        // Sample slope.
        Vector<T, 2> slope = mdf.sample11(cos_theta, s);

        // Rotate.
        const T cos_phi = std::cos(phi);
        const T sin_phi = std::sin(phi);

        slope = Vector<T, 2>(
            cos_phi * slope[0] - sin_phi * slope[1],
            sin_phi * slope[0] + cos_phi * slope[1]);

        // Stretch and normalize.
        const Vector<T, 3> m(
            -slope[0] * alpha_x,
            T(1.0),
            -slope[1] * alpha_y);
        return normalize(m);
    }

    template <typename Distribution>
    T pdf_visible_normals(
        const Distribution&  mdf,
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const
    {
        assert(is_normalized(v));

        const T cos_theta_v = MDF<T>::cos_theta(v);

        if (cos_theta_v == T(0.0))
            return T(0.0);

        return
            mdf.G1(v, h, alpha_x, alpha_y) * std::abs(dot(v, h)) *
            mdf.D(h, alpha_x, alpha_y) / std::abs(cos_theta_v);
    }
};


//
// Blinn-Phong Microfacet Distribution Function.
//
// References:
//
//   Physically Based Rendering, first edition, pp. 444-447 and 681-684
//
//   Microfacet Models for Refraction through Rough Surfaces
//   http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
//
//   Importance Sampling Microfacet-Based BSDFs using the Distribution of Visible Normals.
//   https://hal.inria.fr/hal-00996995/en
//

template <typename T>
class BlinnMDF
  : public MDF<T>
{
  public:
    BlinnMDF() {}

    virtual T D(
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return (alpha_x + T(2.0)) * T(RcpTwoPi) * std::pow(MDF<T>::cos_theta(h), alpha_x);
    }

    virtual T G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return std::min(G1(incoming, h, alpha_x, alpha_y), G1(outgoing, h, alpha_x, alpha_y));
    }

    T G1(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  m,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        if (v.y <= T(0.0))
            return T(0.0);

        if (dot(v, m) <= T(0.0))
            return T(0.0);

        const T cos_vh = std::abs(dot(v, m));
        if (cos_vh == T(0.0))
            return T(0.0);

        return std::min(T(1.0), T(2.0) * std::abs(m.y * v.y) / cos_vh);
    }

    virtual Vector<T, 3> sample(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  s,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        const T cos_theta = std::pow(T(1.0) - s[0], T(1.0) / (alpha_x + T(2.0)));
        const T sin_theta = std::sqrt(T(1.0) - cos_theta * cos_theta);

        T cos_phi, sin_phi;
        MDF<T>::sample_phi(s[1], cos_phi, sin_phi);

        const Vector<T, 3> h =
            Vector<T, 3>::make_unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);

        return MDF<T>::v_cavity_choose_microfacet_normal(v, h, s[2]);
    }

    virtual T pdf(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF<T>::pdf_visible_normals(*this, v, h, alpha_x, alpha_y);
    }
};


//
// Anisotropic Beckmann Microfacet Distribution Function.
//
// References:
//
//   [1] http://en.wikipedia.org/wiki/Specular_highlight#Beckmann_distribution
//
//   [2] A Microfacet Based Coupled Specular-Matte BRDF Model with Importance Sampling
//       http://sirkan.iit.bme.hu/~szirmay/scook.pdf
//
//   [3] Microfacet Models for Refraction through Rough Surfaces
//       http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
//
//   [4] Understanding the Masking-Shadowing Function in Microfacet-Based BRDFs
//       http://hal.inria.fr/docs/00/96/78/44/PDF/RR-8468.pdf
//
//   [5] Importance Sampling Microfacet-Based BSDFs using the Distribution of Visible Normals.
//       https://hal.inria.fr/hal-00996995/en
//
//   [6] An Improved Visible Normal Sampling Routine for the Beckmann Distribution.
//       http://www.mitsuba-renderer.org/~wenzel/files/visnormal.pdf
//

template <typename T>
class BeckmannMDF
  : public MDF<T>
{
  public:
    BeckmannMDF() {}

    virtual T D(
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        const T cos_theta = MDF<T>::cos_theta(h);

        if (cos_theta == T(0.0))
            return T(0.0);

        const T cos_theta_2 = square(cos_theta);
        const T cos_theta_4 = square(cos_theta_2);
        const T tan_theta_2 = (T(1.0) - cos_theta_2) / cos_theta_2;

        const T A = MDF<T>::stretched_roughness(
            h,
            MDF<T>::sin_theta(h),
            alpha_x,
            alpha_y);

        return std::exp(-tan_theta_2 * A) / (T(Pi) * alpha_x * alpha_y * cos_theta_4);
    }

    virtual T G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return T(1.0) / (T(1.0) + lambda(outgoing, alpha_x, alpha_y) + lambda(incoming, alpha_x, alpha_y));
    }

    virtual T G1(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  m,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return T(1.0) / (T(1.0) + lambda(v, alpha_x, alpha_y));
    }

    T lambda(
        const Vector<T, 3>&  v,
        const T              alpha_x,
        const T              alpha_y) const
    {
        const T cos_theta = MDF<T>::cos_theta(v);

        if (cos_theta == T(0.0))
            return T(0.0);

        const T sin_theta = MDF<T>::sin_theta(v);

        const T alpha =
            MDF<T>::projected_roughness(
                v,
                sin_theta,
                alpha_x,
                alpha_y);

        const T tan_theta = std::abs(sin_theta / cos_theta);
        const T a = T(1.0) / (alpha * tan_theta);

        if (a < T(1.6))
        {
            const T a2 = square(a);
            return (T(1.0) - T(1.259) * a + T(0.396) * a2) / (T(3.535) * a + T(2.181) * a2);
        }

        return T(0.0);
    }

    virtual Vector<T, 3> sample(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  s,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return
            MDF<T>::sample_visible_normals(
                *this,
                v,
                s,
                alpha_x,
                alpha_y);
    }

    // This code comes from OpenShadingLanguage test render.
    Vector<T, 2> sample11(
        const T             cos_theta,
        const Vector<T, 3>& s) const
    {
        const T ct = std::max(cos_theta, T(1.0e-6));
        const T tan_theta = std::sqrt(T(1.0) - square(ct)) / ct;
        const T cot_theta = T(1.0) / tan_theta;

        // Sample slope X:
        // compute a coarse approximation using the approximation:
        // exp(-ierf(x)^2) ~= 1 - x * x
        // solve y = 1 + b + K * (1 - b * b).

        const T c = erf(cot_theta);
        const T K = tan_theta / std::sqrt(Pi);
        const T y_approx = s[0] * (T(1.0) + c + K * (1 - c * c));
        const T y_exact  = s[0] * (T(1.0) + c + K * std::exp(-square(cot_theta)));
        T b = K > T(0.0)
            ? (T(0.5) - std::sqrt(K * (K - y_approx + T(1.0)) + T(0.25))) / K
            : y_approx - T(1.0);

        // Perform a Newton step to refine toward the true root.
        T inv_erf = erf_inv(b);
        T value = T(1.0) + b + K * std::exp(-square(inv_erf)) - y_exact;

        // Check if we are close enough already.
        // This also avoids NaNs as we get close to the root.
        Vector2d slope;

        if (std::abs(value) > T(1.0e-6))
        {
            b -= value / (T(1.0) - inv_erf * tan_theta); // newton step 1
            inv_erf = erf_inv(b);
            value = T(1.0) + b + K * std::exp(-square(inv_erf)) - y_exact;
            b -= value / (T(1.0) - inv_erf * tan_theta); // newton step 2
            // Compute the slope from the refined value.
            slope[0] = erf_inv(b);
        }
        else
            slope[0] = inv_erf;

        // Sample slope Y.
        slope[1] = erf_inv(T(2.0) * s[1] - T(1.0));
        return slope;
    }

    virtual T pdf(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF<T>::pdf_visible_normals(*this, v, h, alpha_x, alpha_y);
    }

  private:

    //
    //  Reference:
    //
    //    Handbook of Mathematical Functions.
    //    Abramowitz and Stegun.
    //    http://people.math.sfu.ca/~cbm/aands/toc.htm
    //
    //  Copied from from pbrt-v3.

    inline T erf(const T x) const
    {
        // Constants.
        const T a1 = T(0.254829592);
        const T a2 = T(-0.284496736);
        const T a3 = T(1.421413741);
        const T a4 = T(-1.453152027);
        const T a5 = T(1.061405429);
        const T p = T(0.3275911);

        // A&S formula 7.1.26.
        const T abs_x = std::abs(x);
        const T t = T(1.0) / (T(1.0) + p * abs_x);
        const T y = T(1.0) - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * std::exp(-square(abs_x * abs_x));
        return x < T(0.0) ? -y : y;
    }

    //
    //  Reference:
    //
    //    Approximating the erfinv function, Mike Giles.
    //    https://people.maths.ox.ac.uk/gilesm/files/gems_erfinv.pdf
    //

    inline T erf_inv(const T x) const
    {
        const T y = clamp(x, T(-0.99999), T(0.99999));
        T w = -std::log((1 - y) * (1 + y));
        T p;

        if (w < T(5.0))
        {
            w = w - T(2.5);
            p = T(2.81022636e-08);
            p = T(3.43273939e-07) + p * w;
            p = T(-3.5233877e-06) + p * w;
            p = T(-4.39150654e-06) + p * w;
            p = T(0.00021858087) + p * w;
            p = T(-0.00125372503) + p * w;
            p = T(-0.00417768164) + p * w;
            p = T(0.246640727) + p * w;
            p = T(1.50140941) + p * w;
        }
        else
        {
            w = std::sqrt(w) - T(3.0);
            p = T(-0.000200214257);
            p = T(0.000100950558) + p * w;
            p = T(0.00134934322) + p * w;
            p = T(-0.00367342844) + p * w;
            p = T(0.00573950773) + p * w;
            p = T(-0.0076224613) + p * w;
            p = T(0.00943887047) + p * w;
            p = T(1.00167406) + p * w;
            p = T(2.83297682) + p * w;
        }

        return p * y;
    }
};


//
// Anisotropic GGX Microfacet Distribution Function.
//
// References:
//
//   [1] Microfacet Models for Refraction through Rough Surfaces
//       http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
//
//   [2] Understanding the Masking-Shadowing Function in Microfacet-Based BRDFs
//       http://hal.inria.fr/docs/00/96/78/44/PDF/RR-8468.pdf
//
//   [3] Importance Sampling Microfacet-Based BSDFs using the Distribution of Visible Normals.
//       https://hal.inria.fr/hal-00996995/en
//

template <typename T>
class GGXMDF
  : public MDF<T>
{
  public:
    GGXMDF() {}

    virtual T D(
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        const T cos_theta = MDF<T>::cos_theta(h);

        if (cos_theta == T(0.0))
            return square(alpha_x) * T(RcpPi);

        const T cos_theta_2 = square(cos_theta);
        const T cos_theta_4 = square(cos_theta_2);
        const T tan_theta_2 = (T(1.0) - cos_theta_2) / cos_theta_2;

        const T A =
            MDF<T>::stretched_roughness(
                h,
                MDF<T>::sin_theta(h),
                alpha_x,
                alpha_y);

        const T tmp = T(1.0) + tan_theta_2 * A;
        return T(1.0) / (T(Pi) * alpha_x * alpha_y * cos_theta_4 * square(tmp));
    }

    virtual T G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return T(1.0) / (T(1.0) + lambda(outgoing, alpha_x, alpha_y) + lambda(incoming, alpha_x, alpha_y));
    }

    virtual T G1(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  m,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return T(1.0) / (T(1.0) + lambda(v, alpha_x, alpha_y));
    }

    T lambda(
        const Vector<T, 3>&  v,
        const T              alpha_x,
        const T              alpha_y) const
    {
        const T cos_theta = std::abs(v.y);

        if (cos_theta == T(0.0))
            return T(0.0);

        const T cos_theta_2 = square(cos_theta);
        const T sin_theta = MDF<T>::sin_theta(v);
        const T tan_theta_2 = square(sin_theta) / cos_theta_2;

        const T alpha =
            MDF<T>::projected_roughness(
                v,
                sin_theta,
                alpha_x,
                alpha_y);

        const T a2_rcp = square(alpha) * tan_theta_2;
        return (T(-1.0) + std::sqrt(T(1.0) + a2_rcp)) * T(0.5);
    }

    virtual Vector<T, 3> sample(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  s,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return
            MDF<T>::sample_visible_normals(
                *this,
                v,
                s,
                alpha_x,
                alpha_y);
    }

    // Adapted from the sample code provided in [3].
    Vector<T, 2> sample11(
        const T             cos_theta,
        const Vector<T, 3>& s) const
    {
        const T sin_theta = std::sqrt(T(1.0) - square(cos_theta));

        // Special case (normal incidence).
        if (sin_theta < T(1.0e-4))
        {
            const T r = std::sqrt(s[0] / (T(1.0) - s[0]));
            const T cos_phi = std::cos(TwoPi * s[1]);
            const T sin_phi = std::sin(TwoPi * s[1]);
            return Vector<T, 2>(r * cos_phi, r * sin_phi);
        }

        Vector<T, 2> slope;

        // Precomputations.
        const T tan_theta = sin_theta / cos_theta;
        const T tan_theta2 = square(tan_theta);
        const T cot_theta = T(1.0) / tan_theta;
        const T G1 = T(2.0) / (T(1.0) + std::sqrt(T(1.0) + tan_theta2));

        // Sample slope x.
        const T A = T(2.0) * s[0] / G1 - T(1.0);
        const T A2 = square(A);
        const T rcp_A2_minus_one = T(1.0) / (A2 - T(1.0));
        const T B = tan_theta;
        const T B2 = square(B);

        const T D = std::sqrt(B2 * square(rcp_A2_minus_one) - (A2 - B2) * rcp_A2_minus_one);
        const T slope_x_1 = B * rcp_A2_minus_one - D;
        const T slope_x_2 = B * rcp_A2_minus_one + D;

        if (A < T(0.0) || slope_x_2 > cot_theta)
            slope[0] = slope_x_1;
        else
            slope[0] = slope_x_2;

        // Sample slope y.
        const T z =
            (s[1] * (s[1] * (s[1] * T(0.27385) - T(0.73369)) + T(0.46341))) /
            (s[1] * (s[1] * (s[1] * T(0.093073) + T(0.309420)) - T(1.0)) + T(0.597999));

        const T S = s[2] < T(0.5) ? T(1.0) : T(-1.0);
        slope[1] = S * z * std::sqrt(T(1.0) + square(slope[0]));
        return slope;
    }

    virtual T pdf(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF<T>::pdf_visible_normals(*this, v, h, alpha_x, alpha_y);
    }
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
  : public MDF<T>
{
  public:
    WardMDF() {}

    virtual T D(
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        const T cos_theta = MDF<T>::cos_theta(h);

        assert(cos_theta >= T(0.0));

        if (cos_theta == T(0.0))
            return T(0.0);

        const T cos_theta_2 = cos_theta * cos_theta;
        const T cos_theta_3 = cos_theta * cos_theta_2;
        const T tan_alpha_2 = (T(1.0) - cos_theta_2) / cos_theta_2;

        const T alpha_x2 = square(alpha_x);
        return std::exp(-tan_alpha_2 / alpha_x2) / (alpha_x2 * T(Pi) * cos_theta_3);
    }

    virtual T G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return std::min(G1(incoming, h, alpha_x, alpha_y), G1(outgoing, h, alpha_x, alpha_y));
    }

    virtual T G1(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  m,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        if (v.y <= T(0.0))
            return T(0.0);

        if (dot(v, m) <= T(0.0))
            return T(0.0);

        const T cos_vh = std::abs(dot(v, m));
        if (cos_vh == T(0.0))
            return T(0.0);

        return std::min(T(1.0), T(2.0) * std::abs(m.y * v.y) / cos_vh);
    }

    virtual Vector<T, 3> sample(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  s,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        const T tan_alpha_2 = square(alpha_x) * (-std::log(T(1.0) - s[0]));
        const T cos_alpha = T(1.0) / std::sqrt(T(1.0) + tan_alpha_2);
        const T sin_alpha = cos_alpha * std::sqrt(tan_alpha_2);
        const T phi = TwoPi * s[1];

        return Vector<T, 3>::make_unit_vector(cos_alpha, sin_alpha, std::cos(phi), std::sin(phi));
    }

    virtual T pdf(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return D(h, alpha_x, alpha_y);
    }
};


//
// Berry Microfacet Distribution Function.
// Used in the Disney BRDF clearcoat layer.
//

template <typename T>
class BerryMDF
  : public MDF<T>
{
  public:
    BerryMDF() {}

    virtual T D(
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        const T alpha_x_2 = square(alpha_x);
        const T cos_theta_2 = square(MDF<T>::cos_theta(h));
        const T a = (alpha_x_2 - T(1.0)) / (T(Pi) * std::log(alpha_x_2));
        const T b = (T(1.0) / (T(1.0) + (alpha_x_2 - T(1.0)) * cos_theta_2));
        return a * b;
    }

    virtual T G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return
            G1(outgoing, h, alpha_x, alpha_y) *
            G1(incoming, h, alpha_x, alpha_y);
    }

    virtual T G1(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  m,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        if (dot(v, m) * v.y <= T(0.0))
            return T(0.0);

        const T cos_theta = std::abs(v.y);
        const T cos_theta_2 = square(cos_theta);
        const T sin_theta = MDF<T>::sin_theta(v);
        const T tan_theta_2 = square(sin_theta) / cos_theta_2;

        if (tan_theta_2 == T(0.0))
            return T(1.0);

        const T a2_rcp = square(alpha_x) * tan_theta_2;
        const T lambda = (T(-1.0) + std::sqrt(T(1.0) + a2_rcp)) * T(0.5);
        return T(1.0) / (T(1.0) + lambda);
    }

    virtual Vector<T, 3> sample(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  s,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        const T alpha_x_2 = square(alpha_x);
        const T a = T(1.0) - std::pow(alpha_x_2, T(1.0) - s[0]);
        const T cos_theta = std::sqrt(a / (T(1.0) - alpha_x_2));
        const T sin_theta  = std::sqrt(T(1.0) - square(cos_theta));

        T cos_phi, sin_phi;
        MDF<T>::sample_phi(s[1], cos_phi, sin_phi);
        return Vector<T, 3>::make_unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);
    }

    virtual T pdf(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return D(h, alpha_x, alpha_y) * MDF<T>::cos_theta(h);
    }
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_MICROFACET_H
