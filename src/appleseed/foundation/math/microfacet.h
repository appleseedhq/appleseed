
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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

// Boost headers.
#include "boost/math/special_functions/erf.hpp"
#include "boost/math/special_functions/sign.hpp"
#include "boost/mpl/bool.hpp"

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

    T D(
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const
    {
        // Preconditions.
        assert(cos_theta(h) >= T(0.0));
        assert(alpha_x > T(0.0));
        assert(alpha_y > T(0.0));

        const T result = do_eval_D(h, alpha_x, alpha_y);

        // Postconditions.
        assert(result >= T(0.0));
        return result;
    }

    T G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const
    {
        // Preconditions.
        assert(alpha_x > T(0.0));
        assert(alpha_y > T(0.0));

        const T result = do_eval_G(incoming, outgoing, h, alpha_x, alpha_y);

        // Postconditions.
        assert(result >= T(0.0));
        return result;
    }

    T G1(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  m,
        const T              alpha_x,
        const T              alpha_y) const
    {
        // Preconditions.
        assert(alpha_x > T(0.0));
        assert(alpha_y > T(0.0));

        const T result = do_eval_G1(v, m, alpha_x, alpha_y);

        // Postconditions.
        assert(result >= T(0.0));
        return result;
    }

    Vector<T, 3> sample(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  s,
        const T              alpha_x,
        const T              alpha_y) const
    {
        // Preconditions.
        assert(s[0] >= T(0.0));
        assert(s[0] <  T(1.0));
        assert(s[1] >= T(0.0));
        assert(s[1] <  T(1.0));
        assert(s[2] >= T(0.0));
        assert(s[2] <  T(1.0));
        assert(alpha_x > T(0.0));
        assert(alpha_y > T(0.0));

        const Vector<T, 3> result = do_sample(v, s, alpha_x, alpha_y);

        // Postconditions.
        assert(is_normalized(result));
        return result;
    }

    T pdf(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const
    {
        // Preconditions.
        assert(cos_theta(h) >= T(0.0));
        assert(alpha_x > T(0.0));
        assert(alpha_y > T(0.0));

        const T result = do_eval_pdf(v, h, alpha_x, alpha_y);

        // Postconditions.
        assert(result >= T(0.0));
        return result;
    }

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

    static T v_cavity_G1(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h)
    {
        if (v.y <= T(0.0))
            return T(0.0);

        if (dot(v, h) <= T(0.0))
            return T(0.0);

        const T cos_vh = std::abs(dot(v, h));
        if (cos_vh == T(0.0))
            return T(0.0);

        return std::min(T(1.0), T(2.0) * std::abs(h.y * v.y) / cos_vh);
    }

    static T v_cavity_G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h)
    {
        return std::min(v_cavity_G1(incoming, h), v_cavity_G1(outgoing, h));
    }

    template <typename Distribution>
    static T smith_G1(
        const Distribution&  mdf,
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  m,
        const T              alpha_x,
        const T              alpha_y)
    {
        if (dot(v, m) * v.y <= T(0.0))
            return T(0.0);

        return T(1.0) / (T(1.0) + mdf.do_eval_lambda(v, alpha_x, alpha_y));
    }

    template <typename Distribution>
    static T smith_G(
        const Distribution&  mdf,
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y)
    {
        return
            mdf.do_eval_G1(incoming, h, alpha_x, alpha_y) *
            mdf.do_eval_G1(outgoing, h, alpha_x, alpha_y);
    }

    static Vector<T, 3> v_cavity_choose_microfacet_normal(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              s)
    {
        // Preconditions.
        assert(is_normalized(v));

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
        Vector<T, 3> stretched(
            v[0] * alpha_x,
            v[1],
            v[2] * alpha_y);

        stretched = normalize(stretched);

        const T cos_theta = stretched[1];
        const T phi =
            stretched[1] < T(0.99999) ? std::atan2(stretched[2], stretched[0]) : T(0.0);

        // Sample slope.
        Vector<T, 2> slope = mdf.do_sample_11(cos_theta, s);

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
    static T do_eval_Dw(
        const Distribution&  mdf,
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y)
    {
        assert(is_normalized(v));

        if (cos_theta(v) == T(0.0))
            return T(0.0);

        return
            mdf.do_eval_G1(v, h, alpha_x, alpha_y) *
            std::abs(dot(v, h)) *
            mdf.do_eval_D(h, alpha_x, alpha_y) /
            std::abs(cos_theta(v));
    }

  private:
    virtual T do_eval_D(
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const = 0;

    virtual T do_eval_G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const = 0;

    virtual T do_eval_G1(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  m,
        const T              alpha_x,
        const T              alpha_y) const = 0;

    virtual Vector<T, 3> do_sample(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  s,
        const T              alpha_x,
        const T              alpha_y) const = 0;

    virtual T do_eval_pdf(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const = 0;
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
    typedef boost::mpl::bool_<false> IsAnisotropicType;

    BlinnMDF() {}

  private:
    friend class MDF<T>;

    virtual T do_eval_D(
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return (alpha_x + T(2.0)) * T(RcpTwoPi) * std::pow(MDF<T>::cos_theta(h), alpha_x);
    }

    virtual T do_eval_G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF<T>::v_cavity_G(incoming, outgoing, h);
    }

    T do_eval_G1(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  m,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF<T>::v_cavity_G1(v, m);
    }

    virtual Vector<T, 3> do_sample(
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
            Vector<T, 3>::unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);

        return MDF<T>::v_cavity_choose_microfacet_normal(v, h, s[2]);
    }

    virtual T do_eval_pdf(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return
            MDF<T>::do_eval_Dw(
                *this,
                v,
                h,
                alpha_x,
                alpha_y);
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
    typedef boost::mpl::bool_<true> IsAnisotropicType;

    BeckmannMDF() {}

  private:
    friend class MDF<T>;

    virtual T do_eval_D(
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

    virtual T do_eval_G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF<T>::smith_G(*this, incoming, outgoing, h, alpha_x, alpha_y);
    }

    virtual T do_eval_G1(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  m,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF<T>::smith_G1(*this, v, m, alpha_x, alpha_y);
    }

    T do_eval_lambda(
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
        const T a = 1.0 / (alpha * tan_theta);

        if (a < T(1.6))
        {
            const T a2 = square(a);
            return (T(1.0) - T(1.259) * a + T(0.396) * a2) / (T(3.535) * a + T(2.181) * a2);
        }

        return T(0.0);
    }

    virtual Vector<T, 3> do_sample(
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

    virtual T do_eval_pdf(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return
            MDF<T>::do_eval_Dw(
                *this,
                v,
                h,
                alpha_x,
                alpha_y);
    }

    // This code comes from OpenShadingLanguage test render.
    Vector<T, 2> do_sample_11(
        const T             cos_theta,
        const Vector<T, 3>& s) const
    {
        const T ct = std::max(cos_theta, T(1e-6));
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

        if (std::abs(value) > T(1e-6))
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

    typedef boost::math::policies::policy<
        boost::math::policies::promote_float<false>,
        boost::math::policies::digits10<6>
    > ErfPolicyType;

    static T erf(const T x)
    {
        return boost::math::erf(x, ErfPolicyType());
    }

    static T erf_inv(T x)
    {
        if (std::abs(x) == T(1.0))
            x -= boost::math::copysign(T(1e-5), x);

        return boost::math::erf_inv(x, ErfPolicyType());
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
    typedef boost::mpl::bool_<true> IsAnisotropicType;

    GGXMDF() {}

  private:
    friend class MDF<T>;

    virtual T do_eval_D(
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

    virtual T do_eval_G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF<T>::smith_G(*this, incoming, outgoing, h, alpha_x, alpha_y);
    }

    virtual T do_eval_G1(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  m,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF<T>::smith_G1(*this, v, m, alpha_x, alpha_y);
    }

    T do_eval_lambda(
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

    virtual Vector<T, 3> do_sample(
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

    virtual T do_eval_pdf(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return
            MDF<T>::do_eval_Dw(
                *this,
                v,
                h,
                alpha_x,
                alpha_y);
    }

    // Adapted from the sample code provided in [3].
    Vector<T, 2> do_sample_11(
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
    typedef boost::mpl::bool_<false> IsAnisotropicType;

    WardMDF() {}

  private:
    virtual T do_eval_D(
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

    virtual T do_eval_G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF<T>::v_cavity_G(incoming, outgoing, h);
    }

    virtual T do_eval_G1(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  m,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF<T>::v_cavity_G1(v, m);
    }

    virtual Vector<T, 3> do_sample(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  s,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        const T tan_alpha_2 = square(alpha_x) * (-std::log(T(1.0) - s[0]));
        const T cos_alpha = T(1.0) / std::sqrt(T(1.0) + tan_alpha_2);
        const T sin_alpha = cos_alpha * std::sqrt(tan_alpha_2);
        const T phi = TwoPi * s[1];

        return Vector<T, 3>::unit_vector(cos_alpha, sin_alpha, std::cos(phi), std::sin(phi));
    }

    virtual T do_eval_pdf(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return do_eval_D(h, alpha_x, alpha_y);
    }
};

//
// Berry microfacet distribution function.
// It's used in the Disney BRDF clearcoat layer.
//

template <typename T>
class BerryMDF
  : public MDF<T>
{
  public:
    typedef boost::mpl::bool_<false> IsAnisotropicType;

    BerryMDF() {}

  private:
    virtual T do_eval_D(
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

    virtual T do_eval_G(
        const Vector<T, 3>&  incoming,
        const Vector<T, 3>&  outgoing,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return
            do_eval_G1(outgoing, h, alpha_x, alpha_y) *
            do_eval_G1(incoming, h, alpha_x, alpha_y);
    }

    virtual T do_eval_G1(
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

    virtual Vector<T, 3> do_sample(
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
        return Vector<T, 3>::unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);
    }

    virtual T do_eval_pdf(
        const Vector<T, 3>&  v,
        const Vector<T, 3>&  h,
        const T              alpha_x,
        const T              alpha_y) const APPLESEED_OVERRIDE
    {
        return do_eval_D(h, alpha_x, alpha_y) * MDF<T>::cos_theta(h);
    }
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_MICROFACET_H
