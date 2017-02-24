
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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

class MDF
  : public NonCopyable
{
  public:
    MDF() {}

    virtual ~MDF();

    virtual float D(
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const = 0;

    virtual float G(
        const Vector3f&     incoming,
        const Vector3f&     outgoing,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const = 0;

    virtual float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y) const = 0;

    virtual Vector3f sample(
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y) const = 0;

    virtual float pdf(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const = 0;

  protected:
    static float cos_theta(const Vector3f& v)
    {
        return v.y;
    }

    static float sin_theta(const Vector3f& v)
    {
        return std::sqrt(std::max(0.0f, 1.0f - square(cos_theta(v))));
    }

    static void sample_phi(
        const float         s,
        float&              cos_phi,
        float&              sin_phi)
    {
        const float phi = TwoPi<float>() * s;
        cos_phi = std::cos(phi);
        sin_phi = std::sin(phi);
    }

    static void sample_phi(
        const float         s,
        const float         alpha_x,
        const float         alpha_y,
        float&              cos_phi,
        float&              sin_phi)
    {
        Vector2f sin_cos_phi(
            std::cos(TwoPi<float>() * s) * alpha_x,
            std::sin(TwoPi<float>() * s) * alpha_y);
        sin_cos_phi = normalize(sin_cos_phi);
        cos_phi = sin_cos_phi[0];
        sin_phi = sin_cos_phi[1];
    }

    static float stretched_roughness(
        const Vector3f&     h,
        const float         sin_theta,
        const float         alpha_x,
        const float         alpha_y)
    {
        if (alpha_x == alpha_y || sin_theta == 0.0f)
            return 1.0f / square(alpha_x);

        const float cos_phi_2_ax_2 = square(h.x / (sin_theta * alpha_x));
        const float sin_phi_2_ay_2 = square(h.z / (sin_theta * alpha_y));
        return cos_phi_2_ax_2 + sin_phi_2_ay_2;
    }

    static float projected_roughness(
        const Vector3f&     h,
        const float         sin_theta,
        const float         alpha_x,
        const float         alpha_y)
    {
        if (alpha_x == alpha_y || sin_theta == 0.0f)
            return alpha_x;

        const float cos_phi_2_ax_2 = square((h.x * alpha_x) / sin_theta);
        const float sin_phi_2_ay_2 = square((h.z * alpha_y) / sin_theta);
        return std::sqrt(cos_phi_2_ax_2 + sin_phi_2_ay_2);
    }

    static Vector3f v_cavity_choose_microfacet_normal(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         s)
    {
        // Preconditions.
        assert(is_normalized(v));
        assert(v.y >= 0.0f);

        const Vector3f hm(-h[0], h[1], -h[2]);
        const float dot_vh  = std::max(dot(v, h), 0.0f);
        const float dot_vhm = std::max(dot(v, hm), 0.0f);

        if (dot_vhm == 0.0f)
            return h;

        const float w = dot_vhm / (dot_vh + dot_vhm);
        return s < w ? hm : h;
    }

    template <typename Distribution>
    static Vector3f sample_visible_normals(
        const Distribution& mdf,
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y)
    {
        // Preconditions.
        assert(is_normalized(v));

        // Stretch incident.
        const float sign_cos_vn = v.y < 0.0f ? -1.0f : 1.0f;
        Vector3f stretched(
            sign_cos_vn * v[0] * alpha_x,
            sign_cos_vn * v[1],
            sign_cos_vn * v[2] * alpha_y);

        stretched = normalize(stretched);

        const float cos_theta = stretched[1];
        const float phi =
            stretched[1] < 0.99999f ? std::atan2(stretched[2], stretched[0]) : 0.0f;

        // Sample slope.
        Vector2f slope = mdf.sample11(cos_theta, s);

        // Rotate.
        const float cos_phi = std::cos(phi);
        const float sin_phi = std::sin(phi);
        slope = Vector2f(
            cos_phi * slope[0] - sin_phi * slope[1],
            sin_phi * slope[0] + cos_phi * slope[1]);

        // Stretch and normalize.
        const Vector3f m(
            -slope[0] * alpha_x,
            1.0f,
            -slope[1] * alpha_y);
        return normalize(m);
    }

    template <typename Distribution>
    static float pdf_visible_normals(
        const Distribution& mdf,
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y)
    {
        assert(is_normalized(v));

        const float cos_theta_v = MDF::cos_theta(v);

        if (cos_theta_v == 0.0f)
            return 0.0f;

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

class BlinnMDF
  : public MDF
{
  public:
    BlinnMDF() {}

    virtual float D(
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return (alpha_x + 2.0f) * RcpTwoPi<float>() * std::pow(MDF::cos_theta(h), alpha_x);
    }

    virtual float G(
        const Vector3f&     incoming,
        const Vector3f&     outgoing,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return std::min(G1(incoming, h, alpha_x, alpha_y), G1(outgoing, h, alpha_x, alpha_y));
    }

    float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        if (v.y <= 0.0f)
            return 0.0f;

        if (dot(v, m) <= 0.0f)
            return 0.0f;

        const float cos_vh = std::abs(dot(v, m));
        if (cos_vh == 0.0f)
            return 0.0f;

        return std::min(1.0f, 2.0f * std::abs(m.y * v.y) / cos_vh);
    }

    virtual Vector3f sample(
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        const float cos_theta = std::pow(1.0f - s[0], 1.0f / (alpha_x + 2.0f));
        const float sin_theta = std::sqrt(1.0f - cos_theta * cos_theta);

        float cos_phi, sin_phi;
        MDF::sample_phi(s[1], cos_phi, sin_phi);

        const Vector3f h =
            Vector3f::make_unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);

        return MDF::v_cavity_choose_microfacet_normal(v, h, s[2]);
    }

    virtual float pdf(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF::pdf_visible_normals(*this, v, h, alpha_x, alpha_y);
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

class BeckmannMDF
  : public MDF
{
  public:
    BeckmannMDF() {}

    virtual float D(
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        const float cos_theta = MDF::cos_theta(h);

        if (cos_theta == 0.0f)
            return 0.0f;

        const float cos_theta_2 = square(cos_theta);
        const float cos_theta_4 = square(cos_theta_2);
        const float tan_theta_2 = (1.0f - cos_theta_2) / cos_theta_2;

        const float A = MDF::stretched_roughness(
            h,
            MDF::sin_theta(h),
            alpha_x,
            alpha_y);

        return std::exp(-tan_theta_2 * A) / (Pi<float>() * alpha_x * alpha_y * cos_theta_4);
    }

    virtual float G(
        const Vector3f&     incoming,
        const Vector3f&     outgoing,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return 1.0f / (1.0f + lambda(outgoing, alpha_x, alpha_y) + lambda(incoming, alpha_x, alpha_y));
    }

    virtual float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return 1.0f / (1.0f + lambda(v, alpha_x, alpha_y));
    }

    float lambda(
        const Vector3f&     v,
        const float         alpha_x,
        const float         alpha_y) const
    {
        const float cos_theta = MDF::cos_theta(v);

        if (cos_theta == 0.0f)
            return 0.0f;

        const float sin_theta = MDF::sin_theta(v);

        const float alpha =
            MDF::projected_roughness(
                v,
                sin_theta,
                alpha_x,
                alpha_y);

        const float tan_theta = std::abs(sin_theta / cos_theta);
        const float a = 1.0f / (alpha * tan_theta);

        if (a < 1.6f)
        {
            const float a2 = square(a);
            return (1.0f - 1.259f * a + 0.396f * a2) / (3.535f * a + 2.181f * a2);
        }

        return 0.0f;
    }

    virtual Vector3f sample(
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return
            MDF::sample_visible_normals(
                *this,
                v,
                s,
                alpha_x,
                alpha_y);
    }

    // This code comes from OpenShadingLanguage test render.
    Vector2f sample11(
        const float         cos_theta,
        const Vector3f&     s) const
    {
        const float ct = std::max(cos_theta, 1.0e-6f);
        const float tan_theta = std::sqrt(1.0f - square(ct)) / ct;
        const float cot_theta = 1.0f / tan_theta;

        // Sample slope X:
        // compute a coarse approximation using the approximation:
        // exp(-ierf(x)^2) ~= 1 - x * x
        // solve y = 1 + b + K * (1 - b * b).

        const float c = erf(cot_theta);
        const float K = tan_theta / SqrtPi<float>();
        const float y_approx = s[0] * (1.0f + c + K * (1 - c * c));
        const float y_exact  = s[0] * (1.0f + c + K * std::exp(-square(cot_theta)));
        float b = K > 0.0f
            ? (0.5f - std::sqrt(K * (K - y_approx + 1.0f) + 0.25f)) / K
            : y_approx - 1.0f;

        // Perform a Newton step to refine toward the true root.
        float inv_erf = erf_inv(b);
        float value = 1.0f + b + K * std::exp(-square(inv_erf)) - y_exact;

        // Check if we are close enough already.
        // This also avoids NaNs as we get close to the root.
        Vector2f slope;

        if (std::abs(value) > 1.0e-6f)
        {
            b -= value / (1.0f - inv_erf * tan_theta); // newton step 1
            inv_erf = erf_inv(b);
            value = 1.0f + b + K * std::exp(-square(inv_erf)) - y_exact;
            b -= value / (1.0f - inv_erf * tan_theta); // newton step 2
            // Compute the slope from the refined value.
            slope[0] = erf_inv(b);
        }
        else
            slope[0] = inv_erf;

        // Sample slope Y.
        slope[1] = erf_inv(2.0f * s[1] - 1.0f);
        return slope;
    }

    virtual float pdf(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF::pdf_visible_normals(*this, v, h, alpha_x, alpha_y);
    }

  private:

    //
    // Reference:
    //
    //   Handbook of Mathematical Functions.
    //   Abramowitz and Stegun.
    //   http://people.math.sfu.ca/~cbm/aands/toc.htm
    //
    // Copied from from pbrt-v3.
    //

    inline float erf(const float x) const
    {
        // Constants.
        const float a1 = 0.254829592f;
        const float a2 = -0.284496736f;
        const float a3 = 1.421413741f;
        const float a4 = -1.453152027f;
        const float a5 = 1.061405429f;
        const float p  = 0.3275911f;

        // A&S formula 7.1.26.
        const float abs_x = std::abs(x);
        const float t = 1.0f / (1.0f + p * abs_x);
        const float y = 1.0f - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * std::exp(-square(abs_x * abs_x));
        return x < 0.0f ? -y : y;
    }

    //
    // Reference:
    //
    //   Approximating the erfinv function, Mike Giles.
    //   https://people.maths.ox.ac.uk/gilesm/files/gems_erfinv.pdf
    //

    inline float erf_inv(const float x) const
    {
        const float y = clamp(x, -0.99999f, 0.99999f);
        float w = -std::log((1 - y) * (1 + y));
        float p;

        if (w < 5.0f)
        {
            w = w - 2.5f;
            p = 2.81022636e-08f;
            p = 3.43273939e-07f + p * w;
            p = -3.5233877e-06f + p * w;
            p = -4.39150654e-06f + p * w;
            p = 0.00021858087f + p * w;
            p = -0.00125372503f + p * w;
            p = -0.00417768164f + p * w;
            p = 0.246640727f + p * w;
            p = 1.50140941f + p * w;
        }
        else
        {
            w = std::sqrt(w) - 3.0f;
            p = -0.000200214257f;
            p = 0.000100950558f + p * w;
            p = 0.00134934322f + p * w;
            p = -0.00367342844f + p * w;
            p = 0.00573950773f + p * w;
            p = -0.0076224613f + p * w;
            p = 0.00943887047f + p * w;
            p = 1.00167406f + p * w;
            p = 2.83297682f + p * w;
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

class GGXMDF
  : public MDF
{
  public:
    GGXMDF() {}

    virtual float D(
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        const float cos_theta = MDF::cos_theta(h);

        if (cos_theta == 0.0f)
            return square(alpha_x) * RcpPi<float>();

        const float cos_theta_2 = square(cos_theta);
        const float cos_theta_4 = square(cos_theta_2);
        const float tan_theta_2 = (1.0f - cos_theta_2) / cos_theta_2;

        const float A =
            MDF::stretched_roughness(
                h,
                MDF::sin_theta(h),
                alpha_x,
                alpha_y);

        const float tmp = 1.0f + tan_theta_2 * A;
        return 1.0f / (Pi<float>() * alpha_x * alpha_y * cos_theta_4 * square(tmp));
    }

    virtual float G(
        const Vector3f&     incoming,
        const Vector3f&     outgoing,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return 1.0f / (1.0f + lambda(outgoing, alpha_x, alpha_y) + lambda(incoming, alpha_x, alpha_y));
    }

    virtual float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return 1.0f / (1.0f + lambda(v, alpha_x, alpha_y));
    }

    float lambda(
        const Vector3f&     v,
        const float         alpha_x,
        const float         alpha_y) const
    {
        const float cos_theta = std::abs(v.y);

        if (cos_theta == 0.0f)
            return 0.0f;

        const float cos_theta_2 = square(cos_theta);
        const float sin_theta = MDF::sin_theta(v);
        const float tan_theta_2 = square(sin_theta) / cos_theta_2;

        const float alpha =
            MDF::projected_roughness(
                v,
                sin_theta,
                alpha_x,
                alpha_y);

        const float a2_rcp = square(alpha) * tan_theta_2;
        return (-1.0f + std::sqrt(1.0f + a2_rcp)) * 0.5f;
    }

    virtual Vector3f sample(
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return
            MDF::sample_visible_normals(
                *this,
                v,
                s,
                alpha_x,
                alpha_y);
    }

    // Adapted from the sample code provided in [3].
    Vector2f sample11(
        const float         cos_theta,
        const Vector3f&     s) const
    {
        const float sin_theta = std::sqrt(1.0f - square(cos_theta));

        // Special case (normal incidence).
        if (sin_theta < 1.0e-4f)
        {
            const float r = std::sqrt(s[0] / (1.0f - s[0]));
            const float cos_phi = std::cos(TwoPi<float>() * s[1]);
            const float sin_phi = std::sin(TwoPi<float>() * s[1]);
            return Vector2f(r * cos_phi, r * sin_phi);
        }

        Vector2f slope;

        // Precomputations.
        const float tan_theta = sin_theta / cos_theta;
        const float tan_theta2 = square(tan_theta);
        const float cot_theta = 1.0f / tan_theta;
        const float G1 = 2.0f / (1.0f + std::sqrt(1.0f + tan_theta2));

        // Sample slope x.
        const float A = 2.0f * s[0] / G1 - 1.0f;
        const float A2 = square(A);
        const float rcp_A2_minus_one = std::min(1.0f / (A2 - 1.0f), 1.0e10f);
        const float B = tan_theta;
        const float B2 = square(B);
        const float D = std::sqrt(std::max(B2 * square(rcp_A2_minus_one) - (A2 - B2) * rcp_A2_minus_one, 0.0f));
        const float slope_x_1 = B * rcp_A2_minus_one - D;
        const float slope_x_2 = B * rcp_A2_minus_one + D;
        slope[0] =
            A < 0.0f || slope_x_2 > cot_theta
                ? slope_x_1
                : slope_x_2;

        // Sample slope y.
        const float z =
            (s[1] * (s[1] * (s[1] * 0.27385f - 0.73369f) + 0.46341f)) /
            (s[1] * (s[1] * (s[1] * 0.093073f + 0.309420f) - 1.0f) + 0.597999f);
        const float S = s[2] < 0.5f ? 1.0f : -1.0f;
        slope[1] = S * z * std::sqrt(1.0f + square(slope[0]));

        return slope;
    }

    virtual float pdf(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return MDF::pdf_visible_normals(*this, v, h, alpha_x, alpha_y);
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

class WardMDF
  : public MDF
{
  public:
    WardMDF() {}

    virtual float D(
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        const float cos_theta = MDF::cos_theta(h);

        assert(cos_theta >= 0.0f);

        if (cos_theta == 0.0f)
            return 0.0f;

        const float cos_theta_2 = cos_theta * cos_theta;
        const float cos_theta_3 = cos_theta * cos_theta_2;
        const float tan_alpha_2 = (1.0f - cos_theta_2) / cos_theta_2;

        const float alpha_x2 = square(alpha_x);
        return std::exp(-tan_alpha_2 / alpha_x2) / (alpha_x2 * Pi<float>() * cos_theta_3);
    }

    virtual float G(
        const Vector3f&     incoming,
        const Vector3f&     outgoing,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return std::min(G1(incoming, h, alpha_x, alpha_y), G1(outgoing, h, alpha_x, alpha_y));
    }

    virtual float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        if (v.y <= 0.0f)
            return 0.0f;

        if (dot(v, m) <= 0.0f)
            return 0.0f;

        const float cos_vh = std::abs(dot(v, m));
        if (cos_vh == 0.0f)
            return 0.0f;

        return std::min(1.0f, 2.0f * std::abs(m.y * v.y) / cos_vh);
    }

    virtual Vector3f sample(
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        const float tan_alpha_2 = square(alpha_x) * (-std::log(1.0f - s[0]));
        const float cos_alpha = 1.0f / std::sqrt(1.0f + tan_alpha_2);
        const float sin_alpha = cos_alpha * std::sqrt(tan_alpha_2);
        const float phi = TwoPi<float>() * s[1];

        return Vector3f::make_unit_vector(cos_alpha, sin_alpha, std::cos(phi), std::sin(phi));
    }

    virtual float pdf(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return D(h, alpha_x, alpha_y);
    }
};


//
// GTR1 Microfacet Distribution Function.
//
// References:
//
//   [1] Physically-Based Shading at Disney
//       https://disney-animation.s3.amazonaws.com/library/s2012_pbs_disney_brdf_notes_v2.pdf
//
//   [2] Deriving the Smith shadowing function G1 for gamma (0, 4]
//       https://docs.chaosgroup.com/download/attachments/7147732/gtr_shadowing.pdf?version=2&modificationDate=1434539612000&api=v2
//

class GTR1MDF
  : public MDF
{
  public:
    GTR1MDF() {}

    virtual float D(
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        const float alpha_x_2 = square(alpha_x);
        const float cos_theta_2 = square(MDF::cos_theta(h));
        const float a = (alpha_x_2 - 1.0f) / (Pi<float>() * std::log(alpha_x_2));
        const float b = (1.0f / (1.0f + (alpha_x_2 - 1.0f) * cos_theta_2));
        return a * b;
    }

    virtual float G(
        const Vector3f&     incoming,
        const Vector3f&     outgoing,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return 1.0f / (1.0f + lambda(outgoing, alpha_x, alpha_y) + lambda(incoming, alpha_x, alpha_y));
    }

    virtual float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return 1.0f / (1.0f + lambda(v, alpha_x, alpha_y));
    }

    float lambda(
        const Vector3f&     v,
        const float         alpha_x,
        const float         alpha_y) const
    {
        const float cos_theta = std::abs(v.y);

        if (cos_theta == 0.0f)
            return 0.0f;

        if (cos_theta == 1.0f)
            return 1.0f;

        // [2] section 3.2.
        const float cos_theta_2 = square(cos_theta);
        const float sin_theta = MDF::sin_theta(v);
        const float cot_theta_2 = cos_theta_2 / square(sin_theta);
        const float cot_theta = std::sqrt(cot_theta_2);
        const float alpha_2 = square(alpha_x);

        const float a = std::sqrt(cot_theta_2 + alpha_2);
        const float b = std::sqrt(cot_theta_2 + 1.0f);
        const float c = std::log(cot_theta + b);
        const float d = std::log(cot_theta + a);

        return (a - b + cot_theta * (c - d)) / (cot_theta * std::log(alpha_2));
    }

    virtual Vector3f sample(
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        const float alpha2 = square(alpha_x);
        const float a = 1.0f - std::pow(alpha2, 1.0f - s[0]);
        const float cos_theta = std::sqrt(a / (1.0f - alpha2));
        const float sin_theta  = std::sqrt(1.0f - square(cos_theta));

        float cos_phi, sin_phi;
        MDF::sample_phi(s[1], cos_phi, sin_phi);
        return Vector3f::make_unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);
    }

    virtual float pdf(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y) const APPLESEED_OVERRIDE
    {
        return D(h, alpha_x, alpha_y) * MDF::cos_theta(h);
    }
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_MICROFACET_H
