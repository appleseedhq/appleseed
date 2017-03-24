
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

// Interface header.
#include "microfacet.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/specialfunctions.h"

// Standard headers.
#include <cassert>
#include <cmath>

using namespace std;

namespace foundation
{

namespace
{
    void sample_phi(
        const float         s,
        float&              cos_phi,
        float&              sin_phi)
    {
        const float phi = TwoPi<float>() * s;
        cos_phi = cos(phi);
        sin_phi = sin(phi);
    }

    void sample_phi(
        const float         s,
        const float         alpha_x,
        const float         alpha_y,
        float&              cos_phi,
        float&              sin_phi)
    {
        const float two_pi_s = TwoPi<float>() * s;
        Vector2f sin_cos_phi(
            cos(two_pi_s) * alpha_x,
            sin(two_pi_s) * alpha_y);
        sin_cos_phi = normalize(sin_cos_phi);
        cos_phi = sin_cos_phi[0];
        sin_phi = sin_cos_phi[1];
    }

    float stretched_roughness(
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

    float projected_roughness(
        const Vector3f&     h,
        const float         sin_theta,
        const float         alpha_x,
        const float         alpha_y)
    {
        if (alpha_x == alpha_y || sin_theta == 0.0f)
            return alpha_x;

        const float cos_phi_2_ax_2 = square((h.x * alpha_x) / sin_theta);
        const float sin_phi_2_ay_2 = square((h.z * alpha_y) / sin_theta);
        return sqrt(cos_phi_2_ax_2 + sin_phi_2_ay_2);
    }

    Vector3f v_cavity_choose_microfacet_normal(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         s)
    {
        // Preconditions.
        assert(is_normalized(v));
        assert(v.y >= 0.0f);

        const Vector3f hm(-h[0], h[1], -h[2]);
        const float dot_vh  = max(dot(v, h), 0.0f);
        const float dot_vhm = max(dot(v, hm), 0.0f);

        if (dot_vhm == 0.0f)
            return h;

        const float w = dot_vhm / (dot_vh + dot_vhm);
        return s < w ? hm : h;
    }

    template <typename Distribution>
    Vector3f sample_visible_normals(
        const Distribution& mdf,
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma)
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
            stretched[1] < 0.99999f ? atan2(stretched[2], stretched[0]) : 0.0f;

        // Sample slope.
        Vector2f slope = mdf.sample11(cos_theta, s, gamma);

        // Rotate.
        const float cos_phi = cos(phi);
        const float sin_phi = sin(phi);
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
    float pdf_visible_normals(
        const Distribution& mdf,
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma)
    {
        assert(is_normalized(v));

        const float cos_theta_v = v.y;

        if (cos_theta_v == 0.0f)
            return 0.0f;

        return
            mdf.G1(v, h, alpha_x, alpha_y, gamma) * abs(dot(v, h)) *
            mdf.D(h, alpha_x, alpha_y, gamma) / abs(cos_theta_v);
    }
}


//
// MDF class implementation.
//

MDF::~MDF()
{
}


//
// BlinnMDF class implementation.
//

float BlinnMDF::D(
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return (alpha_x + 2.0f) * RcpTwoPi<float>() * pow(h.y, alpha_x);
}

float BlinnMDF::G(
    const Vector3f&     incoming,
    const Vector3f&     outgoing,
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return min(
        G1(incoming, h, alpha_x, alpha_y, gamma),
        G1(outgoing, h, alpha_x, alpha_y, gamma));
}

float BlinnMDF::G1(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    if (v.y <= 0.0f)
        return 0.0f;

    if (dot(v, m) <= 0.0f)
        return 0.0f;

    const float cos_vh = abs(dot(v, m));
    if (cos_vh == 0.0f)
        return 0.0f;

    return min(1.0f, 2.0f * abs(m.y * v.y) / cos_vh);
}

Vector3f BlinnMDF::sample(
    const Vector3f&     v,
    const Vector3f&     s,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    const float cos_theta = pow(1.0f - s[0], 1.0f / (alpha_x + 2.0f));
    const float sin_theta = sqrt(max(0.0f, 1.0f - square(cos_theta)));

    float cos_phi, sin_phi;
    sample_phi(s[1], cos_phi, sin_phi);

    const Vector3f h =
        Vector3f::make_unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);

    return v_cavity_choose_microfacet_normal(v, h, s[2]);
}

float BlinnMDF::pdf(
    const Vector3f&     v,
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return pdf_visible_normals(*this, v, h, alpha_x, alpha_y, gamma);
}


//
// BeckmannMDF class implementation.
//

float BeckmannMDF::D(
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    const float cos_theta = h.y;

    if (cos_theta == 0.0f)
        return 0.0f;

    const float cos_theta_2 = square(cos_theta);
    const float sin_theta = sqrt(max(0.0f, 1.0f - cos_theta_2));
    const float cos_theta_4 = square(cos_theta_2);
    const float tan_theta_2 = (1.0f - cos_theta_2) / cos_theta_2;

    const float A = stretched_roughness(
        h,
        sin_theta,
        alpha_x,
        alpha_y);

    return exp(-tan_theta_2 * A) / (Pi<float>() * alpha_x * alpha_y * cos_theta_4);
}

float BeckmannMDF::G(
    const Vector3f&     incoming,
    const Vector3f&     outgoing,
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return 1.0f / (1.0f + lambda(outgoing, alpha_x, alpha_y, gamma) + lambda(incoming, alpha_x, alpha_y, gamma));
}

float BeckmannMDF::G1(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return 1.0f / (1.0f + lambda(v, alpha_x, alpha_y, gamma));
}

float BeckmannMDF::lambda(
    const Vector3f&     v,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    const float cos_theta = v.y;

    if (cos_theta == 0.0f)
        return 0.0f;

    const float sin_theta = sqrt(max(0.0f, 1.0f - square(cos_theta)));

    const float alpha =
        projected_roughness(
            v,
            sin_theta,
            alpha_x,
            alpha_y);

    const float tan_theta = abs(sin_theta / cos_theta);
    const float a = 1.0f / (alpha * tan_theta);

    if (a < 1.6f)
    {
        const float a2 = square(a);
        return (1.0f - 1.259f * a + 0.396f * a2) / (3.535f * a + 2.181f * a2);
    }

    return 0.0f;
}

Vector3f BeckmannMDF::sample(
    const Vector3f&     v,
    const Vector3f&     s,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return
        sample_visible_normals(
            *this,
            v,
            s,
            alpha_x,
            alpha_y,
            gamma);
}


// This code comes from OpenShadingLanguage test render.
Vector2f BeckmannMDF::sample11(
    const float         cos_theta,
    const Vector3f&     s,
    const float         gamma) const
{
    const float threshold = 1e-06f;
    Vector2f slope;

    // Sample slope Y.
    slope[1] = erf_inv(2.0f * s[1] - 1.0f);

    // Sample slope X:

    // Special case (normal incidence).
    if (cos_theta > 1.0f - threshold)
    {
        slope[0] = erf_inv(2.0f * s[0] - 1.0f);
        return slope;
    }

    const float ct = max(cos_theta, threshold);
    const float tan_theta = sqrt(1.0f - square(ct)) / ct;
    const float cot_theta = 1.0f / tan_theta;

    // Compute a coarse approximation using the approximation:
    // exp(-ierf(x)^2) ~= 1 - x * x
    // solve y = 1 + b + K * (1 - b * b).

    const float c = erf(cot_theta);

    const float K = tan_theta / SqrtPi<float>();
    const float y_approx = s[0] * (1.0f + c + K * (1 - c * c));
    const float y_exact  = s[0] * (1.0f + c + K * exp(-square(cot_theta)));
    float b = (0.5f - sqrt(K * (K - y_approx + 1.0f) + 0.25f)) / K;

    // Perform a Newton step to refine toward the true root.
    float inv_erf = erf_inv(b);
    float value = 1.0f + b + K * exp(-square(inv_erf)) - y_exact;

    // Check if we are close enough already.
    // This also avoids NaNs as we get close to the root.
    if (abs(value) > threshold)
    {
        b -= value / (1.0f - inv_erf * tan_theta); // newton step 1
        inv_erf = erf_inv(b);
        value = 1.0f + b + K * exp(-square(inv_erf)) - y_exact;
        b -= value / (1.0f - inv_erf * tan_theta); // newton step 2
        // Compute the slope from the refined value.
        slope[0] = erf_inv(b);
    }
    else
        slope[0] = inv_erf;

    return slope;
}

float BeckmannMDF::pdf(
    const Vector3f&     v,
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return pdf_visible_normals(*this, v, h, alpha_x, alpha_y, gamma);
}


//
// GGXMDF class implementation.
//

float GGXMDF::D(
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    const float cos_theta = h.y;

    if (cos_theta == 0.0f)
        return square(alpha_x) * RcpPi<float>();

    const float cos_theta_2 = square(cos_theta);
    const float sin_theta = sqrt(max(0.0f, 1.0f - cos_theta_2));
    const float cos_theta_4 = square(cos_theta_2);
    const float tan_theta_2 = (1.0f - cos_theta_2) / cos_theta_2;

    const float A =
        stretched_roughness(
            h,
            sin_theta,
            alpha_x,
            alpha_y);

    const float tmp = 1.0f + tan_theta_2 * A;
    return 1.0f / (Pi<float>() * alpha_x * alpha_y * cos_theta_4 * square(tmp));
}

float GGXMDF::G(
    const Vector3f&     incoming,
    const Vector3f&     outgoing,
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return 1.0f / (1.0f + lambda(outgoing, alpha_x, alpha_y, gamma) + lambda(incoming, alpha_x, alpha_y, gamma));
}

float GGXMDF::G1(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return 1.0f / (1.0f + lambda(v, alpha_x, alpha_y, gamma));
}

float GGXMDF::lambda(
    const Vector3f&     v,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    const float cos_theta = abs(v.y);

    if (cos_theta == 0.0f)
        return 0.0f;

    const float cos_theta_2 = square(cos_theta);
    const float sin_theta = sqrt(max(0.0f, 1.0f - square(cos_theta)));
    const float tan_theta_2 = square(sin_theta) / cos_theta_2;

    const float alpha =
        projected_roughness(
            v,
            sin_theta,
            alpha_x,
            alpha_y);

    const float a2_rcp = square(alpha) * tan_theta_2;
    return (-1.0f + sqrt(1.0f + a2_rcp)) * 0.5f;
}

Vector3f GGXMDF::sample(
    const Vector3f&     v,
    const Vector3f&     s,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return
        sample_visible_normals(
            *this,
            v,
            s,
            alpha_x,
            alpha_y,
            gamma);
}

// Adapted from the sample code provided in [3].
Vector2f GGXMDF::sample11(
    const float         cos_theta,
    const Vector3f&     s,
    const float         gamma) const
{
    const float sin_theta = sqrt(max(0.0f, 1.0f - square(cos_theta)));

    // Special case (normal incidence).
    if (sin_theta < 1.0e-4f)
    {
        const float r = sqrt(s[0] / (1.0f - s[0]));
        const float two_pi_s1 = TwoOverPi<float>() * s[1];
        const float cos_phi = cos(two_pi_s1);
        const float sin_phi = sin(two_pi_s1);
        return Vector2f(r * cos_phi, r * sin_phi);
    }

    Vector2f slope;

    // Precomputations.
    const float tan_theta = sin_theta / cos_theta;
    const float tan_theta2 = square(tan_theta);
    const float cot_theta = 1.0f / tan_theta;
    const float G1 = 2.0f / (1.0f + sqrt(1.0f + tan_theta2));

    // Sample slope x.
    const float A = 2.0f * s[0] / G1 - 1.0f;
    const float A2 = square(A);
    const float rcp_A2_minus_one = min(1.0f / (A2 - 1.0f), 1.0e10f);
    const float B = tan_theta;
    const float B2 = square(B);
    const float D = sqrt(max(B2 * square(rcp_A2_minus_one) - (A2 - B2) * rcp_A2_minus_one, 0.0f));
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
    slope[1] = S * z * sqrt(1.0f + square(slope[0]));

    return slope;
}

float GGXMDF::pdf(
    const Vector3f&     v,
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return pdf_visible_normals(*this, v, h, alpha_x, alpha_y, gamma);
}


//
// WardMDF class implementation.
//

float WardMDF::D(
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    const float cos_theta = h.y;

    assert(cos_theta >= 0.0f);

    if (cos_theta == 0.0f)
        return 0.0f;

    const float cos_theta_2 = cos_theta * cos_theta;
    const float cos_theta_3 = cos_theta * cos_theta_2;
    const float tan_alpha_2 = (1.0f - cos_theta_2) / cos_theta_2;

    const float alpha_x2 = square(alpha_x);
    return exp(-tan_alpha_2 / alpha_x2) / (alpha_x2 * Pi<float>() * cos_theta_3);
}

float WardMDF::G(
    const Vector3f&     incoming,
    const Vector3f&     outgoing,
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return min(
        G1(incoming, h, alpha_x, alpha_y, gamma),
        G1(outgoing, h, alpha_x, alpha_y, gamma));
}

float WardMDF::G1(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    if (v.y <= 0.0f)
        return 0.0f;

    if (dot(v, m) <= 0.0f)
        return 0.0f;

    const float cos_vh = abs(dot(v, m));
    if (cos_vh == 0.0f)
        return 0.0f;

    return min(1.0f, 2.0f * abs(m.y * v.y) / cos_vh);
}

Vector3f WardMDF::sample(
    const Vector3f&     v,
    const Vector3f&     s,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    const float tan_alpha_2 = square(alpha_x) * (-std::log(1.0f - s[0]));
    const float cos_alpha = 1.0f / sqrt(1.0f + tan_alpha_2);
    const float sin_alpha = cos_alpha * sqrt(tan_alpha_2);
    const float phi = TwoPi<float>() * s[1];

    return Vector3f::make_unit_vector(cos_alpha, sin_alpha, cos(phi), sin(phi));
}

float WardMDF::pdf(
    const Vector3f&     v,
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return D(h, alpha_x, alpha_y, gamma);
}


//
// GTR1MDF class implementation.
//

float GTR1MDF::D(
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    const float alpha_x_2 = square(alpha_x);
    const float cos_theta_2 = square(h.y);
    const float a = (alpha_x_2 - 1.0f) / (Pi<float>() * std::log(alpha_x_2));
    const float b = (1.0f / (1.0f + (alpha_x_2 - 1.0f) * cos_theta_2));
    return a * b;
}

float GTR1MDF::G(
    const Vector3f&     incoming,
    const Vector3f&     outgoing,
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return 1.0f / (1.0f + lambda(outgoing, alpha_x, alpha_y, gamma) + lambda(incoming, alpha_x, alpha_y, gamma));
}

float GTR1MDF::G1(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return 1.0f / (1.0f + lambda(v, alpha_x, alpha_y, gamma));
}

float GTR1MDF::lambda(
    const Vector3f&     v,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    const float cos_theta = abs(v.y);

    if (cos_theta == 0.0f)
        return 0.0f;

    if (cos_theta == 1.0f)
        return 1.0f;

    // [2] section 3.2.
    const float cos_theta_2 = square(cos_theta);
    const float sin_theta = sqrt(max(0.0f, 1.0f - square(cos_theta)));
    const float cot_theta_2 = cos_theta_2 / square(sin_theta);
    const float cot_theta = sqrt(cot_theta_2);
    const float alpha_2 = square(alpha_x);

    const float a = sqrt(cot_theta_2 + alpha_2);
    const float b = sqrt(cot_theta_2 + 1.0f);
    const float c = std::log(cot_theta + b);
    const float d = std::log(cot_theta + a);

    return (a - b + cot_theta * (c - d)) / (cot_theta * std::log(alpha_2));
}

Vector3f GTR1MDF::sample(
    const Vector3f&     v,
    const Vector3f&     s,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    const float alpha2 = square(alpha_x);
    const float a = 1.0f - pow(alpha2, 1.0f - s[0]);
    const float cos_theta = sqrt(a / (1.0f - alpha2));
    const float sin_theta  = sqrt(max(0.0f, 1.0f - square(cos_theta)));

    float cos_phi, sin_phi;
    sample_phi(s[1], cos_phi, sin_phi);
    return Vector3f::make_unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);
}

float GTR1MDF::pdf(
    const Vector3f&     v,
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return D(h, alpha_x, alpha_y, gamma) * h.y;
}


//
// StdMDF class implementation
//

float StdMDF::D(
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    const float cos_theta = h.y;

    if (cos_theta == 0.0f)
        return 0.0;

    const float cos_theta_2 = square(cos_theta);
    const float cos_theta_4 = square(cos_theta_2);
    const float alpha_x2 = square(alpha_x);
    const float tan_theta_2 = (1.0f - cos_theta_2) / cos_theta_2;

    // [1] Equation 11.
    // Following Disney implementation idea - divide gamma power by four first to avoid explosion
    const float A_a = pow(alpha_x, (2.0f * gamma - 2.0f) / 4.0f);
    const float A_b = pow(gamma - 1.0f, gamma / 4.0f);
    const float A_c = pow((gamma - 1.0f) * alpha_x2 + tan_theta_2, gamma / 4.0f);
    const float A_num4 = A_a * A_b;
    const float A_denom4 = max(A_c, numeric_limits<float>::epsilon());
    const float A4 = A_num4 / A_denom4;

    const float A2 = A4 * A4;
    const float A = A2 * A2;


    return A / (Pi<float>() * cos_theta_4);
}

float StdMDF::G(
    const Vector3f&     incoming,
    const Vector3f&     outgoing,
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    assert(gamma > 1.5f);

    return 1.0f / (1.0f + lambda(outgoing, alpha_x, alpha_y, gamma) + lambda(incoming, alpha_x, alpha_y, gamma));
}

float StdMDF::G1(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    assert(gamma > 1.5f);

    return 1.0f / (1.0f + lambda(v, alpha_x, alpha_y, gamma));
}

// Disney code provided in supplement of [1].
float StdMDF::gamma_fraction(
    const float         numerator_arg,
    const float         denominator_arg) const
{
    const float ab1 = abgamma(numerator_arg + 5.0f);
    const float ab2 = abgamma(denominator_arg + 5.0f);

    const float ac1 = 1.0f / (numerator_arg * (numerator_arg + 1.0f) * (numerator_arg + 2.0f) * (numerator_arg + 3.0f) * (numerator_arg + 4.0f));
    const float ac2 = 1.0f / (denominator_arg * (denominator_arg + 1.0f) * (denominator_arg + 2.0f) * (denominator_arg + 3.0f) * (denominator_arg + 4.0f));

    return exp(ab1 - ab2) * (ac1 / ac2);
}

// Disney code provided in supplement of [1].
float StdMDF::abgamma(
    const float         x) const
{
    const float gm0 = 1.0f /12.0f;
    const float gm1 = 1.0f / 30.0f;
    const float gm2 = 53.0f / 210.0f;
    const float gm3 = 195.0f / 371.0f;
    const float gm4 = 22999.0f / 22737.0f;
    const float gm5 = 29944523.0f / 19733142.0f;
    const float gm6 = 109535241009.0f / 48264275462.0f;

    return (0.5f * std::log(2.0f * Pi<float>()) - x + (x - 0.5f) * std::log(x)
             + gm0 / (x + gm1 / (x + gm2 / (x + gm3 / (x + gm4 / (x + gm5 / (x + gm6 / x)))))));
}

float StdMDF::lambda(
    const Vector3f&     v,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    const float alpha_2 = square(alpha_x);
    const float cos_theta = abs(v.y);
    const float sin_theta = sqrt(max(0.0f, 1.0f - square(cos_theta)));
    const float tan_theta = sin_theta / cos_theta;
    const float tan_theta_2 = square(tan_theta);

    if (tan_theta == 0.0f)
        return 0.0f;

    // If equation 14 from [1] would be implemented dirrectly as it is written,
    // the multiplier of S1, (we'll call it here A1), would explode for gamma
    // values over 30 because of exponent.
    // To avoid the exponential explosion, function needs to be rewritten
    // so that the base of the power lowers.
    // Here we combine A1 with part of S1 which is also raised to gamma.
    const float A1S1_a = gamma - 1.0f;
    const float A1S1_b = alpha_x * tan_theta / (2 * gamma - 3.0f);
    const float A1S1_c = (gamma - 1.0f + 1.0f / (alpha_2 * tan_theta_2));
    const float A1S1 = pow(A1S1_a / A1S1_c, gamma) * A1S1_b * pow(A1S1_c, 1.5f);

    const float A2 = sqrt(gamma - 1.0f);
    // S2 Rational fractions for GAF approximation [1] Equation 23.
    const float z = 1.0f / (alpha_x * tan_theta);
    const float z_2 = square(z);
    const float z_3 = z_2 * z;
    const float gamma_2 = square(gamma);
    const float gamma_3 = gamma_2 * gamma;
    const float F_21 = (1.066f * z + 2.655f * z_2 + 4.892f * z_3) / (1.038f + 2.969f * z + 4.305f * z_2 + 4.418f * z_3);
    const float F_22 = (14.402f - 27.145f * gamma + 20.574f * gamma_2 - 2.745f * gamma_3) / (-30.612f + 86.567f * gamma - 84.341f * gamma_2 +29.938f * gamma_3);
    const float F_23 = (-129.404f + 324.987f * gamma - 299.305f * gamma_2 + 93.268f * gamma_3) / (-92.609f + 256.006f * gamma - 245.663f * gamma_2 + 86.064f * gamma_3);
    const float F_24 = (6.537f + 6.074f * z - 0.623f * z_2 + 5.223f * z_3) / (6.538f + 6.103f * z - 3.218f * z_2 + 6.347f * z_3);
    const float S2 = F_21 * (F_22 + F_23 * F_24);

    return (gamma_fraction(gamma - 0.5f, gamma) / SqrtPi<float>()) * (A1S1 + A2 * S2) - 0.5f;
}

float StdMDF::pdf(
    const Vector3f&     v,
    const Vector3f&     h,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    return D(h, alpha_x, alpha_y, gamma) * h.y;
}

Vector3f StdMDF::sample(
    const Vector3f&     v,
    const Vector3f&     s,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma) const
{
    const float phi = TwoPi<float>() * s[0];
    const float a = gamma - 1.0f;
    const float b = pow(1.0f - s[1], 1.0f / (1.0f - gamma)) - 1.0f;
    const float theta = atan(alpha_x * sqrt(a * b));

    return Vector3f::make_unit_vector(theta, phi);
}

}   // namespace foundation
