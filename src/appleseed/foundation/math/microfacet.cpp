
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
        cos_phi = std::cos(phi);
        sin_phi = std::sin(phi);
    }

    void sample_phi(
        const float         s,
        const float         alpha_x,
        const float         alpha_y,
        float&              cos_phi,
        float&              sin_phi)
    {
        const float phi = TwoPi<float>() * s;
        Vector2f sin_cos_phi(
            std::cos(phi) * alpha_x,
            std::sin(phi) * alpha_y);
        sin_cos_phi = normalize(sin_cos_phi);
        cos_phi = sin_cos_phi[0];
        sin_phi = sin_cos_phi[1];
    }

    float stretched_roughness(
        const Vector3f&     m,
        const float         sin_theta,
        const float         alpha_x,
        const float         alpha_y)
    {
        if (alpha_x == alpha_y || sin_theta == 0.0f)
            return 1.0f / square(alpha_x);

        const float cos_phi_2_ax_2 = square(m.x / (sin_theta * alpha_x));
        const float sin_phi_2_ay_2 = square(m.z / (sin_theta * alpha_y));
        return cos_phi_2_ax_2 + sin_phi_2_ay_2;
    }

    float projected_roughness(
        const Vector3f&     m,
        const float         sin_theta,
        const float         alpha_x,
        const float         alpha_y)
    {
        if (alpha_x == alpha_y || sin_theta == 0.0f)
            return alpha_x;

        const float cos_phi_2_ax_2 = square((m.x * alpha_x) / sin_theta);
        const float sin_phi_2_ay_2 = square((m.z * alpha_y) / sin_theta);
        return std::sqrt(cos_phi_2_ax_2 + sin_phi_2_ay_2);
    }

    template <typename MDF>
    float pdf_visible_normals(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y)
    {
        assert(is_normalized(v));

        const float cos_theta_v = v.y;

        if (cos_theta_v == 0.0f)
            return 0.0f;

        return
            MDF::G1(v, m, alpha_x, alpha_y) * std::abs(dot(v, m)) *
            MDF::D(m, alpha_x, alpha_y) / std::abs(cos_theta_v);
    }
}


//
// BlinnMDF class implementation.
//

float BlinnMDF::D(
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return (alpha_x + 2.0f) * RcpTwoPi<float>() * std::pow(std::abs(m.y), alpha_x);
}

float BlinnMDF::G(
    const Vector3f&     wi,
    const Vector3f&     wo,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return std::min(
        G1(wi, m, alpha_x, alpha_y),
        G1(wo, m, alpha_x, alpha_y));
}

float BlinnMDF::G1(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    const float cos_vm = dot(v, m);
    if (cos_vm == 0.0f)
        return 0.0f;

    return std::min(1.0f, 2.0f * std::abs(m.y * v.y / cos_vm));
}

Vector3f BlinnMDF::sample(
    const Vector3f&     v,
    const Vector2f&     s,
    const float         alpha_x,
    const float         alpha_y)
{
    const float cos_theta = std::pow(1.0f - s[0], 1.0f / (alpha_x + 2.0f));
    const float sin_theta = std::sqrt(std::max(0.0f, 1.0f - square(cos_theta)));

    float cos_phi, sin_phi;
    sample_phi(s[1], cos_phi, sin_phi);

    return Vector3f::make_unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);
}

float BlinnMDF::pdf(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return pdf_visible_normals<BlinnMDF>(v, m, alpha_x, alpha_y);
}


//
// BeckmannMDF class implementation.
//

float BeckmannMDF::D(
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    const float cos_theta = m.y;
    if (cos_theta == 0.0f)
        return 0.0f;

    const float cos_theta_2 = square(cos_theta);
    const float sin_theta = std::sqrt(std::max(0.0f, 1.0f - cos_theta_2));
    const float cos_theta_4 = square(cos_theta_2);
    const float tan_theta_2 = (1.0f - cos_theta_2) / cos_theta_2;

    const float A =
        stretched_roughness(
            m,
            sin_theta,
            alpha_x,
            alpha_y);

    return std::exp(-tan_theta_2 * A) / (Pi<float>() * alpha_x * alpha_y * cos_theta_4);
}

float BeckmannMDF::G(
    const Vector3f&     wi,
    const Vector3f&     wo,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return 1.0f / (1.0f + lambda(wo, alpha_x, alpha_y) + lambda(wi, alpha_x, alpha_y));
}

float BeckmannMDF::G1(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return 1.0f / (1.0f + lambda(v, alpha_x, alpha_y));
}

float BeckmannMDF::lambda(
    const Vector3f&     v,
    const float         alpha_x,
    const float         alpha_y)
{
    const float cos_theta = v.y;
    if (cos_theta == 0.0f)
        return 0.0f;

    const float sin_theta = std::sqrt(std::max(0.0f, 1.0f - square(cos_theta)));

    // Normal incidence. No shadowing.
    if (sin_theta == 0.0f)
        return 0.0f;

    const float alpha =
        projected_roughness(
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

Vector3f BeckmannMDF::sample(
    const Vector3f&     v,
    const Vector2f&     s,
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

    Vector2f slope = sample_slope(cos_theta, s);

    // Rotate.
    const float cos_phi = std::cos(phi);
    const float sin_phi = std::sin(phi);
    slope = Vector2f(
        cos_phi * slope[0] - sin_phi * slope[1],
        sin_phi * slope[0] + cos_phi * slope[1]);

    // Unstretch and normalize.
    const Vector3f m(
        -slope[0] * alpha_x,
        1.0f,
        -slope[1] * alpha_y);
    return normalize(m);
}

// This code comes from OpenShadingLanguage test render.
Vector2f BeckmannMDF::sample_slope(
    const float         cos_theta,
    const Vector2f&     s)
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

    const float ct = std::max(cos_theta, threshold);
    const float tan_theta = std::sqrt(1.0f - square(ct)) / ct;
    const float cot_theta = 1.0f / tan_theta;

    // Compute a coarse approximation using the approximation:
    // exp(-ierf(x)^2) ~= 1 - x * x
    // solve y = 1 + b + K * (1 - b * b).

    const float c = std::erf(cot_theta);

    const float K = tan_theta / SqrtPi<float>();
    const float y_approx = s[0] * (1.0f + c + K * (1 - c * c));
    const float y_exact  = s[0] * (1.0f + c + K * std::exp(-square(cot_theta)));
    float b = (0.5f - std::sqrt(K * (K - y_approx + 1.0f) + 0.25f)) / K;

    // Perform a Newton step to refine toward the true root.
    float inv_erf = erf_inv(b);
    float value = 1.0f + b + K * std::exp(-square(inv_erf)) - y_exact;

    // Check if we are close enough already.
    // This also avoids NaNs as we get close to the root.
    if (std::abs(value) > threshold)
    {
        // Newton step 1.
        b -= value / (1.0f - inv_erf * tan_theta);

        inv_erf = erf_inv(b);
        value = 1.0f + b + K * std::exp(-square(inv_erf)) - y_exact;

        // Newton step 2.
        b -= value / (1.0f - inv_erf * tan_theta);

        // Compute the slope from the refined value.
        slope[0] = erf_inv(b);
    }
    else
        slope[0] = inv_erf;

    return slope;
}

float BeckmannMDF::pdf(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return pdf_visible_normals<BeckmannMDF>(v, m, alpha_x, alpha_y);
}


//
// GGXMDF class implementation.
//

float GGXMDF::D(
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    const float cos_theta = m.y;
    if (cos_theta == 0.0f)
        return square(alpha_x) * RcpPi<float>();

    const float cos_theta_2 = square(cos_theta);
    const float sin_theta = std::sqrt(std::max(0.0f, 1.0f - cos_theta_2));
    const float cos_theta_4 = square(cos_theta_2);
    const float tan_theta_2 = (1.0f - cos_theta_2) / cos_theta_2;

    const float A =
        stretched_roughness(
            m,
            sin_theta,
            alpha_x,
            alpha_y);

    const float tmp = 1.0f + tan_theta_2 * A;
    return 1.0f / (Pi<float>() * alpha_x * alpha_y * cos_theta_4 * square(tmp));
}

float GGXMDF::G(
    const Vector3f&     wi,
    const Vector3f&     wo,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return 1.0f / (1.0f + lambda(wo, alpha_x, alpha_y) + lambda(wi, alpha_x, alpha_y));
}

float GGXMDF::G1(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return 1.0f / (1.0f + lambda(v, alpha_x, alpha_y));
}

float GGXMDF::lambda(
    const Vector3f&     v,
    const float         alpha_x,
    const float         alpha_y)
{
    const float cos_theta = v.y;
    if (cos_theta == 0.0f)
        return 0.0f;

    const float cos_theta_2 = square(cos_theta);
    const float sin_theta = std::sqrt(std::max(0.0f, 1.0f - cos_theta_2));

    const float alpha =
        projected_roughness(
            v,
            sin_theta,
            alpha_x,
            alpha_y);

    const float tan_theta_2 = square(sin_theta) / cos_theta_2;
    const float a2_rcp = square(alpha) * tan_theta_2;
    return (-1.0f + std::sqrt(1.0f + a2_rcp)) * 0.5f;
}

Vector3f GGXMDF::sample(
    const Vector3f&     v,
    const Vector2f&     s,
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

    // Build an orthonormal basis.
    const Vector3f t1 =
        v.y < 0.9999f
            ? normalize(cross(stretched, Vector3f(0.0f, 1.0f, 0.0f)))
            : Vector3f(1.0f, 0.0f, 0.0f);
    const Vector3f t2 = cross(t1, stretched);

    // Sample point with polar coordinates (r, phi).
    const float a = 1.0f / (1.0f + stretched.y);
    const float r = std::sqrt(s[0]);
    const float phi =
        s[1] < a
            ? s[1] / a * Pi<float>()
            : Pi<float>() + (s[1] - a) / (1.0f - a) * Pi<float>();

    const float p1 = r * std::cos(phi);
    const float p2 = r * std::sin(phi) * (s[1] < a ? 1.0f : stretched.y);

    // Compute normal.
    const Vector3f h =
        p1 * t1 + p2 * t2 + std::sqrt(std::max(0.0f, 1.0f - p1 * p1 - p2 * p2)) * stretched;

    // Unstretch and normalize.
    const Vector3f m(
        h.x * alpha_x,
        std::max(0.0f, h.y),
        h.z * alpha_y);
    return normalize(m);
}

float GGXMDF::pdf(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return pdf_visible_normals<GGXMDF>(v, m, alpha_x, alpha_y);
}

float GGXMDF::D(
    const Vector3f&     m,
    const float         alpha)
{
    const float cos_theta = m.y;
    if (cos_theta == 0.0f)
        return square(alpha) * RcpPi<float>();

    const float a2 = square(alpha);
    const float cos_theta_2 = square(cos_theta);
    const float cos_theta_4 = square(cos_theta_2);
    const float tan_theta_2 = (1.0f - cos_theta_2) / cos_theta_2;

    const float A = 1.0f / a2;
    const float tmp = 1.0f + tan_theta_2 * A;
    return 1.0f / (Pi<float>() * a2 * cos_theta_4 * square(tmp));
}

float GGXMDF::G(
    const Vector3f&     wi,
    const Vector3f&     wo,
    const Vector3f&     m,
    const float         alpha)
{
    return 1.0f / (1.0f + lambda(wo, alpha) + lambda(wi, alpha));
}

float GGXMDF::G1(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha)
{
    return 1.0f / (1.0f + lambda(v, alpha));
}

float GGXMDF::lambda(
    const Vector3f&     v,
    const float         alpha)
{
    const float cos_theta = v.y;
    if (cos_theta == 0.0f)
        return 0.0f;

    const float cos_theta_2 = square(cos_theta);
    const float sin_theta = std::sqrt(std::max(0.0f, 1.0f - cos_theta_2));

    const float tan_theta_2 = square(sin_theta) / cos_theta_2;
    const float a2_rcp = square(alpha) * tan_theta_2;
    return (-1.0f + std::sqrt(1.0f + a2_rcp)) * 0.5f;
}

Vector3f GGXMDF::sample(
    const Vector3f&     v,
    const Vector2f&     s,
    const float         alpha)
{
    return sample(v, s, alpha, alpha);
}

float GGXMDF::pdf(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha)
{
    assert(is_normalized(v));

    const float cos_theta_v = v.y;

    if (cos_theta_v == 0.0f)
        return 0.0f;

    return
        G1(v, m, alpha) * std::abs(dot(v, m)) *
        D(m, alpha) / std::abs(cos_theta_v);
}

//
// WardMDF class implementation.
//

float WardMDF::D(
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    const float cos_theta = m.y;

    assert(cos_theta >= 0.0f);

    if (cos_theta == 0.0f)
        return 0.0f;

    const float cos_theta_2 = cos_theta * cos_theta;
    const float cos_theta_3 = cos_theta * cos_theta_2;
    const float tan_alpha_2 = (1.0f - cos_theta_2) / cos_theta_2;

    const float alpha_x2 = square(alpha_x);
    return std::exp(-tan_alpha_2 / alpha_x2) / (alpha_x2 * Pi<float>() * cos_theta_3);
}

float WardMDF::G(
    const Vector3f&     wi,
    const Vector3f&     wo,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return std::min(
        G1(wi, m, alpha_x, alpha_y),
        G1(wo, m, alpha_x, alpha_y));
}

float WardMDF::G1(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    if (v.y <= 0.0f)
        return 0.0f;

    if (dot(v, m) <= 0.0f)
        return 0.0f;

    const float cos_vm = std::abs(dot(v, m));
    if (cos_vm == 0.0f)
        return 0.0f;

    return std::min(1.0f, 2.0f * std::abs(m.y * v.y) / cos_vm);
}

Vector3f WardMDF::sample(
    const Vector3f&     v,
    const Vector2f&     s,
    const float         alpha_x,
    const float         alpha_y)
{
    const float tan_alpha_2 = square(alpha_x) * (-std::log(1.0f - s[0]));
    const float cos_alpha = 1.0f / std::sqrt(1.0f + tan_alpha_2);
    const float sin_alpha = cos_alpha * std::sqrt(tan_alpha_2);
    const float phi = TwoPi<float>() * s[1];
    return Vector3f::make_unit_vector(cos_alpha, sin_alpha, std::cos(phi), std::sin(phi));
}

float WardMDF::pdf(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return D(m, alpha_x, alpha_y);
}


//
// GTR1MDF class implementation.
//

float GTR1MDF::D(
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    const float alpha = clamp(alpha_x, 0.001f, 0.999f);
    const float alpha_x_2 = square(alpha);
    const float cos_theta_2 = square(m.y);
    const float a = (alpha_x_2 - 1.0f) / (Pi<float>() * std::log(alpha_x_2));
    const float b = (1.0f / (1.0f + (alpha_x_2 - 1.0f) * cos_theta_2));
    return a * b;
}

float GTR1MDF::G(
    const Vector3f&     wi,
    const Vector3f&     wo,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return 1.0f / (1.0f + lambda(wo, alpha_x, alpha_y) + lambda(wi, alpha_x, alpha_y));
}

float GTR1MDF::G1(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return 1.0f / (1.0f + lambda(v, alpha_x, alpha_y));
}

float GTR1MDF::lambda(
    const Vector3f&     v,
    const float         alpha_x,
    const float         alpha_y)
{
    const float cos_theta = v.y;
    if (cos_theta == 0.0f)
        return 0.0f;

    // [2] section 3.2.
    const float cos_theta_2 = square(cos_theta);
    const float sin_theta = std::sqrt(std::max(0.0f, 1.0f - cos_theta_2));

    // Normal incidence. No shadowing.
    if (sin_theta == 0.0f)
        return 0.0f;

    const float cot_theta_2 = cos_theta_2 / square(sin_theta);
    const float cot_theta = std::sqrt(cot_theta_2);
    const float alpha = clamp(alpha_x, 0.001f, 0.999f);
    const float alpha_2 = square(alpha);

    const float a = std::sqrt(cot_theta_2 + alpha_2);
    const float b = std::sqrt(cot_theta_2 + 1.0f);
    const float c = std::log(cot_theta + b);
    const float d = std::log(cot_theta + a);

    return (a - b + cot_theta * (c - d)) / (cot_theta * std::log(alpha_2));
}

Vector3f GTR1MDF::sample(
    const Vector3f&     v,
    const Vector2f&     s,
    const float         alpha_x,
    const float         alpha_y)
{
    const float alpha = clamp(alpha_x, 0.001f, 0.999f);
    const float alpha_2 = square(alpha);
    const float a = 1.0f - std::pow(alpha_2, 1.0f - s[0]);
    const float cos_theta_2 = a / (1.0f - alpha_2);
    const float cos_theta = std::sqrt(cos_theta_2);
    const float sin_theta = std::sqrt(std::max(0.0f, 1.0f - cos_theta_2));

    float cos_phi, sin_phi;
    sample_phi(s[1], cos_phi, sin_phi);
    return Vector3f::make_unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);
}

float GTR1MDF::pdf(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y)
{
    return D(m, alpha_x, alpha_y) * std::abs(m.y);
}


//
// StdMDF class implementation
//

float StdMDF::D(
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma)
{
    const float cos_theta = m.y;
    if (cos_theta == 0.0f)
        return 0.0;

    const float cos_theta_2 = square(cos_theta);
    const float sin_theta = std::sqrt(std::max(0.0f, 1.0f - cos_theta_2));
    const float cos_theta_4 = square(cos_theta_2);
    const float tan_theta_2 = (1.0f - cos_theta_2) / cos_theta_2;

    const float A =
        stretched_roughness(
            m,
            sin_theta,
            alpha_x,
            alpha_y);

    // [1] Equation 18.
    const float den = 1.0f + tan_theta_2 * A / (gamma - 1.0f);
    const float den4 = std::pow(den, gamma / 4.0f);
    const float den0 = Pi<float>() * den4;
    const float den1 = alpha_x * den4;
    const float den2 = alpha_y * den4;
    const float den3 = cos_theta_4 * den4;
    return 1.0f / (den0 * den1 * den2 * den3);
}

float StdMDF::G(
    const Vector3f&     wi,
    const Vector3f&     wo,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma)
{
    assert(gamma > 1.5f);

    return 1.0f / (1.0f + lambda(wo, alpha_x, alpha_y, gamma) + lambda(wi, alpha_x, alpha_y, gamma));
}

float StdMDF::G1(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma)
{
    assert(gamma > 1.5f);

    return 1.0f / (1.0f + lambda(v, alpha_x, alpha_y, gamma));
}

float StdMDF::lambda(
    const Vector3f&     v,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma)
{
    const float cos_theta = std::abs(v.y);
    if (cos_theta == 0.0f)
        return 0.0f;

    const float sin_theta = std::sqrt(std::max(0.0f, 1.0f - square(cos_theta)));

    // Normal incidence. No shadowing.
    if (sin_theta == 0.0f)
        return 0.0f;

    const float A =
        projected_roughness(
            v,
            sin_theta,
            alpha_x,
            alpha_y);

    const float cot_theta_a = cos_theta / (sin_theta * A);
    const float Sg2 = S2(cot_theta_a, gamma);

    const float cot_theta_a_2 = square(cot_theta_a);
    const float pg1_cot_theta_a_2 = (gamma - 1.0f) + cot_theta_a_2;
    const float frac_a1_sg1 = (gamma - 1.0f) / pg1_cot_theta_a_2;
    float a1_sg1 = std::pow(frac_a1_sg1, gamma);
    a1_sg1 *= 1.0f / ((2.0f * gamma - 3.0f) * cot_theta_a);
    a1_sg1 *= std::pow(pg1_cot_theta_a_2, 3.0f / 2.0f);
    const float a2 = std::sqrt(gamma - 1.0f);
    const float a2_sg2 = a2 * Sg2;
    const float gfrac = gamma_fraction(gamma - 0.5f, gamma) / SqrtPi<float>();
    return (a1_sg1 + a2_sg2) * gfrac - 0.5f;
}

float StdMDF::pdf(
    const Vector3f&     v,
    const Vector3f&     m,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma)
{
    return D(m, alpha_x, alpha_y, gamma) * std::abs(m.y);
}

Vector3f StdMDF::sample(
    const Vector3f&     v,
    const Vector2f&     s,
    const float         alpha_x,
    const float         alpha_y,
    const float         gamma)
{
    // todo: implement the improved sampling strategy in the supplemental material of [1].

    float theta, phi;
    const float b = std::pow(1.0f - s[1], 1.0f / (1.0f - gamma)) - 1.0f;

    if (alpha_x == alpha_y)
    {
        // [1] Equations 16 and 17.
        phi = TwoPi<float>() * s[0];
        theta = std::atan(alpha_x * std::sqrt((gamma - 1.0f) * b));
    }
    else
    {
        // [1] Equation 19 plus the changes in the Mitsuba sample implementation.
        phi =
            std::atan(alpha_y / alpha_x * std::tan(Pi<float>() + TwoPi<float>() * s[0])) +
            Pi<float>() * std::floor(2.0f * s[0] + 0.5f);

        // [1] Equation 20.
        const float cos_phi_2 = square(std::cos(phi));
        const float sin_phi_2 = 1.0f - cos_phi_2;
        const float A = ((cos_phi_2 / square(alpha_x)) + (sin_phi_2 / square(alpha_y))) / (gamma - 1.0f);
        theta = std::atan(std::sqrt(b / A));
    }

    return Vector3f::make_unit_vector(theta, phi);
}

float StdMDF::S2(const float cot_theta, const float gamma)
{
    const float cot_theta_2 = square(cot_theta);
    const float cot_theta_3 = cot_theta_2 * cot_theta;
    const float gamma_2 = square(gamma);
    const float gamma_3 = gamma_2 * gamma;

    const float F21num = (1.066f * cot_theta + 2.655f * cot_theta_2 + 4.892f * cot_theta_3);
    const float F21den = (1.038f + 2.969f * cot_theta + 4.305f * cot_theta_2 + 4.418f *cot_theta_3);
    const float F21 = F21num / F21den;

    const float F22num = (14.402f - 27.145f * gamma + 20.574f * gamma_2 - 2.745f * gamma_3);
    const float F22den = (-30.612f + 86.567f * gamma - 84.341f * gamma_2 + 29.938f * gamma_3);
    const float F22 = F22num / F22den;

    const float F23num = (-129.404f + 324.987f * gamma - 299.305f * gamma_2 + 93.268f * gamma_3);
    const float F23den = (-92.609f + 256.006f * gamma - 245.663f * gamma_2 + 86.064f * gamma_3);
    const float F23 = F23num / F23den;

    const float F24num = (6.537f + 6.074f * cot_theta - 0.623f * cot_theta_2 + 5.223f * cot_theta_3);
    const float F24den = (6.538f + 6.103f * cot_theta - 3.218f * cot_theta_2 + 6.347f * cot_theta_3);
    const float F24 = F24num / F24den;

    return F21 * (F22 + F23 * F24);
}

}   // namespace foundation
