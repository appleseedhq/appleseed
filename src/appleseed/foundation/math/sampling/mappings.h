
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
#include "foundation/math/sphericaltriangle.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>

namespace foundation
{

//
// References:
//
//   http://www.cs.kuleuven.ac.be/~phil/GI/TotalCompendium.pdf
//   http://jgt.akpeters.com/papers/ShirleyChiu97/
//   http://psgraphics.blogspot.com/2011/01/improved-code-for-concentric-map.html
//


//
// Sphere sampling functions.
//

// Map a uniform sample in [0,1)^2 to a direction over the unit sphere
// with a uniform probability density p(theta) = 1/(4*Pi).
template <typename T>
Vector<T, 3> sample_sphere_uniform(const Vector<T, 2>& s);


//
// Hemisphere sampling functions.
//

// Map a uniform sample in [0,1)^2 to a direction over the unit hemisphere
// with a uniform probability density p(theta) = 1/(2*Pi).
template <typename T>
Vector<T, 3> sample_hemisphere_uniform(const Vector<T, 2>& s);

// Map a uniform sample in [0,1)^2 to a direction over the unit hemisphere
// with a cosine-weighted probability density p(theta) = cos(theta)/Pi.
template <typename T>
Vector<T, 3> sample_hemisphere_cosine(const Vector<T, 2>& s);

// Map a uniform sample in [0,1)^2 to a direction over the unit hemisphere
// with a cosine lobe probability density p(theta) = (n+1)/(2*Pi)*cos(theta)^n.
template <typename T>
Vector<T, 3> sample_hemisphere_cosine_power(const Vector<T, 2>& s, const T n);
template <typename T>
T sample_hemisphere_cosine_power_pdf(const T cos_theta, const T n);


//
// Disk sampling functions.
//

// Map a uniform sample in [0,1)^2 to a point on the surface of the unit disk
// with a uniform probability density p(x) = 1/Pi.
template <typename T>
Vector<T, 2> sample_disk_uniform(const Vector<T, 2>& s);

// An alternate implementation of foundation::sample_disk_uniform() based on
// a polar parameterization of the unit disk. This implementation is about
// twice as fast as foundation::sample_disk_uniform() but has high distortion
// around the center of the disk. In practice it is fine for random or even
// QMC sampling.
template <typename T>
Vector<T, 2> sample_disk_uniform_alt(const Vector<T, 2>& s);

// Map a uniform random sample in [0,1) to a radius on the unit disk such
// that samples follow a 2D Gaussian distribution with probability density
//   pdf(x, y) = 4*a/Pi*exp(-a*(x^2+y^2))
//   pdf(r, theta) = r*pdf(x, y) = a/Pi*r*exp(-a*r^2)
template <typename T>
T sample_disk_gaussian(
    const T s,
    const T a);
template <typename T>
T sample_disk_gaussian_polar_pdf(
    const T r,
    const T a);
template <typename T>
T sample_disk_gaussian_area_pdf(
    const T r,
    const T a);


//
// Other sampling functions.
//

// Map a uniform sample in [0,1)^2 to a direction in the cone with its apex at
// the origin and extending toward Y+.
template <typename T>
Vector<T, 3> sample_cone_uniform(const Vector<T, 2>& s, const T cos_theta_max);
template <typename T>
T sample_cone_uniform_pdf(const T cos_theta_max);

// Map a uniform sample in [0,1) to a point on the unit circle with a uniform
// probability density p(x) = 1/(2*Pi).
template <typename T>
Vector<T, 2> sample_circle_uniform(const T s);

// Map a uniform sample in [0,1)^2 to a point on the surface of a triangle
// with a uniform probability density p(x) = 1/A. Return the barycentric
// coordinates of the point inside the triangle.
template <typename T>
Vector<T, 3> sample_triangle_uniform(const Vector<T, 2>& s);

// An alternate implementation of sample_triangle_uniform() due to Eric Heitz.
// See https://drive.google.com/file/d/1J-183vt4BrN9wmqItECIjjLIKwm29qSg/view
// for details.
template <typename T>
Vector<T, 3> sample_triangle_uniform_heitz(const Vector<T, 2>& s);

// Build a regular N-sided polygon to use with sample_regular_polygon_uniform().
template <typename T>
void build_regular_polygon(
    const size_t        vertex_count,
    const T             tilt_angle,
    Vector<T, 2>        vertices[]);

// Map a uniform sample in [0,1)^3 to a point on the surface of a regular
// N-sided polygon with a uniform probability density p(x) = 1/A.
template <typename T>
Vector<T, 2> sample_regular_polygon_uniform(
    const Vector<T, 3>& s,
    const size_t        vertex_count,
    const Vector<T, 2>  vertices[]);

// Map a uniform sample in [0,1)^2 to a point on the surface of a regular
// N-sided polygon with a uniform probability density p(x) = 1/A.
template <typename T>
Vector<T, 2> sample_regular_polygon_uniform(
    const Vector<T, 2>& s,
    const size_t        vertex_count,
    const Vector<T, 2>  vertices[]);

// Map a uniform sample in [0,1)^2 to a point on the surface of a spherical
// triangle on the unit sphere.
template <typename T>
Vector<T, 3> sample_spherical_triangle_uniform(
    const Vector<T, 3>& v0,
    const Vector<T, 3>& v1,
    const Vector<T, 3>& v2,
    const Vector<T, 2>& eta);


//
// Exponential sampling functions.
//
// Reference:
//
//   Physically Based Rendering, first edition, page 641.
//

// Map a uniform random sample in [0,1) to an exponential distribution
// of the form exp(-a*x).
template <typename T>
T sample_exponential_distribution(
    const T s,
    const T a);
template <typename T>
T exponential_distribution_pdf(
    const T x,
    const T a);

// Map a uniform random sample in [0,1) to an exponential distribution on segment [l, r].
// of the form exp(-a*(x - l)) - exp(-a*(x - r)).
template <typename T>
T sample_exponential_distribution_on_segment(
    const T s,
    const T a,
    const T l,
    const T r);
template <typename T>
T exponential_distribution_on_segment_pdf(
    const T x,
    const T a,
    const T l,
    const T r);


//
// Equiangular sampling along a ray with respect to a point.
//
// Reference:
//
//   Importance Sampling Techniques for Path Tracing in Participating Media
//   Christopher Kulla and Marcos Fajardo
//   https://www.solidangle.com/research/egsr2012_volume.pdf
//

template <typename T>
T sample_equiangular_distribution(
    const T u,                      // uniform random sample in [0,1)
    const T theta_a,                // start angle
    const T theta_b,                // end angle
    const T distance);              // distance from point to the ray
template <typename T>
T equiangular_distribution_pdf(
    const T t,
    const T theta_a,                // start angle
    const T theta_b,                // end angle
    const T distance);              // distance from point to the ray


//
// Reciprocal distribution:
// p(X) ~ 1 / X, where X belongs to [l, r).
//

template <typename T>
T sample_rcp_distribution(
    const T s,
    const T l,
    const T r);
template <typename T>
T rcp_distribution_pdf(
    const T x,
    const T l,
    const T r);


//
// Implementation.
//

template <typename T>
inline Vector<T, 3> sample_sphere_uniform(const Vector<T, 2>& s)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T phi = TwoPi<T>() * s[0];
    const T cos_theta = T(1.0) - T(2.0) * s[1];
    const T sin_theta = std::sqrt(T(1.0) - cos_theta * cos_theta);

    return
        Vector<T, 3>::make_unit_vector(
            cos_theta,
            sin_theta,
            std::cos(phi),
            std::sin(phi));
}

template <typename T>
inline Vector<T, 3> sample_hemisphere_uniform(const Vector<T, 2>& s)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T phi = TwoPi<T>() * s[0];
    const T cos_theta = T(1.0) - s[1];
    const T sin_theta = std::sqrt(T(1.0) - cos_theta * cos_theta);

    return
        Vector<T, 3>::make_unit_vector(
            cos_theta,
            sin_theta,
            std::cos(phi),
            std::sin(phi));
}

template <typename T>
inline Vector<T, 3> sample_hemisphere_cosine(const Vector<T, 2>& s)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T phi = TwoPi<T>() * s[0];
    const T cos_theta = std::sqrt(T(1.0) - s[1]);
    const T sin_theta = std::sqrt(s[1]);

    return
        Vector<T, 3>::make_unit_vector(
            cos_theta,
            sin_theta,
            std::cos(phi),
            std::sin(phi));
}

template <typename T>
inline Vector<T, 3> sample_hemisphere_cosine_power(const Vector<T, 2>& s, const T n)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));
    assert(n >= T(0.0));

    const T phi = TwoPi<T>() * s[0];
    const T cos_theta = std::pow(T(1.0) - s[1], T(1.0) / (n + T(1.0)));
    const T sin_theta = std::sqrt(T(1.0) - cos_theta * cos_theta);

    return
        Vector<T, 3>::make_unit_vector(
            cos_theta,
            sin_theta,
            std::cos(phi),
            std::sin(phi));
}

template <typename T>
inline T sample_hemisphere_cosine_power_pdf(const T cos_theta, const T n)
{
    return (n + T(1.0)) * RcpTwoPi<T>() * std::pow(cos_theta, n);
}

template <typename T>
inline Vector<T, 2> sample_disk_uniform(const Vector<T, 2>& s)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T a = T(2.0) * s[0] - T(1.0);
    const T b = T(2.0) * s[1] - T(1.0);

    T phi, r;

    if (a * a > b * b)
    {
        r = a;
        phi = PiOverFour<T>() * (b / a);
    }
    else if (b != T(0.0))
    {
        r = b;
        phi = HalfPi<T>() - PiOverFour<T>() * (a / b);
    }
    else return Vector<T, 2>(0.0);

    return Vector<T, 2>(r * std::cos(phi), r * std::sin(phi));
}

template <typename T>
inline Vector<T, 2> sample_disk_uniform_alt(const Vector<T, 2>& s)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T phi = TwoPi<T>() * s[0];
    const T r = std::sqrt(T(1.0) - s[1]);

    return r * Vector<T, 2>(std::cos(phi), std::sin(phi));
}

template <typename T>
inline T sample_disk_gaussian(
    const T s,
    const T a)
{
    assert(s >= T(0.0));
    assert(s < T(1.0));

    return std::sqrt(-std::log(T(1.0) - s) / a);
}

template <typename T>
inline T sample_disk_gaussian_polar_pdf(
    const T r,
    const T a)
{
    assert(r >= T(0.0));

    return a * RcpPi<T>() * r * std::exp(-a * r * r);
}

template <typename T>
inline T sample_disk_gaussian_area_pdf(
    const T r,
    const T a)
{
    assert(r >= T(0.0));

    return a * FourOverPi<T>() * std::exp(-a * r * r);
}

template <typename T>
inline Vector<T, 3> sample_cone_uniform(const Vector<T, 2>& s, const T cos_theta_max)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T phi = TwoPi<T>() * s[0];
    const T cos_theta = lerp(T(1.0), cos_theta_max, s[1]);
    const T sin_theta = std::sqrt(T(1.0) - cos_theta * cos_theta);

    return
        Vector<T, 3>::make_unit_vector(
            cos_theta,
            sin_theta,
            std::cos(phi),
            std::sin(phi));
}

template <typename T>
inline T sample_cone_uniform_pdf(const T cos_theta_max)
{
    return T(1.0) / (TwoPi<T>() * (T(1.0) - cos_theta_max));
}

template <typename T>
inline Vector<T, 2> sample_circle_uniform(const T s)
{
    assert(s >= T(0.0) && s < T(1.0));

    const T phi = s * TwoPi<T>();

    return Vector<T, 2>(std::cos(phi), std::sin(phi));
}

template <typename T>
inline Vector<T, 3> sample_triangle_uniform(const Vector<T, 2>& s)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T sqrt_s0 = std::sqrt(s[0]);

    Vector<T, 3> b;
    b[0] = T(1.0) - sqrt_s0;
    b[1] = (T(1.0) - s[1]) * sqrt_s0;
    b[2] = T(1.0) - b[0] - b[1];

    assert(feq(b[0] + b[1] + b[2], T(1.0)));

    return b;
}

template <typename T>
inline Vector<T, 3> sample_triangle_uniform_heitz(const Vector<T, 2>& s)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    T t0 = T(0.5) * s[0];
    T t1 = T(0.5) * s[1];

    const T offset = t1 - t0;

    //
    // Potentially branchless version of the block below:
    //
    //   t0 -= std::min(offset, T(0.0));
    //   t1 += std::max(offset, T(0.0));
    //

    if (offset > T(0.0))
        t1 += offset;
    else t0 -= offset;

    Vector<T, 3> b;
    b[0] = t0;
    b[1] = t1;
    b[2] = T(1.0) - t0 - t1;

    assert(feq(b[0] + b[1] + b[2], T(1.0)));

    return b;
}

template <typename T>
void build_regular_polygon(
    const size_t        vertex_count,
    const T             tilt_angle,
    Vector<T, 2>        vertices[])
{
    assert(vertex_count >= 3);

    for (size_t i = 0; i < vertex_count; ++i)
    {
        const T a = (i * TwoPi<T>()) / vertex_count + tilt_angle;
        vertices[i] = Vector<T, 2>(std::cos(a), std::sin(a));
    }
}

template <typename T>
inline Vector<T, 2> sample_regular_polygon_uniform(
    const Vector<T, 3>& s,
    const size_t        vertex_count,
    const Vector<T, 2>  vertices[])
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));
    assert(s[2] >= T(0.0) && s[2] < T(1.0));
    assert(vertex_count >= 3);

    const size_t v0_index = truncate<size_t>(s[0] * vertex_count);

    size_t v1_index = v0_index + 1;
    if (v1_index == vertex_count)
        v1_index = 0;

    const Vector<T, 2>& v0 = vertices[v0_index];
    const Vector<T, 2>& v1 = vertices[v1_index];

    T t0 = T(0.5) * s[1];
    T t1 = T(0.5) * s[2];

    const T offset = t1 - t0;

    if (offset > T(0.0))
        t1 += offset;
    else t0 -= offset;

    return t0 * v0 + t1 * v1;
}

template <typename T>
inline Vector<T, 2> sample_regular_polygon_uniform(
    const Vector<T, 2>& s,
    const size_t        vertex_count,
    const Vector<T, 2>  vertices[])
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));
    assert(vertex_count >= 3);

    const size_t v0_index = truncate<size_t>(s[0] * vertex_count);
    const T rescaled_s0 = s[0] * vertex_count - v0_index;
    assert(rescaled_s0 >= T(0.0) && rescaled_s0 < T(1.0));

    size_t v1_index = v0_index + 1;
    if (v1_index == vertex_count)
        v1_index = 0;

    const Vector<T, 2>& v0 = vertices[v0_index];
    const Vector<T, 2>& v1 = vertices[v1_index];

    T t0 = T(0.5) * rescaled_s0;
    T t1 = T(0.5) * s[1];

    const T offset = t1 - t0;

    if (offset > T(0.0))
        t1 += offset;
    else t0 -= offset;

    return t0 * v0 + t1 * v1;
}

template <typename T>
Vector<T, 3> sample_spherical_triangle_uniform(
    const Vector<T, 3>& A,
    const Vector<T, 3>& B,
    const Vector<T, 3>& C,
    const Vector<T, 2>& eta)
{
    // Compute the arc lengths of the sides of the spherical triangle.
    T a, b, c;
    compute_spherical_triangle_edge_lengths(A, B, C, a, b, c);

    // Compute the interior angles of the spherical triangle.
    T alpha, beta, gamma;
    compute_spherical_triangle_interior_angles(a, b, c, alpha, beta, gamma);
    const T cos_alpha = std::cos(alpha);
    const T sin_alpha = std::sin(alpha);

    // Compute the area of the spherical triangle.
    const T area = compute_spherical_triangle_area(alpha, beta, gamma);

    // Use one random variable to select the new area.
    const T area_hat = eta[0] * area;

    // Save the sine and cosine of the angle phi.
    const T s = std::sin(area_hat - alpha);
    const T t = std::cos(area_hat - alpha);

    // Compute the pair (u, v) that determines beta_hat.
    const T u = t - cos_alpha;
    const T v = s + sin_alpha * std::cos(c);

    // Let q be the cosine of the new edge length b_hat.
    const T q_num = (v * t - u * s) * cos_alpha - v;
    const T q_den = (v * s + u * t) * sin_alpha;
    const T q = clamp(q_num / q_den, T(-1.0), T(1.0));

    // Compute the third vertex of the sub-triangle.
    const Vector<T, 3> C_hat = q * A + std::sqrt(T(1.0) - q * q) * normalize(C - dot(C, A) * A);
    const T dot_C_hat_B = dot(C_hat, B);

    // Use the other random variable to select cos(theta).
    const T z = T(1.0) - eta[1] * (T(1.0) - dot_C_hat_B);

    // Construct the corresponding point on the sphere.
    return z * B + std::sqrt(T(1.0) - z * z) * normalize(C_hat - dot_C_hat_B * B);
}

template <typename T>
inline T sample_exponential_distribution(
    const T s,
    const T a)
{
    assert(s >= T(0.0));
    assert(s < T(1.0));

    return -std::log(T(1.0) - s) / a;
}

template <typename T>
inline T exponential_distribution_pdf(
    const T x,
    const T a)
{
    assert(x >= T(0.0));

    return a * std::exp(-a * x);
}

template <typename T>
inline T sample_exponential_distribution_on_segment(
    const T s,
    const T a,
    const T l,
    const T r)
{
    assert(s >= T(0.0));
    assert(s < T(1.0));
    assert(a > T(0.0));

    const T tail = T(1.0) - std::exp(-(r - l) * a);
    return clamp(l - std::log(T(1.0) - s * tail) / a, l, r);
}

template <typename T>
inline T exponential_distribution_on_segment_pdf(
    const T x,
    const T a,
    const T l,
    const T r)
{
    assert(x >= l);
    assert(x <= r);

    const T left = std::exp(a * (x - l));
    const T right = std::exp(a * (x - r));
    return a / (left - right);
}

template <typename T>
inline T sample_equiangular_distribution(
    const T s,
    const T theta_a,
    const T theta_b,
    const T distance)
{
    assert(s >= T(0.0));
    assert(s < T(1.0));

    return distance * std::tan(lerp(theta_a, theta_b, s));
}

template <typename T>
inline T equiangular_distribution_pdf(
    const T t,
    const T theta_a,
    const T theta_b,
    const T distance)
{
    return distance / ((theta_b - theta_a) * (square(distance) + square(t)));
}

template <typename T>
inline T sample_rcp_distribution(
    const T s,
    const T l,
    const T r)
{
    assert(l > T(0.0));
    assert(r > l);

    const T d = r / l;
    return
        d < T(1.01)
            ? l + s * (r - l)
            : clamp(l * std::pow(d, s), l, r);
}

template <typename T>
inline T rcp_distribution_pdf(
    const T x,
    const T l,
    const T r)
{
    assert(l > T(0.0));
    assert(x >= l);
    assert(x < r);

    const T d = r / l;
    return
        d < T(1.01)
            ? rcp(r - l)
            : rcp(x * std::log(d));
}

}   // namespace foundation
