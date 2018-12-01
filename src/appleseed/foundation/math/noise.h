
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
#include "foundation/core/exceptions/exceptionnotimplemented.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>

namespace foundation
{

//
// Coherent gradient noise functions.
//
// All noise() functions return values in the range [-1, 1].
//
// References:
//
//   http://mrl.nyu.edu/~perlin/paper445.pdf
//   http://mrl.nyu.edu/~perlin/noise/
//   http://amd-dev.wpengine.netdna-cdn.com/wordpress/media/2012/10/Tatarchuk-Noise%28GDC07-D3D_Day%29.pdf
//   http://jcgt.org/published/0003/02/02/
//

template <typename T, size_t N>
T noise(const Vector<T, N>& p);

template <typename T>
T noise(const T x);


//
// Fractional Brownian Motion functions.
//
// All fbm() functions return values in the range [-1, 1].
//

template <typename T, size_t N>
T fbm(
    Vector<T, N>    p,
    const size_t    octaves,
    const T         lacunarity = T(2.0),
    const T         gain = T(0.5));

template <typename T>
T fbm(
    T               p,
    const size_t    octaves,
    const T         lacunarity = T(2.0),
    const T         gain = T(0.5));


//
// Turbulence functions.
//
// All turbulence() functions return values in the range [0, 1].
//

template <typename T, size_t N>
T turbulence(
    Vector<T, N>    p,
    const size_t    octaves,
    const T         lacunarity = T(2.0),
    const T         gain = T(0.5));

template <typename T>
T turbulence(
    T               p,
    const size_t    octaves,
    const T         lacunarity = T(2.0),
    const T         gain = T(0.5));


//
// Coherent gradient noise functions implementation.
//

namespace noise_impl
{

    struct Permutations
    {
        static const size_t Size = 256;
        size_t  m_p1[2 * Size];
        size_t  m_p2[2 * Size];
        size_t  m_p3[2 * Size];
        size_t  m_p4[2 * Size];
    };

    // Permutation tables.
    extern Permutations g_perms;

    // Quintic interpolation polynomial 6.t^5 - 15.t^4 + 10.t^3.
    template <typename T>
    inline T fade(const T t)
    {
        return t * t * t * (t * (t * T(6.0) - T(15.0)) + T(10.0));
    }

    // Convert the low 4 bits of the hash code into 12 gradient directions.
    template <typename T>
    inline T grad(const size_t hash, const T x, const T y, const T z)
    {
        const size_t h = hash & 15;
        const T u = h < 8 ? x : y;
        const T v = h < 4 ? y : h == 12 || h == 14 ? x : z;
        return ((h & 1) == 0 ? u : -u) +
               ((h & 2) == 0 ? v : -v);
    }

}   // namespace noise_impl

template <typename T>
T noise(const Vector<T, 1>& p)
{
    throw ExceptionNotImplemented();
    return T(0.0);
}

template <typename T>
T noise(const Vector<T, 2>& p)
{
    throw ExceptionNotImplemented();
    return T(0.0);
}

template <typename T>
T noise(const Vector<T, 3>& p)
{
    // Find the coordinates of the unit cube that contains the point.
    const int ix = truncate<int>(fast_floor(p.x));
    const int iy = truncate<int>(fast_floor(p.y));
    const int iz = truncate<int>(fast_floor(p.z));

    // Compute the fractional coordinates of the point inside the cube.
    const T fx = p.x - ix;
    const T fy = p.y - iy;
    const T fz = p.z - iz;

    // Compute the interpolation weights.
    const T u = noise_impl::fade(fx);
    const T v = noise_impl::fade(fy);
    const T w = noise_impl::fade(fz);

    // Hash together the coordinates of the 8 cube corners.
    const size_t hx = ix & (noise_impl::Permutations::Size - 1);
    const size_t hy = iy & (noise_impl::Permutations::Size - 1);
    const size_t hz = iz & (noise_impl::Permutations::Size - 1);
    const size_t a  = noise_impl::g_perms.m_p1[hx]     + hy;
    const size_t aa = noise_impl::g_perms.m_p1[a]      + hz;
    const size_t ab = noise_impl::g_perms.m_p1[a  + 1] + hz;
    const size_t b  = noise_impl::g_perms.m_p1[hx + 1] + hy;
    const size_t ba = noise_impl::g_perms.m_p1[b]      + hz;
    const size_t bb = noise_impl::g_perms.m_p1[b  + 1] + hz;
    const size_t h0 = noise_impl::g_perms.m_p1[aa];
    const size_t h1 = noise_impl::g_perms.m_p1[ba];
    const size_t h2 = noise_impl::g_perms.m_p1[ab];
    const size_t h3 = noise_impl::g_perms.m_p1[bb];
    const size_t h4 = noise_impl::g_perms.m_p1[aa + 1];
    const size_t h5 = noise_impl::g_perms.m_p1[ba + 1];
    const size_t h6 = noise_impl::g_perms.m_p1[ab + 1];
    const size_t h7 = noise_impl::g_perms.m_p1[bb + 1];

    // Blend together the gradients of the 8 cube corners.
    const T value =
        lerp(lerp(lerp(noise_impl::grad(h0, fx    , fy    , fz   ),
                       noise_impl::grad(h1, fx - 1, fy    , fz   ), u),
                  lerp(noise_impl::grad(h2, fx    , fy - 1, fz   ),
                       noise_impl::grad(h3, fx - 1, fy - 1, fz   ), u), v),
             lerp(lerp(noise_impl::grad(h4, fx    , fy    , fz - 1),
                       noise_impl::grad(h5, fx - 1, fy    , fz - 1), u),
                  lerp(noise_impl::grad(h6, fx    , fy - 1, fz - 1),
                       noise_impl::grad(h7, fx - 1, fy - 1, fz - 1), u), v), w);

    return clamp(value, T(-1.0), T(1.0));
}

template <typename T>
T noise(const Vector<T, 4>& p)
{
    throw ExceptionNotImplemented();
    return T(0.0);
}

template <typename T>
inline T noise(const T x)
{
    return noise(Vector<T, 1>(x));
}


//
// Fractional Brownian Motion functions implementation.
//

template <typename T, size_t N>
T fbm(
    Vector<T, N>    p,
    const size_t    octaves,
    const T         lacunarity,
    const T         gain)
{
    assert(octaves > 0);

    T amplitude = T(1.0);
    T amplitude_sum = T(0.0);
    T value = T(0.0);

    for (size_t i = 0; i < octaves; ++i)
    {
        value += noise(p) * amplitude;
        amplitude_sum += amplitude;
        amplitude *= gain;
        p *= lacunarity;
    }

    value /= amplitude_sum;

    return clamp(value, T(-1.0), T(1.0));
}

template <typename T>
inline T fbm(
    T               p,
    const size_t    octaves,
    const T         lacunarity,
    const T         gain)
{
    return fbm(
        Vector<T, 1>(p),
        octaves,
        lacunarity,
        gain);
}


//
// Turbulence functions implementation.
//

template <typename T, size_t N>
T turbulence(
    Vector<T, N>    p,
    const size_t    octaves,
    const T         lacunarity,
    const T         gain)
{
    assert(octaves > 0);

    T amplitude = T(1.0);
    T amplitude_sum = T(0.0);
    T value = T(0.0);

    for (size_t i = 0; i < octaves; ++i)
    {
        value += std::abs(noise(p)) * amplitude;
        amplitude_sum += amplitude;
        amplitude *= gain;
        p *= lacunarity;
    }

    value /= amplitude_sum;

    return saturate(value);
}

template <typename T>
inline T turbulence(
    T               p,
    const size_t    octaves,
    const T         lacunarity,
    const T         gain)
{
    return turbulence(
        Vector<T, 1>(p),
        octaves,
        lacunarity,
        gain);
}

}   // namespace foundation
