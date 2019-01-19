
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
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cmath>

namespace renderer
{

// Compute the spherical coordinates of a given direction.
template <typename T>
void unit_vector_to_angles(
    const foundation::Vector<T, 3>& v,              // unit length
    T&                              theta,          // in [0, Pi]
    T&                              phi);           // in [-Pi, Pi]

// Shift spherical coordinates by given amounts.
template <typename T>
void shift_angles(
    T&                              theta,          // in [0, Pi]
    T&                              phi,            // in [-Pi, Pi]
    const T                         theta_shift,    // arbitrary, in radians
    const T                         phi_shift);     // arbitrary, in radians

// Convert a given direction from spherical coordinates to [0,1]^2.
template <typename T>
void angles_to_unit_square(
    const T                         theta,          // in [0, Pi]
    const T                         phi,            // in [-Pi, Pi]
    T&                              u,              // in [0, 1]
    T&                              v);             // in [0, 1]

// Convert a given direction from [0,1]^2 to spherical coordinates.
template <typename T>
void unit_square_to_angles(
    const T                         u,              // in [0, 1]
    const T                         v,              // in [0, 1]
    T&                              theta,          // in [0, Pi]
    T&                              phi);           // in [-Pi, Pi]


//
// Implementation.
//

template <typename T>
inline void unit_vector_to_angles(
    const foundation::Vector<T, 3>& v,
    T&                              theta,
    T&                              phi)
{
    assert(foundation::is_normalized(v));

    theta = static_cast<T>(std::acos(v[1]));
    phi = static_cast<T>(std::atan2(v[2], v[0]));
}

template <typename T>
inline void shift_angles(
    T&                              theta,
    T&                              phi,
    const T                         theta_shift,
    const T                         phi_shift)
{
    using namespace foundation;

    theta += theta_shift;
    phi += phi_shift;

    theta = std::fmod(theta, Pi<T>());

    if (theta < T(0.0))
        theta += Pi<T>();

    phi += Pi<T>();

    phi = std::fmod(phi, TwoPi<T>());

    if (phi < T(0.0))
        phi += TwoPi<T>();

    phi -= Pi<T>();
}

template <typename T>
inline void angles_to_unit_square(
    const T                         theta,
    const T                         phi,
    T&                              u,
    T&                              v)
{
    using namespace foundation;

    assert(theta >= T(0.0));
    assert(theta <= Pi<T>());
    assert(phi >= -Pi<T>());
    assert(phi <= Pi<T>());

    u = RcpTwoPi<T>() * (phi + Pi<T>());
    v = RcpPi<T>() * theta;
}

template <typename T>
inline void unit_square_to_angles(
    const T                         u,
    const T                         v,
    T&                              theta,
    T&                              phi)
{
    using namespace foundation;

    assert(u >= T(0.0));
    assert(u <= T(1.0));
    assert(v >= T(0.0));
    assert(v <= T(1.0));

    theta = Pi<T>() * v;
    phi = Pi<T>() * (T(2.0) * u - T(1.0));
}

}   // namespace renderer
