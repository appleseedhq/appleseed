
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
#include "foundation/math/minmax.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>

namespace foundation
{

//
// Multiple Importance Sampling (MIS) heuristics.
//
// Reference:
//
//   Robust Monte Carlo Methods For Light Transport Simulation
//   http://graphics.stanford.edu/papers/veach_thesis/thesis.pdf
//

// Balance heuristic.
template <typename T> T mis_balance(const T q1, const T q2);
template <typename T> T mis_balance(const T q1, const T q2, const T q3);

// Power heuristic. beta is >= 0.
template <typename T> T mis_power(const T q1, const T q2, const T beta);
template <typename T> T mis_power(const T q1, const T q2, const T q3, const T beta);

// Power heuristic with beta = 2.
template <typename T> T mis_power2(const T q1, const T q2);
template <typename T> T mis_power2(const T q1, const T q2, const T q3);

// Cutoff heuristic. The cutoff threshold alpha is in [0,1].
template <typename T> T mis_cutoff(const T q1, const T q2, const T alpha);
template <typename T> T mis_cutoff(const T q1, const T q2, const T q3, const T alpha);

// Maximum heuristic.
template <typename T> T mis_maximum(const T q1, const T q2);
template <typename T> T mis_maximum(const T q1, const T q2, const T q3);

// Apply a MIS heuristic chosen at runtime.
enum MISHeuristic { MISNone, MISBalance, MISPower2, MISMaximum };
template <typename T> T mis(const MISHeuristic heuristic, const T q1, const T q2);


//
// Implementation.
//

template <typename T>
inline T mis_balance(const T q1, const T q2)
{
    assert(q1 >= T(0.0));
    assert(q2 >= T(0.0));
    assert(q1 + q2 > T(0.0));

    const T r2 = q2 / q1;

    return T(1.0) / (T(1.0) + r2);
}

template <typename T>
inline T mis_balance(const T q1, const T q2, const T q3)
{
    assert(q1 >= T(0.0));
    assert(q2 >= T(0.0));
    assert(q3 >= T(0.0));
    assert(q1 + q2 + q3 > T(0.0));

    const T r2 = q2 / q1;
    const T r3 = q3 / q1;

    return T(1.0) / (T(1.0) + r2 + r3);
}

template <typename T>
inline T mis_power(const T q1, const T q2, const T beta)
{
    assert(q1 >= T(0.0));
    assert(q2 >= T(0.0));
    assert(q1 + q2 > T(0.0));

    assert(beta >= T(0.0));

    const T r2 = q2 / q1;
    const T r2_pow = std::pow(r2, beta);

    return T(1.0) / (T(1.0) + r2_pow);
}

template <typename T>
inline T mis_power(const T q1, const T q2, const T q3, const T beta)
{
    assert(q1 >= T(0.0));
    assert(q2 >= T(0.0));
    assert(q3 >= T(0.0));
    assert(q1 + q2 + q3 > T(0.0));

    assert(beta >= T(0.0));

    const T r2 = q2 / q1;
    const T r3 = q3 / q1;
    const T r2_pow = std::pow(r2, beta);
    const T r3_pow = std::pow(r3, beta);

    return T(1.0) / (T(1.0) + r2_pow + r3_pow);
}

template <typename T>
inline T mis_power2(const T q1, const T q2)
{
    assert(q1 >= T(0.0));
    assert(q2 >= T(0.0));
    assert(q1 + q2 > T(0.0));

    const T r2 = q2 / q1;
    const T r2_pow = r2 * r2;

    return T(1.0) / (T(1.0) + r2_pow);
}

template <typename T>
inline T mis_power2(const T q1, const T q2, const T q3)
{
    assert(q1 >= T(0.0));
    assert(q2 >= T(0.0));
    assert(q3 >= T(0.0));
    assert(q1 + q2 + q3 > T(0.0));

    const T r2 = q2 / q1;
    const T r3 = q3 / q1;
    const T r2_pow = r2 * r2;
    const T r3_pow = r3 * r3;

    return T(1.0) / (T(1.0) + r2_pow + r3_pow);
}

template <typename T>
inline T mis_cutoff(const T q1, const T q2, const T alpha)
{
    assert(q1 >= T(0.0));
    assert(q2 >= T(0.0));
    assert(q1 + q2 > T(0.0));

    assert(alpha >= T(0.0));
    assert(alpha <= T(1.0));

    const T cutoff = std::max(q1, q2) * alpha;

    return
        q1 < cutoff ? T(0.0) :
        q2 < cutoff ? T(1.0) :
        T(1.0) / (T(1.0) + q2 / q1);
}

template <typename T>
inline T mis_cutoff(const T q1, const T q2, const T q3, const T alpha)
{
    assert(q1 >= T(0.0));
    assert(q2 >= T(0.0));
    assert(q3 >= T(0.0));
    assert(q1 + q2 + q3 > T(0.0));

    assert(alpha >= T(0.0));
    assert(alpha <= T(1.0));

    const T cutoff = std::max(q1, q2, q3) * alpha;

    if (q1 < cutoff)
         return T(0.0);

    T den = T(1.0);
    if (q2 >= cutoff) den += q2 / q1;
    if (q3 >= cutoff) den += q3 / q1;

    return T(1.0) / den;
}

template <typename T>
inline T mis_maximum(const T q1, const T q2)
{
    assert(q1 >= T(0.0));
    assert(q2 >= T(0.0));
    assert(q1 + q2 > T(0.0));

    return q1 >= q2 ? T(1.0) : T(0.0);
}

template <typename T>
inline T mis_maximum(const T q1, const T q2, const T q3)
{
    assert(q1 >= T(0.0));
    assert(q2 >= T(0.0));
    assert(q3 >= T(0.0));
    assert(q1 + q2 + q3 > T(0.0));

    return q1 >= q2 && q1 >= q3 ? T(1.0) : T(0.0);
}

template <typename T>
inline T mis(const MISHeuristic heuristic, const T q1, const T q2)
{
    switch (heuristic)
    {
      case MISNone:     return T(1.0);
      case MISBalance:  return mis_balance(q1, q2);
      case MISPower2:   return mis_power2(q1, q2);
      case MISMaximum:  return mis_maximum(q1, q2);
    }

    APPLESEED_UNREACHABLE;
    return T(-1.0);
}

}   // namespace foundation
