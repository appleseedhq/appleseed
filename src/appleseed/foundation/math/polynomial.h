
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Fedor Matantsev, The appleseedhq Organization
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
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation
{

// Evaluates a polynomial represented by a vector of coefficients (in monomial basis).
// Yields p0 + p * (p1 + p * (p2 + p * (...+ p * p{N-1})...)).
template <typename T, typename P, size_t N>
T evaluate_polynomial(
    const Vector<T, N>&     coefficients,
    const P                 param);

// Evaluates the derivative of a polynomial represented by a vector of coefficients (in monomial basis).
// Yields p1 + p * (2 * p2 + p * (...+ N * p * p{N-1}))...)).
template <typename T, typename P, size_t N>
T evaluate_polynomial_derivative(
    const Vector<T, N>&     coefficients,
    const P                 param);


//
// Implementation.
//

template <typename T, typename P, size_t N>
T evaluate_polynomial(
    const Vector<T, N>&     coefficients,
    const P                 param)
{
    assert(N > 0);

    T value = coefficients[N - 1];

    for (size_t i = 1; i < N; ++i)
        value = param * value + coefficients[N - 1 - i];

    return value;
}

template <typename T, typename P, size_t N>
T evaluate_polynomial_derivative(
    const Vector<T, N>&     coefficients,
    const P                 param)
{
    assert(N >= 2);

    // Derivative of a constant is zero.
    if (N == 1)
        return T(0);

    // Here we suppose N >= 2.
    T value = T(N - 1) * coefficients[N - 1];

    for (size_t i = 1; i < N - 1; ++i)
    {
        const size_t idx = N - 1 - i;
        value = param * value + static_cast<P>(idx) * coefficients[idx];
    }

    return value;
}

}   // namespace foundation
