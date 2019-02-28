
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

// Standard headers.
#include <cmath>
#include <cstddef>

namespace foundation
{

// Find one root of the continuous function f over the interval [a, b] using
// the bisection method.
template <typename T, typename F>
bool find_root_bisection(
    const F&        f,
    T               a,
    T               b,
    const T         eps,
    const size_t    max_iterations,
    T&              root);

// Find one root of the continuous function f over the interval [a, b] using
// Newton's method.
template <typename T, typename F, typename D>
bool find_root_newton(
    const F&        f,
    const D&        d,
    const T         a,
    const T         b,
    const T         eps,
    const size_t    max_iterations,
    T&              root);

// Find multiple roots of the continuous function f over the interval [a, b]
// by recursively splitting the input interval into subintervals no longer
// than max_length and using the bisection method on those subintervals.
template <typename T, typename F, typename RootHandler>
void find_multiple_roots_bisection(
    const F&        f,
    const T         a,
    const T         b,
    const T         max_length,
    const T         eps,
    const size_t    max_iterations,
    RootHandler&    root_handler);

// Find multiple roots of the continuous function f over the interval [a, b]
// by recursively splitting the input interval into subintervals no longer
// than max_length and using Newton's method on those subintervals.
template <typename T, typename F, typename D, typename RootHandler>
void find_multiple_roots_newton(
    const F&        f,
    const D&        d,
    const T         a,
    const T         b,
    const T         max_length,
    const T         eps,
    const size_t    max_iterations,
    RootHandler&    root_handler);


//
// Implementation.
//

template <typename T, typename F>
bool find_root_bisection(
    const F&        f,
    T               a,
    T               b,
    const T         eps,
    const size_t    max_iterations,
    T&              root)
{
    T fa = f(a);
    T fb = f(b);

    if (fa * fb > T(0.0))
        return false;

    for (size_t i = 0; i < max_iterations; ++i)
    {
        const T m = (a + b) * T(0.5);

        if (std::abs(b - a) <= eps)
        {
            root = m;
            return true;
        }

        const T fm = f(m);

        if (fa * fm <= T(0.0))
        {
            b = m;
            fb = fm;
        }
        else
        {
            a = m;
            fa = fm;
        }
    }

    return false;
}

template <typename T, typename F, typename D>
bool find_root_newton(
    const F&        f,
    const D&        d,
    const T         a,
    const T         b,
    const T         eps,
    const size_t    max_iterations,
    T&              root)
{
    assert(a <= b);
    T fa = f(a);
    T fb = f(b);

    if (fa * fb > T(0.0))
        return false;

    root = (a + b) * T(0.5);

    for (size_t i = 0; i < max_iterations; ++i)
    {
        const T delta = f(root) / d(root);
        root -= delta;

        if ((root < a) || (root > b))
        {
            root = clamp(root, a, b);
        }
        else if (std::abs(delta) <= eps)
            return true;
    }

    return false;
}

template <typename T, typename F, typename RootHandler>
void find_multiple_roots_bisection(
    const F&        f,
    const T         a,
    const T         b,
    const T         max_length,
    const T         eps,
    const size_t    max_iterations,
    RootHandler&    root_handler)
{
    if (std::abs(b - a) > max_length)
    {
        const T m = (a + b) * T(0.5);
        find_multiple_roots_bisection(f, a, m, max_length, eps, max_iterations, root_handler);
        find_multiple_roots_bisection(f, m, b, max_length, eps, max_iterations, root_handler);
    }
    else
    {
        T root;
        if (find_root_bisection(f, a, b, eps, root))
            root_handler(root);
    }
}

template <typename T, typename F, typename D, typename RootHandler>
void find_multiple_roots_newton(
    const F&        f,
    const D&        d,
    const T         a,
    const T         b,
    const T         max_length,
    const T         eps,
    const size_t    max_iterations,
    RootHandler&    root_handler)
{
    assert(a <= b);
    if (b - a > max_length)
    {
        const T m = (a + b) * T(0.5);
        find_multiple_roots_newton(f, d, a, m, max_length, eps, max_iterations, root_handler);
        find_multiple_roots_newton(f, d, m, b, max_length, eps, max_iterations, root_handler);
    }
    else
    {
        T root;
        if (find_root_newton(f, d, a, b, eps, max_iterations, root))
            root_handler(root);
    }
}

}   // namespace foundation
