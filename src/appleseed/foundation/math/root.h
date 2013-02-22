
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_MATH_ROOT_H
#define APPLESEED_FOUNDATION_MATH_ROOT_H

// Standard headers.
#include <cassert>
#include <cmath>
#include <functional>

namespace foundation
{

// Find one root of the continuous function f over the interval [a, b] using
// the bisection method.
template <typename T, typename F>
bool find_root_bisection(
    const F&    f,
    T           a,
    T           b,
    const T     eps,
    T&          root);

// Find one root of the continuous function f over the interval [a, b] using
// Newton's method.
template <typename T, typename F, typename D>
bool find_root_newton(
    const F&    f,
    const D&    d,
    const T     a,
    const T     b,
    const T     eps,
    T&          root);

// Find multiple roots of the continuous function f over the interval [a, b]
// by recursively splitting the input interval into subintervals no longer
// than max_length and using the bisection method on those subintervals.
template <typename T, typename F>
void find_multiple_roots_bisection(
    const F&                            f,
    const T                             a,
    const T                             b,
    const T                             max_length,
    const T                             eps,
    const std::function<void(double)>&  root_handler);

// Find multiple roots of the continuous function f over the interval [a, b]
// by recursively splitting the input interval into subintervals no longer
// than max_length and using Newton's method on those subintervals.
template <typename T, typename F, typename D>
void find_multiple_roots_newton(
    const F&                            f,
    const D&                            d,
    const T                             a,
    const T                             b,
    const T                             max_length,
    const T                             eps,
    const std::function<void(double)>&  root_handler);


//
// Implementation.
//

template <typename T, typename F>
bool find_root_bisection(
    const F&    f,
    T           a,
    T           b,
    const T     eps,
    T&          root)
{
    T fa = f(a);
    T fb = f(b);

    if (fa * fb > T(0.0))
        return false;

    while (std::abs(b - a) > eps)
    {
        const T m = (a + b) / T(2.0);
        const T fm = f(m);

        if (fa * fm <= T(0.0))
        {
            b = m;
            fb = fm;
        }
        else
        {
            assert(fm * fb <= T(0.0));
            a = m;
            fa = fm;
        }
    }

    root = (a + b) / T(2.0);
    return true;
}

template <typename T, typename F, typename D>
bool find_root_newton(
    const F&    f,
    const D&    d,
    const T     a,
    const T     b,
    const T     eps,
    T&          root)
{
    T fa = f(a);
    T fb = f(b);

    if (fa * fb > T(0.0))
        return false;

    root = (a + b) / T(2.0);

    while (true)
    {
        const T delta = f(root) / d(root);
        root -= delta;

        if (std::abs(delta) <= eps)
            return true;
    }
}

template <typename T, typename F>
void find_multiple_roots_bisection(
    const F&                            f,
    const T                             a,
    const T                             b,
    const T                             max_length,
    const T                             eps,
    const std::function<void(double)>&  root_handler)
{
    if (std::abs(b - a) > max_length)
    {
        const T m = (a + b) / T(2.0);
        find_multiple_roots_bisection(f, a, m, max_length, eps, root_handler);
        find_multiple_roots_bisection(f, m, b, max_length, eps, root_handler);
    }
    else
    {
        T root;
        if (find_root_bisection(f, a, b, eps, root))
            root_handler(root);
    }
}

template <typename T, typename F, typename D>
void find_multiple_roots_newton(
    const F&                            f,
    const D&                            d,
    const T                             a,
    const T                             b,
    const T                             max_length,
    const T                             eps,
    const std::function<void(double)>&  root_handler)
{
    if (std::abs(b - a) > max_length)
    {
        const T m = (a + b) / T(2.0);
        find_multiple_roots_newton(f, d, a, m, max_length, eps, root_handler);
        find_multiple_roots_newton(f, d, m, b, max_length, eps, root_handler);
    }
    else
    {
        T root;
        if (find_root_newton(f, d, a, b, eps, root))
            root_handler(root);
    }
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_ROOT_H
