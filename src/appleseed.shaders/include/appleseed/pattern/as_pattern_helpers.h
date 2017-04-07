
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Luis Barrancos, The appleseedhq Organization
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

// Taken from patterns.h, adjusted to better suite appleseed (and OSL).

/************************************************************************
 * patterns.h - Some handy functions for various patterns.  Wherever
 *              possible, antialiased versions will also be given.
 *
 * Author: Larry Gritz (gritzl@acm.org)
 *
 * $Revision: #1 $    $Date: 2009/03/10 $
 *
 ************************************************************************/ 

#ifndef AS_PATTERN_HELPERS_H
#define AS_PATTERN_HELPERS_H

#include "appleseed/math/as_math_helpers.h"

// Ref: Towards Automatic Band-Limited Procedural Shaders, from
// https://www.cs.virginia.edu/~connelly/publications/2015_bandlimit.pdf 

float filtered_abs(float x, float dx)
{
    float A = x / dx * M_SQRT2;
    float B = dx * sqrt(M_2_PI);
    float C = sqr(x) / 2 * sqr(dx);

    return x * erf(A) + B * exp(-C);
}

float filtered_clamp(float x, float dx)
{
    float A = x * erf(x / dx * M_SQRT2);
    float B = (x - 1) * erf((x - 1) / dx * M_SQRT2);

    float C = exp(-sqr(x) / 2 * sqr(dx));
    float D = exp(-sqr(x - 1) / 2 * sqr(dx));

    return 0.5 * (A - B + dx * sqrt(M_2_PI) * (C - D) + 1);
}

float filtered_step(float a, float x, float dx)
{
    return 0.5 * (1 + erf((x - a) / dx * M_SQRT2));
}

float filtered_smoothstep(
    float edge0,
    float edge1,
    float x,
    float dx)
{
    float integral(float x)
    {
        return -0.5 * sqr(x) * (sqr(x) + x);
    }

    float edgediff = edge1 - edge0;
    float x0 = (x - edge0) / edgediff;
    float fw = dx / edgediff;

    x0 -= 0.5 * fw;

    float x1 = x0 + fw;
    float out = 0;

    if (x0 < 1 && x1 > 0)
    {
        out += integral(min(1, x1)) - integral(max(0, x0));
    }
    if (x1 > 1)
    {
        out += x1 - max(1, x0);
    }
    return out / fw;
}

float filtered_pulse(
    float edge0,
    float edge1,
    float x,
    float dx)
{
    float x0 = x - dx * 0.5;
    float x1 = x0 + dx;

    return max(0, (min(x1, edge1) - max(x0, edge0)) / dx);
}

float filtered_smoothpulse(
    float edge0,
    float edge1,
    float edge2,
    float edge3,
    float x,
    float dx)
{
    return
        filtered_smoothstep(edge0, edge1, x, dx) -
        filtered_smoothstep(edge2, edge3, x, dx);
}

float filtered_pulsetrain(
    float edge,
    float period,
    float x,
    float dx)
{
    float integral(float x, float nedge)
    {
        return ((1 - nedge) * floor(x) + max(0, x - floor(x) - nedge));
    }

    float invPeriod = 1 / period;
    float w = dx * invPeriod;
    float x0 = x * invPeriod - 0.5 * w;
    float x1 = x0 + w;
    float nedge = edge * invPeriod;

    if (x0 != x1)
    {
        return (integral(x1, nedge) - integral(x0, nedge)) / w;
    }
    else
    {
        return (x0 - floor(x0) < nedge) ? 0 : 1;
    }
}

#endif // !AS_PATTERN_HELPERS_H
