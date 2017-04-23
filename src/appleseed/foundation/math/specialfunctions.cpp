
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
#include "specialfunctions.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"

// Standard headers.
#include <cmath>

namespace foundation
{

//
// Reference:
//
//   Handbook of Mathematical Functions.
//   Abramowitz and Stegun.
//   http://people.math.sfu.ca/~cbm/aands/toc.htm
//
// Copied from from pbrt-v3.
//

float erf(const float x)
{
    // Constants.
    const float a1 = 0.254829592f;
    const float a2 = -0.284496736f;
    const float a3 = 1.421413741f;
    const float a4 = -1.453152027f;
    const float a5 = 1.061405429f;
    const float p  = 0.3275911f;

    // A&S formula 7.1.26.
    const float abs_x = std::abs(x);
    const float t = 1.0f / (1.0f + p * abs_x);
    const float y = 1.0f - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * std::exp(-square(abs_x * abs_x));
    return x < 0.0f ? -y : y;
}

//
// Reference:
//
//   Approximating the erfinv function, Mike Giles.
//   https://people.maths.ox.ac.uk/gilesm/files/gems_erfinv.pdf
//

float erf_inv(const float x)
{
    const float y = clamp(x, -0.99999f, 0.99999f);
    float w = -std::log((1 - y) * (1 + y));
    float p;

    if (w < 5.0f)
    {
        w = w - 2.5f;
        p = 2.81022636e-08f;
        p = 3.43273939e-07f + p * w;
        p = -3.5233877e-06f + p * w;
        p = -4.39150654e-06f + p * w;
        p = 0.00021858087f + p * w;
        p = -0.00125372503f + p * w;
        p = -0.00417768164f + p * w;
        p = 0.246640727f + p * w;
        p = 1.50140941f + p * w;
    }
    else
    {
        w = std::sqrt(w) - 3.0f;
        p = -0.000200214257f;
        p = 0.000100950558f + p * w;
        p = 0.00134934322f + p * w;
        p = -0.00367342844f + p * w;
        p = 0.00573950773f + p * w;
        p = -0.0076224613f + p * w;
        p = 0.00943887047f + p * w;
        p = 1.00167406f + p * w;
        p = 2.83297682f + p * w;
    }

    return p * y;
}

float gamma(const float x)
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

float gamma_fraction(const float num, const float denom)
{
    const float ab1 = gamma(num + 5.0f);
    const float ab2 = gamma(denom + 5.0f);

    const float ac1 = 1.0f / (num * (num + 1.0f) * (num + 2.0f) * (num + 3.0f) * (num + 4.0f));
    const float ac2 = 1.0f / (denom * (denom + 1.0f) * (denom + 2.0f) * (denom + 3.0f) * (denom + 4.0f));

    return std::exp(ab1 - ab2) * (ac1 / ac2);
}

}       // namespace foundation
