//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Luis Barrancos, The appleseedhq Organization
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

#ifndef EPS
#define EPS 1.0e-6
#endif

float sqr(float x) { return x * x; }
vector sqr(vector x) { return x * x; }


//
// Reference:
//
//      Hypertexture
//      Ken Perlin, Eric M.Hoffert
//
//      http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.438.4926&rep=rep1&type=pdf
//

float bias(float value, float b)
{
    return pow(value, -log2(b));
}

float gain(float value, float g)
{
    return (value < 0.5)
        ? bias(value * 2, 1 - g) * 0.5
        : 1 - bias(2 - (value * 2), 1 - g) * 0.5;
}


//
// Reference:
//
//      Fast Alternatives To Perlin Bias And Gain Functions
//
//      http://dept-info.labri.u-bordeaux.fr/~schlick/DOC/gem2.ps.gz
//

float fast_bias(float value, float b)
{
    return value / ((1 / b - 2) * (1 - value) + 1);
}

float fast_gain(float value, float g)
{
    return (value < 0.5)
        ? fast_bias(value * 2, g) * 0.5
        : fast_bias(value * 2 - 1, 1 - g) * 0.5 + 0.5;
}


//
// Reference:
//
//  https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
//

int remainder(int x, int y)
{
    int rem = x % y;
    if ((rem > 0 && y < 0) || (rem < 0 && y > 0)) rem += y;
    return rem;
}

float fract(float x)
{
    return x - floor(x);
}
