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

#ifndef AS_MATH_HELPERS_H
#define AS_MATH_HELPERS_H

#ifndef EPS
#define EPS 1.0e-6
#endif

float sqr(float x) { return x * x; }

// Perlin bias
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

// Schlick's bias function: http://dept-info.labri.fr/~schlick/DOC/gem2.ps.gz
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

#endif // !AS_MATH_HELPERS_H
