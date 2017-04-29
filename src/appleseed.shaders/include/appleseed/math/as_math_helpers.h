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
//      Lehmer linear congruential generator (Park-Miller)
//
//      Random Number Generators: Good Ones Are Hard To Find,
//      Stephen K.Park and Keith W.Miller
//
//      http://www.firstpr.com.au/dsp/rand31/p1192-park.pdf
//      https://en.wikipedia.org/wiki/Lehmer_random_number_generator
//

int rand_int(int seed)
{
    int a = 16807;
    int q = 127773;
    int r = 2836;
    int m = 2147483647;

    int hi = seed / q;
    int lo = seed % q;
    int x = a * lo - r * hi;

    return x > 0 ? x : x + m;
}

int rand_float(int seed, output float result)
{
    int x = rand_int(seed);
    result = x / 2147483647;

    return x;
}

#endif // !AS_MATH_HELPERS_H
