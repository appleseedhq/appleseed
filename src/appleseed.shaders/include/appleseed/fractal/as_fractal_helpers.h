
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Luis Barrancos, The appleseedhq Organization
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

// Ref: Texturing & Modelling: A Procedural Approach, 3rd edition,
// David Ebert, F.Kenton Musgrave, Darwyn Peachey, Ken Perlin, Steve Worley.
// ISBN: 9781558608481 

#ifndef AS_FRACTAL_HELPERS_H
#define AS_FRACTAL_HELPERS_H

#include "appleseed/fractal/as_noise_helpers.h"
#include "appleseed/math/as_math_helpers.h"

float fBm(
    point surface_point,
    float filter_width,
    float amplitude,
    int octaves,
    float lacunarity,
    float gain)
{
    point pp = surface_point;
    float amp = amplitude, fw = filter_width, sum = 0.0;

    for (int i = 0; i < octaves; ++i)
    {
        sum += amp * filtered_snoise(pp, fw);
        amp *= gain;
        pp *= lacunarity;
        fw *= lacunarity;
    }
    return sum;
}

#endif // AS_PATTERN_HELPERS_H
