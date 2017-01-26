
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

// Taken from noises.h, slightly tweaked for OSL

/************************************************************************
 * noises.h - various noise-based patterns
 *
 * Author: Larry Gritz (gritzl@acm.org)
 *
 * Reference:
 *   _Advanced RenderMan: Creating CGI for Motion Picture_, 
 *   by Anthony A. Apodaca and Larry Gritz, Morgan Kaufmann, 1999.
 *
 * $Revision: 1.1 $    $Date: 2004/03/02 04:50:34 $
 *
 ************************************************************************/

#ifndef AS_NOISE_HELPERS_H
#define AS_NOISE_HELPERS_H

#define NOISE_CUBE_SIDE   32.0
#define NOISE_CUBE_SHIFT  5
#define NOISE_CUBE_MASK   31

#include "appleseed/fractal/as_noise_tables.h"
#include "appleseed/math/as_math_helpers.h"

#define filtered_noise(p, filter_width)                                 \
    (noise(p) * (1 - smoothstep(0.2, 0.75, filter_width)))

#define filtered_snoise(p, filter_width)                                \
    (snoise(p) * (1 - smoothstep(0.2, 0.75, filter_width)))

// Variable lacunarity noise.
#define vlnoise(p, scale)                                               \
    (snoise((vector) snoise(p) * scale + p))

#define filtered_vlnoise(p, scale)                                      \
    (filtered_snoise((vector) noise(p) *                                \
    (1 - smoothstep(0.2, 0.75, filter_width)) * scale + p,              \
    filter_width)
    
float noise_quadratic(float control_p[3], float t1)
{
    float tmp = control_p[0] - control_p[1];
    
    return ((tmp - control_p[1] + control_p[2]) * t1 - tmp - tmp) *
       t1 + control_p[0] + control_p[1];
}

float noise_derivative(float control_p[3], float t1)
{
    float tmp = control_p[0] - control_p[1];

    return (tmp - control_p[1] + control_p[2]) * t1 - tmp;
}

void noise_lookup(int y, int vx[3], output float px[3])
{
    float rng_table[65536] = { RNG_TABLE };

    px[0] = rng_table[vx[0] | y];
    px[1] = rng_table[vx[1] | y];
    px[2] = rng_table[vx[2] | y];
}

float tnoise2(float x, float y)
{
    float rng_table[65536] = { RNG_TABLE };

    float result, tmp, p1x, p2x, p3x, tx, p1y, p2y, p3y, ty;

    int x1, x2, x3, y1, y2, y3;

    float xx = (x > 1.0e9) ? 0 : x;
    float yy = (y > 1.0e9) ? 0 : y;

    int nx[3], ny[3];

    tx = (float) xx;
    tx = mod(tx, 1.0001) * NOISE_CUBE_SIDE;

    ty = (float) yy;
    ty = mod(ty, 1.0001) * NOISE_CUBE_SIDE; 

    nx[1] = (int) tx;
    ny[1] = (int) ty;

    tx -= (float) nx[1];
    ty -= (float) ny[1];  

    nx[0] = (nx[1] - 1) & NOISE_CUBE_MASK;
    nx[2] = (nx[1] + 1) & NOISE_CUBE_MASK;
    nx[1] = nx[1] & NOISE_CUBE_MASK;
    
    ny[0] = ((ny[1] - 1) & NOISE_CUBE_MASK) << NOISE_CUBE_SHIFT;
    ny[2] = ((ny[1] + 1) & NOISE_CUBE_MASK) << NOISE_CUBE_SHIFT;
    ny[1] = (ny[1] & NOISE_CUBE_MASK) << NOISE_CUBE_SHIFT;

    vector px, py;

    for (int i = 0; i < 3; ++i)
    {
        noise_lookup(ny[i], nx, px);
        py[i] = noise_quadratic(px, tx);
    }
    float out = noise_quadratic(py, ty);

    return out * 0.25;
}

#endif // AS_NOISE_HELPERS_H
