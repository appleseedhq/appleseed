
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

// Filtered noise macros from noises.h, slightly tweaked for OSL

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

#define NOISE_CUBE_SIDE     32.0
#define NOISE_CUBE_SHIFT    5
#define NOISE_CUBE_MASK     31
#define NOISE_TABLE_SIZE    65536

#include "appleseed/fractal/as_noise_tables.h"
#include "appleseed/math/as_math_helpers.h"
#include "appleseed/maya/as_maya_helpers.h"

#define filtered_noise(p, itime, filter_width)                          \
    (noise(p,itime) * (1 - smoothstep(0.2, 0.75, filter_width)))

#define filtered_snoise(p, itime, filter_width)                         \
    (snoise(p, itime) * (1 - smoothstep(0.2, 0.75, filter_width)))

float noise_quadratic(vector control_p, float x)
{
    float tmp = control_p[0] - control_p[1];

    return ((tmp - control_p[1] + control_p[2]) * x - tmp - tmp) *
       x + control_p[0] + control_p[1];
}

float noise_derivative(vector control_p, float x)
{
    float tmp = control_p[0] - control_p[1];

    return (tmp - control_p[1] + control_p[2]) * x - tmp;
}

vector noise_lookup(int y, int vx[3])
{
    float rng_table[NOISE_TABLE_SIZE] = { RNG_TABLE };

    return vector(
        rng_table[vx[0] | y],
        rng_table[vx[1] | y],
        rng_table[vx[2] | y]);
}                  

float value_noise_2d(float x, float y)
{
    float rng_table[NOISE_TABLE_SIZE] = { RNG_TABLE };

    float xx = (x > 1.0e9) ? 0 : x;
    float yy = (y > 1.0e9) ? 0 : y;

    int nx[3], ny[3];

    float tx = xx;
    tx = mod(tx, UVWRAP) * NOISE_CUBE_SIDE;

    float ty = yy;
    ty = mod(ty, UVWRAP) * NOISE_CUBE_SIDE; 

    nx[1] = (int) tx;
    ny[1] = (int) ty;

    tx -= nx[1];
    ty -= ny[1];  

    nx[0] = (nx[1] - 1) & NOISE_CUBE_MASK;
    nx[2] = (nx[1] + 1) & NOISE_CUBE_MASK;
    nx[1] = nx[1] & NOISE_CUBE_MASK;
    
    ny[0] = ((ny[1] - 1) & NOISE_CUBE_MASK) << NOISE_CUBE_SHIFT;
    ny[2] = ((ny[1] + 1) & NOISE_CUBE_MASK) << NOISE_CUBE_SHIFT;
    ny[1] = (ny[1] & NOISE_CUBE_MASK) << NOISE_CUBE_SHIFT;

    vector py;

    for (int i = 0; i < 3; ++i)
    {
        vector px = noise_lookup(ny[i], nx);
        py[i] = noise_quadratic(px, tx);
    }

    return 0.25 * noise_quadratic(py, ty);
}

vector value_noise_2d(float x, float y)
{
    float rng_table[NOISE_TABLE_SIZE] = { RNG_TABLE };

    float xx = (x > 1.0e9) ? 0 : x;
    float yy = (y > 1.0e9) ? 0 : y;

    int nx[3], ny[3];

    float tx = xx;
    tx = mod(tx, UVWRAP) * NOISE_CUBE_SIDE;

    float ty = yy;
    ty = mod(ty, UVWRAP) * NOISE_CUBE_SIDE;

    nx[1] = (int) tx;
    ny[1] = (int) ty;

    tx -= nx[1];
    ty -= ny[1];

    nx[0] = (nx[1] - 1) & NOISE_CUBE_MASK;
    nx[2] = (nx[1] + 1) & NOISE_CUBE_MASK;
    nx[1] = nx[1] & NOISE_CUBE_MASK;
    
    ny[0] = ((ny[1] - 1) & NOISE_CUBE_MASK) << NOISE_CUBE_SHIFT;
    ny[2] = ((ny[1] + 1) & NOISE_CUBE_MASK) << NOISE_CUBE_SHIFT;
    ny[1] = (ny[1] & NOISE_CUBE_MASK) << NOISE_CUBE_SHIFT;

    vector py, dy;

    for (int i = 0; i < 3; ++i)
    {
        vector px = noise_lookup(ny[i], nx);
        py[i] = noise_quadratic(px, tx);
        dy[i] = noise_derivative(px, tx);
    }

    float out_x = noise_quadratic(dy, ty);
    float out_y = noise_derivative(py, ty);
    float out_z = 0.25 * ((ty * (py[0] - py[1] - py[1] + py[2]) +
                4.0 * (-py[0] + py[1])) * ty + py[0] + py[1]);

    return vector(out_x, out_y, out_z);
}

float random_noise(int index)
{
    int ndx = index;

    float rng_table[NOISE_TABLE_SIZE] = { RNG_TABLE };

    if (ndx < 0)
    {
        ndx = -ndx;
    }
    if (ndx >= NOISE_TABLE_SIZE)
    {
        ndx = ndx % NOISE_TABLE_SIZE;
    }
    return rng_table[ndx];
}

#endif // AS_NOISE_HELPERS_H
