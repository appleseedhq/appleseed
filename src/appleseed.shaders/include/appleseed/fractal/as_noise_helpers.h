
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

#ifndef AS_NOISE_HELPERS_H
#define AS_NOISE_HELPERS_H

#define NOISE_CUBE_SIDE     32.0
#define NOISE_CUBE_SHIFT    5
#define NOISE_CUBE_MASK     31
#define NOISE_TABLE_SIZE    65536

#include "appleseed/fractal/as_noise_tables.h"
#include "appleseed/maya/as_maya_helpers.h"

float rng_table(int index)
{
    float table[NOISE_TABLE_SIZE] = { RNG_TABLE };
    return table[index];
}

float filtered_noise(
    point surface_point,
    float current_time,
    float filter_width)
{
    return (1.0 - smoothstep(0.2, 0.75, filter_width)) *
        noise("uperlin", surface_point, current_time);
}

float filtered_snoise(
    point surface_point,
    float current_time,
    float filter_width)
{
    return (1.0 - smoothstep(0.2, 0.75, filter_width)) *
        noise("perlin", surface_point, current_time);
}

float noise_quadratic(vector control_point, float x)
{
    float tmp = control_point[0] - control_point[1];

    return ((tmp - control_point[1] + control_point[2]) *
        x - tmp - tmp) * x + control_point[0] + control_point[1];
}

float noise_derivative(vector control_point, float x)
{
    float tmp = control_point[0] - control_point[1];

    return (tmp - control_point[1] + control_point[2]) * x - tmp;
}

vector noise_lookup(int index, int xyz[3])
{
    return vector(
        rng_table(xyz[0] | index),
        rng_table(xyz[1] | index),
        rng_table(xyz[2] | index));
}

float value_noise_2d(float x, float y)
{
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

    if (ndx < 0)
    {
        ndx = -ndx;
    }
    if (ndx >= NOISE_TABLE_SIZE)
    {
        ndx = ndx % NOISE_TABLE_SIZE;
    }
    return rng_table(ndx);
}

#endif // !AS_NOISE_HELPERS_H
