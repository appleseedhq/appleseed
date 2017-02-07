
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
 
#ifndef AS_MAYA_FRACTAL_HELPERS_H
#define AS_MAYA_FRACTAL_HELPERS_H

#include "appleseed/fractal/as_noise_helpers.h"
#include "appleseed/math/as_math_helpers.h"

void implode_2d(
    float implode,
    float implode_center[2],
    output float x,
    output float y)
{
    if (implode > EPS || implode < -EPS)
    {
        x -= implode_center[0];
        y -= implode_center[1];

        float dist = hypot(x, y);

        if (dist > EPS)
        {
            float factor = pow(dist, 1 - implode) / dist;
            x *= factor;
            y *= factor;
        }
        x += implode_center[0];
        y += implode_center[1];
    }
}

void implode_3d(
    float implode,
    float implode_center[3],
    output float x,
    output float y,
    output float z)
{
    if (implode > EPS || implode < -EPS)
    {
        x -= implode_center[0];
        y -= implode_center[1];
        z -= implode_center[2];

        float dist = hypot(x, y, z);

        if (dist > EPS)
        {
            float factor = pow(dist, 1 - implode) / dist;
            x *= factor;
            y *= factor;
            z *= factor;
        }
        x += implode_center[0];
        y += implode_center[1];
        z += implode_center[2];
    }
}

vector implode_2d(
    float implode,
    float implode_center[2],
    vector Vin)
{
    vector Vout = vector(Vin[0], Vin[1], 0.0);
    implode_2d(implode, implode_center, Vout[0], Vout[1]);
    return Vout;
}

point implode_2d(
    float implode,
    float implode_center[2],
    point Pin)
{
    point Pout = point(Pin[0], Pin[1], 0.0);
    implode_2d(implode, implode_center, Pout[0], Pout[1]);
    return Pout;
}

vector implode_3d(
    float implode,
    float implode_center[3],
    vector Vin)
{
    vector Vout = Vin;
    implode_3d(implode, implode_center, Vout[0], Vout[1], Vout[2]);
    return Vout;
}

point implode_3d(
    float implode,
    float implode_center[3],
    point Vin)
{
    point Pout = Vin;
    implode_3d(implode, implode_center, Pout[0], Pout[1], Pout[2]);
    return Pout;
}

float maya_turbulence(
    point surface_point,
    float initial_time,
    float filter_width,
    float amplitude,
    int octaves,
    float lacunarity,
    float gain)
{
    point pp = surface_point;
    float amp = amplitude, fw = filter_width, sum = 0.0, ttime = initial_time;

    for (int i = 0; i < octaves; ++i)
    {
        // Base frequency looks too regular, break it with point+noise.
        float tmp = amp * filtered_snoise(pp + noise(lacunarity), ttime, fw);
        sum += abs(tmp);
        amp *= gain;
        pp *= lacunarity;
        fw *= lacunarity;
        ttime *= lacunarity;
    }
    return sum;
}

float maya_fBm(
    point surface_point,
    float initial_time,
    float filter_width,
    float amplitude,
    int octaves,
    float lacunarity,
    float gain)
{
    point pp = surface_point;
    float amp = amplitude, fw = filter_width, sum = 0.0, ttime = initial_time;

    for (int i = 0; i < octaves; ++i)
    {
        // These magic numbers seem to match Maya better.
        sum += amp * 1.2 * (filtered_snoise(pp, ttime, fw) - 0.1) + 0.05;
        amp *= gain;
        pp *= lacunarity;
        fw *= lacunarity;
        ttime *= lacunarity;
    }
    return clamp(sum * 0.5 + 0.5, 0.0, 1.0);
}

float maya_cos_waves_2d(
    point coords,
    float current_time,
    int current_step,
    int waves)
{
    int seed = current_step * 50;

    float kx, ky, k, out = 0.0;

    for (int i = 0; i < waves; ++i)
    {
        do
        {
            kx = random_noise(seed++);
            ky = random_noise(seed++);
            k = hypot(kx, ky);
        }
        while (k <= 0);

        kx /= k;
        ky /= k;

        float phi = random_noise(seed++) * M_PI;

        out += cos(kx * coords[0] + ky * coords[1] + phi + current_time);
    }
    return out / (float) waves;
}
                                                   
float maya_waves_noise(
    point xyz,
    float amplitude,
    float current_time,
    float frequency_ratio,
    float ratio,
    int max_depth,
    int num_waves,
    int inflection)
{
    float time_ratio = sqrt(frequency_ratio);
    float cycle_time = current_time * M_PI, amp = amplitude, out = 0.0;

    for (int i = 0; i < max_depth; ++i)
    {
        if (!amp)
        {
            break;
        }

        out += amp * maya_cos_waves_2d(xyz, cycle_time, i, num_waves);

        amp *= ratio;
        xyz *= frequency_ratio;

        cycle_time *= time_ratio;
    }

    return (inflection) ? abs(out) : out * 0.5 + 0.5;
}

#endif // AS_MAYA_FRACTAL_HELPERS_H
