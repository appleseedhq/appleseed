
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Luis Barrancos, The appleseedhq Organization
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

#define MAYA_LATTICE_SIZE   20

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
            float factor = pow(dist, 1.0 - implode) / dist;
            x *= factor;
            y *= factor;
        }
        x += implode_center[0];
        y += implode_center[1];
    }
}

float maya_turbulence(
    point surface_point,
    float initial_time,
    float filter_width,
    float amplitude,
    int octaves,
    float lacunarity,
    float ratio)
{
    point xyz = surface_point;
    float amp = amplitude, filter_size = filter_width;
    float current_time = initial_time, sum = 0.0;

    for (int i = 0; i < octaves; ++i)
    {
        if (amp == 0.0)
        {
            break;
        }

        // The base frequency looks too regular, perturb it with point+noise.
        float val = amp *
            filtered_snoise(
                xyz + noise("uperlin", lacunarity),
                current_time,
                filter_size);

        sum += abs(val);
        amp *= ratio;

        xyz *= lacunarity;

        filter_size *= lacunarity;
        current_time *= lacunarity;
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
    float ratio)
{
    point xyz = surface_point;
    float amp = amplitude, filter_size = filter_width;
    float current_time = initial_time, sum = 0.0;

    for (int i = 0; i < octaves; ++i)
    {
        if (amp == 0.0)
        {
            break;
        }

        // These factors seem to match Maya's result better.
        sum += amp * 1.2 *
            (filtered_snoise(xyz, current_time, filter_size) - 0.1) + 0.05;

        amp *= ratio;
        xyz *= lacunarity;

        filter_size *= lacunarity;
        current_time *= lacunarity;
    }
    return clamp(sum * 0.5 + 0.5, 0.0, 1.0);
}

float maya_cos_waves_2d(
    point surface_point,
    float current_time,
    int current_step,
    int waves)
{
    int seed = current_step * 50;

    float x, y, h, phi, sum = 0.0;

    for (int i = 0; i < waves; ++i)
    {
        x = random_noise(seed++);
        y = random_noise(seed++);
        h = hypot(x, y);

        if (h >= EPS)
        {
            x /= h;
            y /= h;

            phi = random_noise(seed++);

            sum += cos(
                x * surface_point[0] * M_2PI +
                y * surface_point[1] * M_2PI +
                phi * M_PI +
                current_time * M_PI);
        }
    }
    return sum / (float) waves;
}

float maya_waves_noise(
    point surface_point,
    float amplitude,
    float initial_time,
    float frequency_ratio,
    float ratio,
    int max_depth,
    int num_waves,
    int inflection)
{
    point xyz = surface_point;
    float time_ratio = sqrt(frequency_ratio);
    float current_time = initial_time, amp = amplitude, sum = 0.0;

    for (int i = 0; i < max_depth; ++i)
    {
        if (amp == 0.0)
        {
            break;
        }

        float val = amp *
            maya_cos_waves_2d(xyz, current_time, i, num_waves);

        if (inflection)
        {
            val = abs(val);
        }

        sum += val;
        amp *= ratio;
        xyz *= frequency_ratio;
        current_time *= time_ratio;
    }
    return inflection ? sum : sum * 0.5 + 0.5;
}

float billow_noise_2d(
    float x,
    float y,
    float density,
    float randomness,
    float size_randomness,
    float spottyness,
    float current_time,
    int current_step,
    int falloff_mode)
{
    float blob_density = 2.0 - randomness;

    if (blob_density < density)
    {
        blob_density = density;
    }
    if (blob_density < 1.0e-4)
    {
        return 0.0;
    }

    float lattice_x = mod(x, 1.0) * MAYA_LATTICE_SIZE;
    float lattice_y = mod(y, 1.0) * MAYA_LATTICE_SIZE;

    float cell_x = lattice_x - trunc(lattice_x);
    float cell_y = lattice_y - trunc(lattice_y);

    int x_cell_index = (int) lattice_x, nx;
    int y_cell_index = (int) lattice_y, ny;

    float blob_size = 2.0 / blob_density;
    float jittering = 0.5 * randomness;
    float inv_spottyness = 1.0 - spottyness, falloff, sum = 0.0;

    for (int dy = -1; dy <= 1; dy++)
    {
        ny = y_cell_index + dy;

        if (ny >= MAYA_LATTICE_SIZE)
        {
            ny -= MAYA_LATTICE_SIZE;
        }
        else if (ny < 0)
        {
            ny += MAYA_LATTICE_SIZE;
        }

        ny *= MAYA_LATTICE_SIZE;

        for (int dx = -1; dx <= 1; dx++)
        {
            nx = x_cell_index + dx;

            if (nx >= MAYA_LATTICE_SIZE)
            {
                nx -= MAYA_LATTICE_SIZE;
            }
            else if (nx < 0)
            {
                nx += MAYA_LATTICE_SIZE;
            }

            int index = nx + ny;

            float pos_x =
                sin(current_time * 2.0 + M_2PI * random_noise(index++));

            float pos_y =
                cos(current_time + M_2PI * random_noise(index++));

            pos_x = pos_x * jittering + 0.5;
            pos_y = pos_y * jittering + 0.5;

            float x_sample_distance = cell_x - (float)dx - pos_x;
            float y_sample_distance = cell_y - (float)dy - pos_y;

            float sample_distance = blob_size * (
                sqr(x_sample_distance) + sqr(y_sample_distance));
            
            if (size_randomness)
            {
                sample_distance /= (random_noise(index++) + 1.0) *
                    0.5 * size_randomness + (1.0 - size_randomness);
            }
            else
            {
                index++;
            }

            if (sample_distance < 1.0)
            {
                if (falloff_mode == 0)
                {
                    falloff = 1.0 - sqrt(sample_distance);
                }
                else if (falloff_mode == 1)
                {
                    falloff = 1.0 + sample_distance *
                        (-3.0 + 2.0 * sqrt(sample_distance));
                }
                else if (falloff_mode == 2)
                {
                    falloff = 1.0 - sample_distance;
                }
                else if (falloff_mode == 3)
                {
                    sample_distance *= 1.1;

                    falloff = (sample_distance > 1.0)
                        ? 1.0 - (sample_distance - 1.0) * 10.0
                        : sample_distance;
                }

                if (spottyness)
                {
                    falloff *= spottyness *
                        (random_noise(index++) + 1.0) + inv_spottyness;
                }
                sum += falloff;
            }
        }
    }
    return sum;
}

float maya_billow_noise_2d(
    point surface_point,
    float gain,
    float initial_time,
    float frequency_ratio,
    float ratio,
    float density,
    float randomness,
    float size_randomness,
    float spottyness,
    int falloff_mode,
    int max_depth,
    int inflection)
{
    float x = surface_point[0] / MAYA_LATTICE_SIZE, x_offset = 0.0;
    float y = surface_point[1] / MAYA_LATTICE_SIZE, y_offset = 0.0;

    float current_time = initial_time, time_ratio = sqrt(ratio);
    float amplitude = 1.0, total_amplitude = 0.0, sum = 0.0;

    for (int i = 0; i < max_depth; ++i)
    {
        if (!amplitude)
        {
            break;
        }

        x_offset += 0.021;
        y_offset += 0.33;

        float val = billow_noise_2d(
            x + x_offset,
            y + y_offset,
            density,
            randomness,
            size_randomness,
            spottyness,
            current_time,
            i,
            falloff_mode);

        if (inflection)
        {
            val = abs(val);
        }

        sum += amplitude * val;

        total_amplitude += amplitude;
        amplitude *= ratio;

        x *= frequency_ratio;
        y *= frequency_ratio;

        current_time *= time_ratio;
    }
    sum /= total_amplitude;
    return gain * sum;
}

#endif // !AS_MAYA_FRACTAL_HELPERS_H
