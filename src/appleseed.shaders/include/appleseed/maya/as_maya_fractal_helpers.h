
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Luis Barrancos, The appleseedhq Organization
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

#include "appleseed/fractal/as_noise_helpers.h"
#include "appleseed/math/as_math_complex.h"
#include "appleseed/math/as_math_helpers.h"

#define MAYA_LATTICE_SIZE   20

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

void implode_3d(
    float implode,
    point implode_center,
    output point Pp)
{
    if (implode > EPS || implode < -EPS)
    {
        Pp -= implode_center;

        float dist = hypot(Pp[0], Pp[1], Pp[2]);

        if (dist > EPS)
        {
            float factor = pow(dist, 1.0 - implode) / dist;
            Pp *= factor;
        }
        Pp += implode_center;
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
    float seed = current_step;

    float sum = 0.0;

    for (int i = 0; i < waves; ++i)
    {
        vector xi = hashnoise(seed++, 50 * seed++);

        float h = hypot(xi[0], xi[1]);

        if (h >= 0.0)
        {
            float phi = xi[2];
            xi /= h;

            sum += cos(
                xi[0] * surface_point[0] * M_2PI +
                xi[1] * surface_point[1] * M_2PI +
                phi * M_PI +
                current_time * M_PI);
        }
    }
    return sum / (float) waves;
}

float maya_cos_waves_3d(
    point surface_point,
    float current_time,
    int current_step,
    int waves)
{
    int seed = current_step;

    float sum = 0.0;

    for (int i = 0; i < waves; ++i)
    {
        vector xi = hashnoise(seed++, 50 * seed++);

        float h = hypot(xi[0], xi[1], xi[2]);

        if (h >= 0.0)
        {
            xi /= h;
            xi *= surface_point * M_2PI;

            float phi = hashnoise(seed++);

            sum += cos(
                xi[0] + xi[1] + xi[2] +
                phi * M_PI +
                current_time * M_PI);
        }
    }
    return sum / (float) waves;
}

float maya_waves_noise_2d(
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

float maya_waves_noise_3d(
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
            maya_cos_waves_3d(xyz, current_time, i, num_waves);

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
    float jittering = 0.5 * randomness, inv_spottyness = 1.0 - spottyness;
    float blob_density = min(density, 2.0 - randomness), falloff;

    if (blob_density < 1.0e-4)
    {
        return 0.0;
    }

    float blob_size = 2.0 / blob_density;

    float lattice_x = mod(x, 1.0) * MAYA_LATTICE_SIZE;
    float lattice_y = mod(y, 1.0) * MAYA_LATTICE_SIZE;

    float cell_x = lattice_x - trunc(lattice_x);
    float cell_y = lattice_y - trunc(lattice_y);

    int x_cell_index = (int) lattice_x, nx;
    int y_cell_index = (int) lattice_y, ny;

    float sum = 0.0;

    for (int dy = -1; dy <= 1; dy++)
    {
        ny = y_cell_index + dy;

        if (ny >= MAYA_LATTICE_SIZE)
        {
            ny -= MAYA_LATTICE_SIZE;
        }
        else if (ny < 0.0)
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
            else if (nx < 0.0)
            {
                nx += MAYA_LATTICE_SIZE;
            }

            int index = nx + ny;

            vector xi = hashnoise(index++, index++);

            float pos_x = sin(current_time * 2.0 + M_2PI * xi[0]);
            float pos_y = cos(current_time + M_2PI * xi[1]);

            pos_x = pos_x * jittering + 0.5;
            pos_y = pos_y * jittering + 0.5;

            float x_sample_distance = cell_x - (float)dx - pos_x;
            float y_sample_distance = cell_y - (float)dy - pos_y;

            float sample_distance = blob_size * (
                sqr(x_sample_distance) + sqr(y_sample_distance));

            if (size_randomness > 0.0)
            {
                sample_distance /= (xi[2] + 1.0) *
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

                if (spottyness > 0.0)
                {
                    falloff *= spottyness *
                        (hashnoise(index++, index++) + 1.0) + inv_spottyness;
                }
                sum += falloff;
            }
        }
    }
    return sum;
}

float billow_noise_3d(
    float x,
    float y,
    float z,
    float density,
    float randomness,
    float size_randomness,
    float spottyness,
    float current_time,
    int current_step,
    int falloff_mode)
{
    float jittering = 0.5 * randomness, inv_spottyness = 1.0 - spottyness;
    float blob_density = min(density, 2.0 - randomness), falloff;

    if (blob_density < 1.0e-4)
    {
        return 0.0;
    }

    float blob_size = 2.0 / blob_density;

    float lattice_x = mod(x, 1.0) * MAYA_LATTICE_SIZE;
    float lattice_y = mod(y, 1.0) * MAYA_LATTICE_SIZE;
    float lattice_z = mod(z, 1.0) * MAYA_LATTICE_SIZE;

    float cell_x = lattice_x - trunc(lattice_x);
    float cell_y = lattice_y - trunc(lattice_y);
    float cell_z = lattice_z - trunc(lattice_z);

    int x_cell_index = (int) lattice_x, nx;
    int y_cell_index = (int) lattice_y, ny;
    int z_cell_index = (int) lattice_z, nz;

    float sum = 0.0;

    for (int dz = -1; dz <= 1; dz++)
    {
        nz = z_cell_index + dz;

        if (nz >= MAYA_LATTICE_SIZE)
        {
            nz -= MAYA_LATTICE_SIZE;
        }
        else if (nz < 0.0)
        {
            nz += MAYA_LATTICE_SIZE;
        }
        nz *= MAYA_LATTICE_SIZE;

        for (int dy = -1; dy <= 1; dy++)
        {
            ny = y_cell_index + dy;

            if (ny >= MAYA_LATTICE_SIZE)
            {
                ny -= MAYA_LATTICE_SIZE;
            }
            else if (ny < 0.0)
            {
                ny += MAYA_LATTICE_SIZE;
            }
            ny = MAYA_LATTICE_SIZE * (ny + nz);

            for (int dx = -1; dx <= 1; dx++)
            {
                nx = x_cell_index + dx;

                if (nx >= MAYA_LATTICE_SIZE)
                {
                    nx -= MAYA_LATTICE_SIZE;
                }
                else if (nx < 0.0)
                {
                    nx += MAYA_LATTICE_SIZE;
                }

                int index = nx + ny;

                vector xi = hashnoise(vector(index++));

                float pos_x = sin(current_time * 2.0 + M_2PI * xi[0]);
                float pos_y = cos(current_time * 1.5 + M_2PI * xi[1]);
                float pos_z = cos(current_time + M_2PI * xi[2]);

                pos_x = pos_x * jittering + 0.5;
                pos_y = pos_y * jittering + 0.5;
                pos_z = pos_z * jittering * 0.5;

                float x_sample_distance = cell_x - (float) dx - pos_x;
                float y_sample_distance = cell_y - (float) dy - pos_y;
                float z_sample_distance = cell_z - (float) dz - pos_z;

                float sample_distance = blob_size * (
                    sqr(x_sample_distance) +
                    sqr(y_sample_distance) +
                    sqr(z_sample_distance));

                if (size_randomness > 0.0)
                {
                    sample_distance /= (hashnoise(vector(index++)) + 1.0) *
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

                    if (spottyness > 0.0)
                    {
                        falloff *= spottyness *
                            (hashnoise(vector(index++)) + 1.0) + inv_spottyness;
                    }
                    sum += falloff;
                }
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

    float current_time = initial_time, time_ratio = sqrt(frequency_ratio);
    float amplitude = 1.0, total_amplitude = 0.0, sum = 0.0;

    for (int i = 0; i < max_depth; ++i)
    {
        if (amplitude == 0.0)
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
    sum = gain * sum / total_amplitude;
    return sum;
}

float maya_billow_noise_3d(
    point surface_point,
    vector point_scale,
    point origin,
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
    float z = surface_point[2] / MAYA_LATTICE_SIZE, z_offset = 0.0;

    x /= point_scale[0];
    y /= point_scale[1];
    z /= point_scale[2];

    x += origin[0];
    y += origin[1];
    z += origin[2];

    float current_time = initial_time, time_ratio = sqrt(frequency_ratio);
    float amplitude = 1.0, total_amplitude = 0.0, sum = 0.0;

    for (int i = 0; i < max_depth; ++i)
    {
        if (amplitude == 0.0)
        {
            break;
        }

        x_offset += 0.021;
        y_offset += 0.33;
        z_offset += 0.011;

        float val = billow_noise_3d(
            x + x_offset,
            y + y_offset,
            z + z_offset,
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
        z *= frequency_ratio;

        current_time *= time_ratio;
    }
    return gain * sum / total_amplitude;
}


//
// Reference:
//
//      Box and sphere folding: What is a Mandelbox, Tom Lowe
//      https://sites.google.com/site/mandelbox/what-is-a-mandelbox
//
//      The Mandelbox Set, Rudi Chen
//      http://digitalfreepen.com/mandelbox370/
//

void box_fold(output Complex Z)
{
    Z.real = clamp(Z.real, -1.0, 1.0) * 2.0 - Z.real;
    Z.imag = clamp(Z.imag, -1.0, 1.0) * 2.0 - Z.imag;
}

void sphere_fold(
    float sqr_distance,
    float sqr_box_min_radius,
    float sqr_box_radius,
    output Complex Z)
{
    if (sqr_distance < sqr_box_min_radius)
    {
        float tmp = sqr_box_radius / sqr_box_min_radius;
        Z.real *= tmp;
        Z.imag *= tmp;
    }
    else if (sqr_distance < sqr_box_radius)
    {
        float tmp = sqr_box_radius / sqr_distance;
        Z.real *= tmp;
        Z.imag *= tmp;
    }
}

void compute_mandelbox(
    int iteration_depth,
    float box_ratio,
    float leaf_effect,
    float sqr_box_radius,
    float sqr_box_min_radius,
    float sqr_escape_radius,
    Complex C_Z,
    Complex Z,
    output int iteration,
    output int decomposition_sign,
    output float sqr_distance,
    output float min_radius)
{
    Complex Z2, tmp_Z = Z;

    while (iteration < iteration_depth && sqr_distance <= sqr_escape_radius)
    {
        box_fold(tmp_Z);

        Z2.real = sqr(tmp_Z.real);
        Z2.imag = sqr(tmp_Z.imag);

        sqr_distance = Z2.real + Z2.imag;

        if (leaf_effect > 0)
        {
            sqr_distance = mix(
                sqr_distance,
                abs(Z2.real - Z2.imag),
                leaf_effect);
        }

        min_radius = min(min_radius, sqr_distance);

        sphere_fold(
            sqr_distance,
            sqr_box_min_radius,
            sqr_box_radius,
            tmp_Z);

        tmp_Z.real = box_ratio * tmp_Z.real + C_Z.real;
        tmp_Z.imag = box_ratio * tmp_Z.imag + C_Z.imag;
        iteration++;
    }
    decomposition_sign = (int) sign(-tmp_Z.imag);
}

void compute_mandelbrot(
    Complex lobes,
    int iteration_depth,
    float leaf_effect,
    float sqr_escape_radius,
    Complex C_Z,
    Complex Z,
    output int iteration,
    output int decomposition_sign,
    output float sqr_distance,
    output float min_radius)
{
    Complex Z2, tmp_Z = Z;

    while (iteration < iteration_depth && sqr_distance <= sqr_escape_radius)
    {
        if (lobes.real == 2)
        {
            square_complex(tmp_Z);
        }
        else
        {
            pow_complex(tmp_Z, lobes, tmp_Z);
        }

        tmp_Z.real += C_Z.real;
        tmp_Z.imag += C_Z.imag;

        Z2.real = sqr(tmp_Z.real);
        Z2.imag = sqr(tmp_Z.imag);

        sqr_distance = Z2.real + Z2.imag;

        if (leaf_effect > 0)
        {
            sqr_distance = mix(
                sqr_distance,
                abs(Z2.real - Z2.imag),
                leaf_effect);
        }

        min_radius = min(min_radius, sqr_distance);

        iteration++;
    }
    decomposition_sign = (int) sign(-tmp_Z.imag);
}

//
// Reference:
//
//      On Smooth Fractal Coloring Techniques, Jussi Harkonen
//      http://jussiharkonen.com/gallery/coloring-techniques/
//

float mandelbrot_interior_coloring(
    int i,
    int iteration_depth,
    int interior_method,
    float lobes,
    float min_radius,
    float escape_radius,
    float binary_decomposition)
{
    float mapping;

    if (interior_method == 0)
    {
        mapping = 0;
    }
    else if (interior_method == 1)
    {
        mapping = (float) i / (float) iteration_depth;
    }
    else if (interior_method == 2)
    {
        // It doesn't match, but it's a starting point.

        mapping = pow(min_radius, 0.25) * pow(escape_radius, 0.25) / log(2);

        mapping = 1 - mapping;
    }
    else if (interior_method == 3)
    {
        ; // unsupported yet
    }
    else if (interior_method == 4)
    {
        ; // unsupported yet
    }
    else
    {
        ; // unsupported yet
    }
    mapping = clamp(mapping, 0, 1);

    if (interior_method > 0)
    {
        mapping += binary_decomposition;
    }

    return mapping;
}

float mandelbrot_exterior_coloring(
    int i,
    int iteration_depth,
    int exterior_method,
    float lobes,
    float sqr_distance,
    float min_radius,
    float sqr_escape_radius,
    float binary_decomposition)
{
    float mapping;

    if (exterior_method == 0)
    {
        mapping = (float) i / (float) iteration_depth;
    }
    else if (exterior_method == 1)
    {
        float ln_r = log(sqr_distance) / 2;
        float ln_M = log(sqr_escape_radius) / 2;

        mapping = (lobes != 1)
            ? i + 1 + log(ln_M / ln_r) / log(lobes + 1)
            : i + 1 + log(ln_M / ln_r) / M_LN2;

        mapping /= iteration_depth;
    }
    else if (exterior_method == 2)
    {
        // It doesn't match at higher depth counts, but it's a good
        // starting point.

        mapping = sqrt(sqr_escape_radius) - sqrt(min_radius);

        mapping /= iteration_depth;
    }
    else if (exterior_method == 3)
    {
        mapping = sqrt(sqr_distance) / iteration_depth;
        mapping *= sqrt(sqr_escape_radius);
    }
    else
    {
        mapping = 0; // unsupported yet
    }
    mapping = clamp(mapping, 0, 1);

    return mapping + binary_decomposition;
}
