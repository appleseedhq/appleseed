
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Luis Barrancos, The appleseedhq Organization
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

#include "appleseed/math/as_math_helpers.h"
#include "appleseed/maya/as_maya_recurrence_tables.h"

//
// Set of auxiliary functions for the Maya crater, stucco 3D texture node, see
// also: ${MAYA_LOCATION}/docs/Nodes/crater.html
//

float recurrence(
    float size,
    point sample_center,
    float shaker,
    float melt)
{
    float tex_random_table[4096] = { RANDOM_TABLE_2 };
    float tex_noise_offset[12] = { NOISE_TABLE_OFFSET };

    float randomTable(int x, int y, int z)
    {
        return tex_random_table[
             ((x ^ (y >> 4) ^ (z >> 8)) & 255) ^
            (((y ^ (z >> 4) ^ (x >> 8)) & 255) << 4) ^
            (((z ^ (x >> 4) ^ (y >> 8)) & 255) << 8) & 4095];
    }

    float tmp_size = size;
    float delta = tmp_size;
    int nb;

    if (tmp_size < 0.00390625)
    {
        nb = 10;
    }
    else
    {
        nb = 1;

        for (int i = 0; i < 10; ++i)
        {
            if (tmp_size > 1.0)
            {
                break;
            }
            tmp_size *= 2.0;
            nb++;
        }
    }

    int nb1 = nb - 1, gainin = 0x1;
    float out_recurrence = 0;

    for (int i = 0; i < nb; i++, gainin <<= 1)
    {
        float nx = sample_center[0] * gainin + tex_noise_offset[i];
        float ny = sample_center[1] * gainin + tex_noise_offset[i + 1];
        float nz = sample_center[2] * gainin + tex_noise_offset[i + 2];

        int xi = (int) floor(nx);
        int yi = (int) floor(ny);
        int zi = (int) floor(nz);

        float xf = nx - xi;
        float yf = ny - yi;
        float zf = nz - zi;

        float h0 = randomTable(xi, yi, zi);
        float h1 = randomTable(xi + 1, yi, zi);
        float h2 = randomTable(xi, yi, zi + 1);
        float h3 = randomTable(xi + 1, yi, zi + 1);
        float h4 = randomTable(xi, yi + 1, zi);
        float h5 = randomTable(xi + 1, yi + 1, zi);
        float h6 = randomTable(xi, yi + 1, zi + 1);
        float h7 = randomTable(xi + 1, yi + 1, zi + 1);

        float xzf = xf * zf;

        float tmp1 = h0 + xf * (h1 - h0) + zf * (h2 - h0) +
                    xzf * (h0 + h3 - h1 - h2);

        float tmp2 = h4 + xf * (h5 - h4) + zf * (h6 - h4) +
                    xzf * (h4 + h7 - h5 - h6);

        if (nb == 1)
        {
            out_recurrence = (tmp1 + yf * (tmp2 - tmp1)) * shaker;
        }
        else if (i == nb1)
        {
            out_recurrence += (tmp1 + yf * (tmp2 - tmp1)) *
                2.0 * (2.0 / gainin - delta);

            out_recurrence *= shaker;
        }
        else
        {
            out_recurrence += (tmp1 + yf * (tmp2 - tmp1)) / gainin;
        }
    }
    return out_recurrence;
}

float recurrenceN(
    float size,
    point sample_center,
    float shaker,
    float melt)
{
    float tmp_size = size;

    if (tmp_size < melt * 2)
    {
        tmp_size = melt * 2;
    }
    if (tmp_size < EPS)
    {
        tmp_size = 1.0;
    }

    return recurrence(tmp_size, sample_center, shaker, melt);
}

vector recurrence3(
    float size,
    point sample_center,
    float normal_depth,
    float normal_frequency)
{
    float tex_random_table[4096] = { RANDOM_TABLE_2 };
    float tex_noise_offset[12] = { NOISE_TABLE_OFFSET };

    float randomTable(int x, int y, int z)
    {
        return tex_random_table[
             ((x ^ (y >> 4) ^ (z >> 8)) & 255) ^
            (((y ^ (z >> 4) ^ (x >> 8)) & 255) << 4) ^
            (((z ^ (x >> 4) ^ (y >> 8)) & 255) << 8) & 4095];
    }

    float tmp_size = size;

    if (tmp_size < normal_frequency * 2.0)
    {
        tmp_size = normal_frequency * 2.0;
    }

    float delta = tmp_size;
    int nb;

    if (tmp_size < 0.00390625)
    {
        nb = 10;
    }
    else
    {
        nb = 1;

        for (int i = 0; i < 10; ++i)
        {
            if (tmp_size > 1.0)
            {
                break;
            }
            tmp_size *= 2.0;
            nb++;
        }
    }

    vector out_recurrence = vector(0);
    int nb1 = nb - 1, gainin = 1;

    for (int i = 0; i < nb; ++i, gainin <<= 1)
    {
        float nx = sample_center[0] * gainin + tex_noise_offset[i];
        float ny = sample_center[1] * gainin + tex_noise_offset[i + 1];
        float nz = sample_center[2] * gainin + tex_noise_offset[i + 2];

        int xi = (int) floor(nx);
        int yi = (int) floor(ny);
        int zi = (int) floor(nz);

        float tx = nx - xi;
        float ty = ny - yi;
        float tz = nz - zi;

        float h0 = randomTable(xi, yi, zi);
        float h1 = randomTable(xi + 1, yi, zi);
        float h2 = randomTable(xi, yi, zi + 1);
        float h3 = randomTable(xi + 1, yi, zi + 1);
        float h4 = randomTable(xi, yi + 1, zi);
        float h5 = randomTable(xi + 1, yi + 1, zi);
        float h6 = randomTable(xi, yi + 1, zi + 1);
        float h7 = randomTable(xi + 1, yi + 1, zi + 1);

        float tmp_tx = 1.0 - tx;
        float tmp_ty = 1.0 - ty;
        float tmp_tz = 1.0 - tz;

        float coeff0 = tx * tmp_tx;
        float coeff1 = ty * tmp_ty;
        float coeff2 = tz * tmp_tz;

        if (nb == 1)
        {
            out_recurrence[0] = coeff0 * normal_depth *
                (tmp_tz * (tmp_ty * (h1 - h0) + ty * (h5 - h4)) +
                tz * (tmp_ty * (h3 - h2) + ty * (h7 - h6)));

            out_recurrence[1] = coeff1 * normal_depth *
                (tmp_tz * (tmp_tx * (h4 - h0) + tx * (h5 - h1)) +
                tz * (tmp_tx * (h6 - h2) + tx * (h7 - h3)));

            out_recurrence[2] = coeff2 * normal_depth *
                (tmp_ty * (tmp_tx * (h2 - h0) + tx * (h3 - h1)) +
                ty * (tmp_tx * (h6 - h4) + tx * (h7 - h5)));
        }
        else if (i == nb1)
        {
            float coeffin = 2.0 / (gainin * delta) - 1.0;

            out_recurrence[0] += coeff0 * coeffin *
                (tmp_tz * (tmp_ty * (h1 - h0) + ty * (h5 - h4)) +
                tz * (tmp_ty * (h3 - h2) + ty * (h7 - h6)));

            out_recurrence[0] *= normal_depth;

            out_recurrence[1] += coeff1 * coeffin *
                (tmp_tz * (tmp_tx * (h4 - h0) + tx * (h5 - h1)) +
                tz * (tmp_tx * (h6 - h2) + tx + (h7 - h3)));

            out_recurrence[1] *= normal_depth;

            out_recurrence[2] += coeff2 * coeffin *
                (tmp_ty * (tmp_tx * (h2 - h0) + tx * (h3 - h1)) +
                ty * (tmp_tx * (h6 - h4) + tx * (h7 - h5)));

            out_recurrence[2] *= normal_depth;
        }
        else
        {
            out_recurrence[0] += coeff0 *
                (tmp_tz * (tmp_ty * (h1 - h0) + ty * (h5 - h4)) +
                tz * (tmp_ty * (h3 - h2) + ty * (h7 - h6)));

            out_recurrence[1] += coeff1 *
                (tmp_tz * (tmp_tx * (h4 - h0) + tx * (h5 - h1)) +
                tz * (tmp_tx * (h6 - h2) + tx * (h7 - h3)));

            out_recurrence[2] += coeff2 *
                (tmp_ty * (tmp_tx * (h2 - h0) + tx * (h3 - h1)) +
                ty * (tmp_tx * (h6 - h4) + tx * (h7 - h5)));
        }
    }
    return out_recurrence;
}

#endif // !AS_MAYA_FRACTAL_HELPERS_H
