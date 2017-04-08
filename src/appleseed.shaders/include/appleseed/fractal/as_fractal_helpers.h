
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

//
// Reference:
//
//      Texturing & Modeling: A Procedural Approach, 3rd edition,
//      David Ebert, F.Kenton Musgrave, Darwyn Peachey, Ken Perlin,
//      Steve Worley.
//      ISBN: 9781558608481 
//

#ifndef AS_FRACTAL_HELPERS_H
#define AS_FRACTAL_HELPERS_H

#include "appleseed/fractal/as_noise_helpers.h"

float fBm(
    point surface_point,
    float initial_time,
    float filter_width,
    float amplitude,
    int octaves,
    float lacunarity,
    float gain)
{
    point xyz = surface_point;
    float amp = amplitude, filter_size = filter_width;
    float current_time = initial_time, sum = 0.0;

    for (int i = 0; i < octaves; ++i)
    {
        if (!amp)
        {
            break;
        }

        sum += amp * filtered_snoise(xyz, current_time, filter_size);
        amp *= gain;
        xyz *= lacunarity;

        filter_size *= lacunarity;
        current_time *= lacunarity;
    }
    return sum;
}

//
// Reference:
//
//      Texturing & Modeling: A Procedural Approach, 3rd edition,
//      David Ebert, F.Kenton Musgrave, Darwyn Peachey, Ken Perlin,
//      Steve Worley.
//      ISBN: 9781558608481
//
//      https://numerics.mathdotnet.com/distance.html
//      https://en.wikipedia.org/wiki/Karlsruhe_metric
//
//      Metrics:
//
//          0) Euclidian distance, or L2 norm, a case of Minkowski
//             metric with P=2.
//
//          1) Sum of Square Differences, square of L2 norm.
//
//          2) Tchebychev distance, L_inf norm, case of Minkowski metric
//             as P approaches infinity.
//
//          3) Sum of Absolute Difference, Manhattan or taxicab distance or
//             L1 norm. Case of Minkowski metric with P=1.
//
//          4) Akritean distance, a combination of the Euclidian and the
//             Manhattan distance, with coverage parameter.
//
//          5) Minkowski metric, with parameter P.
//
//          6) Karlsruhe or Moscow metric.
//

float metric_2D(
    int metric,
    float surface_position[2],
    float test_position[2],
    float Minkowski_P,
    float Akritean_coverage)
{
    float delta[2] = {
        test_position[0] - surface_position[0],
        test_position[1] - surface_position[1]}, dist;
                        
    if (metric == 0)
    {
        dist = hypot(delta[0], delta[1]);
    }
    else if (metric == 1)
    {
        dist = sqr(delta[0]) + sqr(delta[1]);
    }
    else if (metric == 2)
    {
        dist = max(abs(delta[0]), abs(delta[1]));
    }
    else if (metric == 3)
    {
        dist = abs(delta[0]) + abs(delta[1]);
    }
    else if (metric == 4)
    {
        float L2 = hypot(delta[0], delta[1]);
        float L1 = abs(delta[0]) + abs(delta[1]);

        dist = mix(L2, L1, Akritean_coverage);
    }
    else if (metric == 5)
    {
        float dx = pow(abs(delta[0]), Minkowski_P);
        float dy = pow(abs(delta[1]), Minkowski_P);

        dist = pow(dx + dy, 1 / Minkowski_P);
    }
    else
    {
        float r1 = hypot(test_position[0], test_position[1]);
        float r2 = hypot(surface_position[0], surface_position[1]);

        float theta1 = atan2(test_position[1], -test_position[0]) + M_PI;
        float theta2 = atan2(surface_position[1], -surface_position[0]) + M_PI;

        float abs_delta_theta = abs(theta1 - theta2);
        float delta_theta = min(abs_delta_theta, M_2PI - abs_delta_theta);

        if (delta_theta <= 2)
        {
            dist = min(r1, r2) * delta_theta + abs(r1 - r2);
        }
        else
        {
            dist = r1 + r2;
        }
    }
    return dist;
}

float metric_3D(
    int metric,
    point surface_position,
    point test_position,
    float Minkowski_P,
    float Akritean_coverage)
{
    vector delta = test_position - surface_position;
    float dist = 0;

    if (metric == 0)
    {
        dist = length(delta);
    }
    else if (metric == 1)
    {
        dist = sqr(delta[0]) + sqr(delta[1]) + sqr(delta[2]);
    }
    else if (metric == 2)
    {
        dist = max(abs(delta[0]), max(abs(delta[1]), abs(delta[2])));
    }
    else if (metric == 3)
    {
        dist = abs(delta[0]) + abs(delta[1]) + abs(delta[2]);
    }
    else if (metric == 4)
    {
        float L2 = length(delta);
        float L1 = abs(delta[0]) + abs(delta[1]) + abs(delta[2]);

        dist = mix(L2, L1, Akritean_coverage);
    }
    else if (metric == 5)
    {
        float dx = pow(abs(delta[0]), Minkowski_P);
        float dy = pow(abs(delta[1]), Minkowski_P);
        float dz = pow(abs(delta[2]), Minkowski_P);

        dist = pow(dx + dy + dz, 1 / Minkowski_P);
    }
    else
    {
        float r1 = length(test_position);
        float r2 = length(surface_position);

        float theta1 = atan2(test_position[1], -test_position[0]) + M_PI;
        float theta2 = atan2(surface_position[1], -surface_position[0]) + M_PI;

        float abs_delta_theta = abs(theta1 - theta2);
        float delta_theta = min(abs_delta_theta, M_2PI - abs_delta_theta);

        if (delta_theta <= 2)
        {
            dist = min(r1, r2) * delta_theta + abs(r1 - r2);
        }
        else
        {
            dist = r1 + r2;
        }
    }
    return dist;
}

void voronoi_3D(
    point surface_point,
    float jittering,
    int metric,
    float Minkowski_P,
    float Akritean_coverage,
    output float features[4],
    output point positions[4],
    output color cell_IDs[4])
{
    float dist;

    point test_cell, test_position;
    point this_cell = point(floor(surface_point[0]) + 0.5,
                            floor(surface_point[1]) + 0.5,
                            floor(surface_point[2]) + 0.5);   

    for (int i = -1; i <= 1; ++i)
    {
        for (int j = -1; j <= 1; ++j)
        {
            for (int k = -1; k <= 1; ++k)
            {
                test_cell = this_cell + vector(i, j, k);

                test_position = test_cell + jittering *
                    ((vector) cellnoise(test_cell) - 0.5);
                    
                dist = metric_3D(
                    metric,
                    surface_point,
                    test_position,
                    Minkowski_P,
                    Akritean_coverage);


                if (dist < features[0])
                {
                    features[3] = features[2];
                    features[2] = features[1];
                    features[1] = features[0];
                    features[0] = dist;

                    positions[3] = positions[2];
                    positions[2] = positions[1];
                    positions[1] = positions[0];
                    positions[0] = test_position;

                    cell_IDs[3] = cell_IDs[2];
                    cell_IDs[2] = cell_IDs[1];
                    cell_IDs[1] = cell_IDs[0];
                    cell_IDs[0] = cellnoise(test_position);
                }
                else if (dist < features[1])
                {
                    features[3] = features[2];
                    features[2] = features[1];
                    features[1] = dist;

                    positions[3] = positions[2];
                    positions[2] = positions[1];
                    positions[1] = test_position;

                    cell_IDs[3] = cell_IDs[2];
                    cell_IDs[2] = cell_IDs[1];
                    cell_IDs[1] = cellnoise(test_position);
                }
                else if (dist < features[2])
                {
                    features[3] = features[2];
                    features[2] = dist;

                    positions[3] = positions[2];
                    positions[2] = test_position;

                    cell_IDs[3] = cell_IDs[2];
                    cell_IDs[2] = cellnoise(test_position);
                }
                else if (dist < features[3])
                {
                    features[3] = dist;

                    positions[3] = test_position;

                    cell_IDs[3] = cellnoise(test_position);
                }
            }
        }
    }
}

#endif // !AS_FRACTAL_HELPERS_H
