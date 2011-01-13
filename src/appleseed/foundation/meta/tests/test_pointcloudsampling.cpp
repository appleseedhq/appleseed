
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/drawing.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/math/area.h"
#include "foundation/math/distance.h"
#include "foundation/math/knn.h"
#include "foundation/math/rng.h"
#include "foundation/math/rr.h"
#include "foundation/math/sampling.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <sstream>
#include <vector>

using namespace foundation;
using namespace std;

TEST_SUITE(Exploration_PointCloudSampling)
{
    Vector2d sample_disk_non_uniform(const Vector2d& s)
    {
        return (1.0 - s[0]) * sample_circle_uniform(s[1]);
    }

    Vector2d sample_disk_uniform(
        const Vector2d&             center,
        const double                radius,
        const Vector2d&             s)
    {
        return center + radius * sample_disk_uniform_alt(s);
    }

    Vector2d sample_disk_non_uniform(
        const Vector2d&             center,
        const double                radius,
        const Vector2d&             s)
    {
        return center + radius * sample_disk_non_uniform(s);
    }

    void random_barycentric_coordinates(
        MersenneTwister&            rng,
        const size_t                count,
        double                      bary[])
    {
        double sum = 0.0;

        for (size_t i = 0; i < count; ++i)
        {
            const double s = 1.0 - rand_double2(rng);
            bary[i] = s;
            sum += s;
        }

        assert(sum > 0.0);

        const double rcp_sum = 1.0 / sum;

        for (size_t i = 0; i < count; ++i)
            bary[i] *= rcp_sum;
    }

    void generate_random_initial_points_bean(
        MersenneTwister&            rng,
        vector<Vector2d>&           points,
        const size_t                count)
    {
        const Vector2d ArcOrigin(0.25, 0.25);
        const double ArcRadius = 0.5;
        const double DiskRadius = 0.1;
        const size_t ArcSteps = 10;

        for (size_t i = 0; i < ArcSteps; ++i)
        {
            const double alpha = HalfPi * static_cast<double>(i) / (ArcSteps - 1);

            const Vector2d center(
                ArcOrigin.x + ArcRadius * cos(alpha),
                ArcOrigin.y + ArcRadius * sin(alpha));

            for (size_t j = 0; j < count / ArcSteps; ++j)
            {
                Vector2d s;
                s[0] = rand_double2(rng);
                s[1] = rand_double2(rng);
                points.push_back(sample_disk_uniform(center, DiskRadius, s));
            }
        }
    }

    void generate_random_initial_points_disk(
        MersenneTwister&            rng,
        vector<Vector2d>&           points,
        const size_t                count)
    {
        const Vector2d Center(0.5, 0.5);
        const double Radius = 0.2;

        for (size_t i = 0; i < count; ++i)
        {
            Vector2d s;
            s[0] = rand_double2(rng);
            s[1] = rand_double2(rng);
            points.push_back(sample_disk_uniform(Center, Radius, s));
        }
    }

    void generate_random_initial_points_circle(
        MersenneTwister&            rng,
        vector<Vector2d>&           points,
        const size_t                count)
    {
        const Vector2d Center(0.5, 0.5);
        const double Radius = 0.2;

        for (size_t i = 0; i < count; ++i)
            points.push_back(Center + Radius * sample_circle_uniform(rand_double2(rng)));
    }

    // todo: move to foundation/math/area.h.
    template <typename T>
    T triangle_area(
        const Vector<T, 2>& v0,
        const Vector<T, 2>& v1,
        const Vector<T, 2>& v2)
    {
        const Vector<T, 2> e0 = v1 - v0;
        const Vector<T, 2> e1 = v2 - v0;
        return T(0.5) * abs((v1.x - v0.x) * (v2.y - v0.y) - (v2.x - v0.x) * (v1.y - v0.y));
    }

    //
    // Method   Algorithm
    //
    //   M1     Non-uniform sampling of fixed-radius disks centered on initial points
    //   M2     Non-uniform sampling of disks extending to the farthest of the K nearest
    //          neighbors around initial points
    //   M3     Uniform sampling of triangles whose vertices are random initial points
    //   M4     Uniform sampling of triangles built from the three closest neighbors around
    //          random query points
    //   M5     Old Particle Porn algorithm: apply M4 around points selected via M1
    //   M6     New Particle Porn algorithm: M1 with probabilistic rejection of new points
    //          that are too far from their neighbors
    //   M7     Surface method
    //

    void generate_new_random_points(
        MersenneTwister&            rng,
        vector<Vector2d>&           new_points,
        const vector<Vector2d>&     initial_points,
        const size_t                multiplication_rate,
        const size_t                method)
    {
        const double SamplingRadius = 0.025;
        const size_t NeighborCount = 3;

        assert(NeighborCount >= 3);     // in order to build triangles from nearest neighbors

        knn::Tree2d tree;
        knn::Builder2d builder(tree);
        builder.build(&initial_points[0], initial_points.size());

        knn::Answer<double> answer(NeighborCount);
        knn::Query2d query(tree, answer);

        for (size_t i = 0; i < initial_points.size(); ++i)
        {
            for (size_t j = 0; j < multiplication_rate - 1; ++j)
            {
                if (method == 1)
                {
                    Vector2d s;
                    s[0] = rand_double2(rng);
                    s[1] = rand_double2(rng);

                    const Vector2d p =
                        sample_disk_non_uniform(initial_points[i], SamplingRadius, s);

                    new_points.push_back(p);
                }
                else if (method == 2)
                {
                    query.run(initial_points[i]);
                    answer.sort();

                    const double radius =
                        sqrt(
                            square_distance(
                                initial_points[i],
                                initial_points[answer.get(NeighborCount - 1).m_index]));

                    Vector2d s;
                    s[0] = rand_double2(rng);
                    s[1] = rand_double2(rng);

                    const Vector2d p =
                        sample_disk_non_uniform(initial_points[i], radius, s);

                    new_points.push_back(p);
                }
                else if (method == 3)
                {
                    Vector2d v[3];
                    v[0] = initial_points[rand_int1(rng, 0, initial_points.size() - 1)];
                    v[1] = initial_points[rand_int1(rng, 0, initial_points.size() - 1)];
                    v[2] = initial_points[rand_int1(rng, 0, initial_points.size() - 1)];

                    Vector2d s;
                    s[0] = rand_double2(rng);
                    s[1] = rand_double2(rng);

                    const Vector3d knn_bary = sample_triangle_uniform(s);

                    Vector2d new_point(0.0);
     
                    for (size_t w = 0; w < 3; ++w)
                        new_point += knn_bary[w] * v[w];

                    new_points.push_back(new_point);
                }
                else if (method == 4)
                {
                    Vector2d p;
                    p[0] = rand_double1(rng);
                    p[1] = rand_double1(rng);

                    query.run(p);

                    Vector2d s;
                    s[0] = rand_double2(rng);
                    s[1] = rand_double2(rng);

                    const Vector3d knn_bary = sample_triangle_uniform(s);

                    Vector2d new_point(0.0);
     
                    for (size_t w = 0; w < 3; ++w)
                    {
                        const size_t neighbor_index = answer.get(w).m_index;
                        new_point += knn_bary[w] * initial_points[neighbor_index];
                    }

                    new_points.push_back(new_point);
                }
                else if (method == 5)
                {
                    Vector2d s;
                    s[0] = rand_double2(rng);
                    s[1] = rand_double2(rng);

                    const Vector2d p =
                        sample_disk_non_uniform(initial_points[i], SamplingRadius, s);

                    query.run(p);

                    double knn_bary[NeighborCount];
                    random_barycentric_coordinates(rng, NeighborCount, knn_bary);

                    Vector2d new_point(0.0);

                    for (size_t w = 0; w < NeighborCount; ++w)
                    {
                        const size_t neighbor_index = answer.get(w).m_index;
                        new_point += knn_bary[w] * initial_points[neighbor_index];
                    }

                    new_points.push_back(new_point);
                }
                else if (method == 6)
                {
                    while (true)
                    {
                        Vector2d s;
                        s[0] = rand_double2(rng);
                        s[1] = rand_double2(rng);

                        const Vector2d p =
                            sample_disk_uniform(initial_points[i], SamplingRadius, s);

                        query.run(p);

                        double avg_dist = 0.0;
                        for (size_t w = 0; w < NeighborCount; ++w)
                            avg_dist += answer.get(w).m_distance;
                        avg_dist /= NeighborCount;

                        const double Tension = 150.0;
                        const double k = avg_dist / SamplingRadius;
                        const double acceptance_prob = exp(-k * Tension);

                        if (pass_rr(acceptance_prob, rand_double2(rng)))
                        {
                            new_points.push_back(p);
                            break;
                        }
                    }
                }
                else if (method == 7)
                {
                    query.run(initial_points[i]);

                    Vector2d avg_n(0.0);

                    for (size_t w = 0; w < NeighborCount; ++w)
                    {
                        const size_t neighbor_index = answer.get(w).m_index;
                        const Vector2d neighbor_point = initial_points[neighbor_index];
                        const Vector2d d = initial_points[i] - neighbor_point;

                        Vector2d n(-d.y, d.x);

                        if (dot(n, avg_n) < 0.0)
                            n = -n;

                        if (square_norm(n) > 0.0)
                            avg_n += normalize(n);
                    }

                    avg_n = normalize(avg_n);

                    const Vector2d plane(-avg_n.y, avg_n.x);

                    const Vector2d new_point =
                        initial_points[i] + rand_double1(rng, -SamplingRadius, SamplingRadius) * plane;

                    new_points.push_back(new_point);
                }
                else
                {
                    assert(!"Unknown method");
                }
            }
        }
    }

    TEST_CASE(Main)
    {
        MersenneTwister rng;

        vector<Vector2d> initial_points;
//      generate_random_initial_points_bean(rng, initial_points, 100);
        generate_random_initial_points_circle(rng, initial_points, 100);

        for (size_t method = 1; method <= 7; ++method)
        {
            vector<Vector2d> new_points;
            generate_new_random_points(rng, new_points, initial_points, 100, method);

            Image image(
                1024,
                1024,
                32,
                32,
                3,
                PixelFormatFloat);

            image.clear(Color3f(0.0f));

            for (size_t i = 0; i < new_points.size(); ++i)
                Drawing::draw_dot(image, new_points[i], Color3f(0.2f));

            for (size_t i = 0; i < initial_points.size(); ++i)
                Drawing::draw_dot(image, initial_points[i], Color3f(1.0f, 1.0f, 0.0f));

            stringstream sstr;
            sstr << "output/test_pointcloudsampling_M" << method << ".png";

            GenericImageFileWriter().write(sstr.str().c_str(), image);
        }
    }

    void rasterize(const Vector2d& point, Image& image)
    {
        const int Radius = 100;         // in pixels
        const double Scale = 1.0;
        const double Exponent = 2.0;

        const CanvasProperties& props = image.properties();

        const int cx = static_cast<int>(point.x * props.m_canvas_width);
        const int cy = static_cast<int>(point.y * props.m_canvas_height);
        const int max_x = static_cast<int>(props.m_canvas_width);
        const int max_y = static_cast<int>(props.m_canvas_height);

        for (int y = max(cy - Radius, 0); y < min(cy + Radius, max_y); ++y)
        {
            for (int x = max(cx - Radius, 0); x < min(cx + Radius, max_x); ++x)
            {
                const double dx2 = square(x - cx);
                const double dy2 = square(y - cy);
                const double distance = sqrt(dx2 + dy2) / Radius;
                const double intensity =
                    Scale * pow(saturate(1.0 - distance), Exponent);

                Color3f color;

                image.get_pixel(
                    static_cast<size_t>(x),
                    static_cast<size_t>(y),
                    color);

                color += Color3f(static_cast<float>(intensity));
                color = saturate(color);

                image.set_pixel(
                    static_cast<size_t>(x),
                    static_cast<size_t>(y),
                    color);
            }
        }
    }

    void rasterize(const vector<Vector2d>& points, Image& image)
    {
        for (size_t i = 0; i < points.size(); ++i)
            rasterize(points[i], image);
    }

    void apply_threshold(const double threshold, Image& image)
    {
        const CanvasProperties& props = image.properties();

        for (size_t y = 0; y < props.m_canvas_height; ++y)
        {
            for (size_t x = 0; x < props.m_canvas_width; ++x)
            {
                Color3f color;
                image.get_pixel(x, y, color);

                if (color[0] < threshold)
                    color.set(0.0f);
                else color.set(1.0f);

                image.set_pixel(x, y, color);
            }
        }
    }

    TEST_CASE(Thresholding)
    {
        MersenneTwister rng;

        vector<Vector2d> initial_points;
        generate_random_initial_points_bean(rng, initial_points, 100);

        Image image(
            1024,
            1024,
            32,
            32,
            3,
            PixelFormatFloat);

        image.clear(Color3f(0.0f));

        rasterize(initial_points, image);
        apply_threshold(0.99, image);

        for (size_t i = 0; i < initial_points.size(); ++i)
            Drawing::draw_dot(image, initial_points[i], Color3f(1.0f, 1.0f, 0.0f));

        stringstream sstr;
        sstr << "output/test_pointcloudsampling_thresholding.png";

        GenericImageFileWriter().write(sstr.str().c_str(), image);
    }
}
