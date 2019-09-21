
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/scalar.h"
#include "foundation/math/spline.h"
#include "foundation/math/vector.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_Math_Spline)
{
    std::vector<Vector2d> zip(const double x[], const double y[], const size_t count)
    {
        std::vector<Vector2d> points(count);

        for (size_t i = 0; i < count; ++i)
        {
            points[i].x = x[i];
            points[i].y = y[i];
        }

        return points;
    }

    TEST_CASE(GeneratePlotFiles)
    {
        // Define knots.
        std::vector<double> knot_x, knot_y;
        knot_x.push_back(  0.0);  knot_y.push_back(0.0);
        knot_x.push_back( 50.0);  knot_y.push_back(1.0);
        knot_x.push_back(100.0);  knot_y.push_back(0.0);

        // Compute output points abscissa.
        const size_t N = 1000;
        const double b = knot_x.front();
        const double e = knot_x.back();
        std::vector<double> point_x;
        for (size_t i = 0; i < N; ++i)
            point_x.push_back(fit<size_t, double>(i, 0, N - 1, b, e));

        GnuplotFile plotfile;

        // Knots.
        plotfile
            .new_plot()
            .set_points(zip(&knot_x[0], &knot_y[0], knot_x.size()))
            .set_title("Knots")
            .set_color("gray");

        // Reference.
        plotfile
            .new_plot()
            .set_points(zip(&knot_x[0], &knot_y[0], knot_x.size()))
            .set_title("Reference")
            .set_color("black")
            .set_smoothing("csplines");

        // Tension = 0.0
        {
            std::vector<double> knot_d(knot_x.size());
            std::vector<double> point_y(point_x.size());

            compute_cardinal_spline_tangents(
                knot_x.size(),
                &knot_x[0],
                &knot_y[0],
                0.0,
                &knot_d[0]);

            cubic_hermite_spline(
                knot_x.size(),
                &knot_x[0],
                &knot_y[0],
                &knot_d[0],
                point_x.size(),
                &point_x[0],
                &point_y[0]);

            plotfile
                .new_plot()
                .set_points(zip(&point_x[0], &point_y[0], point_x.size()))
                .set_title("Tension=0.0")
                .set_color("red");
        }

        // Tension = 0.5
        {
            std::vector<double> knot_d(knot_x.size());
            std::vector<double> point_y(point_x.size());

            compute_cardinal_spline_tangents(
                knot_x.size(),
                &knot_x[0],
                &knot_y[0],
                0.5,
                &knot_d[0]);

            cubic_hermite_spline(
                knot_x.size(),
                &knot_x[0],
                &knot_y[0],
                &knot_d[0],
                point_x.size(),
                &point_x[0],
                &point_y[0]);

            plotfile
                .new_plot()
                .set_points(zip(&point_x[0], &point_y[0], point_x.size()))
                .set_title("Tension=0.5")
                .set_color("green");
        }

        // Tension = 1.0
        {
            std::vector<double> knot_d(knot_x.size());
            std::vector<double> point_y(point_x.size());

            compute_cardinal_spline_tangents(
                knot_x.size(),
                &knot_x[0],
                &knot_y[0],
                1.0,
                &knot_d[0]);

            cubic_hermite_spline(
                knot_x.size(),
                &knot_x[0],
                &knot_y[0],
                &knot_d[0],
                point_x.size(),
                &point_x[0],
                &point_y[0]);

            plotfile
                .new_plot()
                .set_points(zip(&point_x[0], &point_y[0], point_x.size()))
                .set_title("Tension=1.0")
                .set_color("blue");
        }

        plotfile.write("unit tests/outputs/test_spline.gnuplot");
    }
}
