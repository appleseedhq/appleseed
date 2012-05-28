
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "foundation/math/spline.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/maplefile.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <string>
#include <vector>

TEST_SUITE(Foundation_Math_Spline)
{
    using namespace foundation;
    using namespace std;

    TEST_CASE(GenerateMaplePlotFiles)
    {
        // Define knots.
        vector<double> knot_x, knot_y;
        knot_x.push_back(  0.0);  knot_y.push_back(0.0);
        knot_x.push_back( 50.0);  knot_y.push_back(1.0);
        knot_x.push_back(100.0);  knot_y.push_back(0.0);

        // Compute output point abscissa.
        const size_t N = 1000;
        vector<double> point_x;
        const double b = knot_x.front();
        const double e = knot_x.back();
        for (size_t i = 0; i < N; ++i)
        {
            const double t = static_cast<double>(i) / (N - 1);
            point_x.push_back(b + t * (e - b));
        }

        // Open Maple file for writing.
        MapleFile maple_file("unit tests/outputs/test_spline.mpl");

        // Initialize.
        maple_file.restart();
        maple_file.with("CurveFitting");

        // Knots.
        maple_file.define("knots", knot_x, knot_y);

        // Reference spline.
        maple_file.print("ref:=Spline(knots,x,degree=3):\n");
        maple_file.print(
            "plot([knots,ref(x)],x=%f..%f,color=[gray,black],"
            "legend=[\"knots\",\"reference\"]);\n",
            b, e);

        // Tension = 0.0
        {
            vector<double> knot_d(knot_x.size());
            vector<double> point_y(point_x.size());
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
            maple_file.define("output1", point_x, point_y);
            maple_file.plot(
                make_vector(
                    MaplePlotDef("knots").set_legend("knots").set_color("gray"),
                    MaplePlotDef("output1").set_legend("spline for tension 0.0").set_color("red")));
        }

        // Tension = 0.5
        {
            vector<double> knot_d(knot_x.size());
            vector<double> point_y(point_x.size());
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
            maple_file.define("output2", point_x, point_y);
            maple_file.plot(
                make_vector(
                    MaplePlotDef("knots").set_legend("knots").set_color("gray"),
                    MaplePlotDef("output2").set_legend("spline for tension 0.5").set_color("red")));
        }

        // Tension = 1.0
        {
            vector<double> knot_d(knot_x.size());
            vector<double> point_y(point_x.size());
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
            maple_file.define("output3", point_x, point_y);
            maple_file.plot(
                make_vector(
                    MaplePlotDef("knots").set_legend("knots").set_color("gray"),
                    MaplePlotDef("output3").set_legend("spline for tension 1.0").set_color("red")));
        }

        // Comparative plots.
        maple_file.plot(
            make_vector(
                MaplePlotDef("output1").set_legend("spline for tension 0.0").set_color("red"),
                MaplePlotDef("output2").set_legend("spline for tension 0.5").set_color("green"),
                MaplePlotDef("output3").set_legend("spline for tension 1.0").set_color("blue")));
        maple_file.print(
            "plot([ref(x),output1,output3],x=%f..%f,color=[black,red,blue],"
            "legend=[\"reference\",\"spline for tension 0.0\",\"spline for tension 1.0\"]);\n",
            b, e);
    }
}
