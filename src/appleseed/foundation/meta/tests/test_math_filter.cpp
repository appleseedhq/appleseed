
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/filter.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace
{
    bool is_zero_on_domain_border(const Filter2d& filter)
    {
        const double Eps = 1.0e-6;

        return
            fz(filter.evaluate(-filter.get_xradius(), -filter.get_yradius()), Eps) &&
            fz(filter.evaluate(                  0.0, -filter.get_yradius()), Eps) &&
            fz(filter.evaluate(+filter.get_xradius(), -filter.get_yradius()), Eps) &&
            fz(filter.evaluate(+filter.get_xradius(),                   0.0), Eps) &&
            fz(filter.evaluate(+filter.get_xradius(), +filter.get_yradius()), Eps) &&
            fz(filter.evaluate(                  0.0, +filter.get_yradius()), Eps) &&
            fz(filter.evaluate(-filter.get_xradius(), +filter.get_yradius()), Eps) &&
            fz(filter.evaluate(-filter.get_xradius(),                   0.0), Eps);
    }

    void plot(
        const string&   filepath,
        const string&   title,
        const Filter2d& filter)
    {
        const double r = filter.get_xradius();

        const size_t PointCount = 256;
        vector<Vector2d> points(PointCount);

        for (size_t i = 0; i < PointCount; ++i)
        {
            const double x = fit<size_t, double>(i, 0, PointCount, -r - 1.0, r + 1.0);
            const double value = x < -r || x > r ? 0.0 : filter.evaluate(x, 0.0);
            points[i] = Vector2d(x, value);
        }

        GnuplotFile plotfile;
        plotfile
            .new_plot()
            .set_points(points)
            .set_title(title + ", radius=" + pretty_scalar(r, 1));
        plotfile.write(filepath);
    }
}

TEST_SUITE(Foundation_Math_Filter_BoxFilter2)
{
    TEST_CASE(TestPropertyGetters)
    {
        const BoxFilter2<double> filter(2.0, 3.0);

        EXPECT_EQ(2.0, filter.get_xradius());
        EXPECT_EQ(3.0, filter.get_yradius());
    }

    TEST_CASE(Plot)
    {
        const BoxFilter2<double> filter(2.0, 3.0);

        plot(
            "unit tests/outputs/test_math_filter_boxfilter2.gnuplot",
            "Box Filter",
            filter);
    }
}

TEST_SUITE(Foundation_Math_Filter_TriangleFilter2)
{
    TEST_CASE(Evaluate_PointsOnDomainBorder_ReturnsZero)
    {
        const TriangleFilter2<double> filter(2.0, 3.0);

        EXPECT_TRUE(is_zero_on_domain_border(filter));
    }

    TEST_CASE(Plot)
    {
        const TriangleFilter2<double> filter(2.0, 3.0);

        plot(
            "unit tests/outputs/test_math_filter_trianglefilter2.gnuplot",
            "Triangle Filter",
            filter);
    }
}

TEST_SUITE(Foundation_Math_Filter_GaussianFilter2)
{
    const double Alpha = 4.0;

    TEST_CASE(Evaluate_PointsOnDomainBorder_ReturnsZero)
    {
        const GaussianFilter2<double> filter(2.0, 3.0, Alpha);

        EXPECT_TRUE(is_zero_on_domain_border(filter));
    }

    TEST_CASE(Plot)
    {
        const GaussianFilter2<double> filter(2.0, 3.0, Alpha);

        plot(
            "unit tests/outputs/test_math_filter_gaussianfilter2.gnuplot",
            "Gaussian Filter, alpha=" + pretty_scalar(Alpha, 1),
            filter);
    }
}

TEST_SUITE(Foundation_Math_Filter_MitchellFilter2)
{
    const double B = 1.0 / 3;
    const double C = (1.0 - B) / 2.0;

    TEST_CASE(Evaluate_PointsOnDomainBorder_ReturnsZero)
    {
        const MitchellFilter2<double> filter(2.0, 3.0, B, C);

        EXPECT_TRUE(is_zero_on_domain_border(filter));
    }

    TEST_CASE(Plot)
    {
        const MitchellFilter2<double> filter(2.0, 3.0, B, C);

        plot(
            "unit tests/outputs/test_math_filter_mitchellfilter2.gnuplot",
            "Mitchell Filter, B=" + pretty_scalar(B, 1) + ", C=" + pretty_scalar(C, 1),
            filter);
    }
}

TEST_SUITE(Foundation_Math_Filter_LanczosFilter2)
{
    const double Tau = 3.0;

    TEST_CASE(Evaluate_PointsOnDomainBorder_ReturnsZero)
    {
        const LanczosFilter2<double> filter(2.0, 3.0, Tau);

        EXPECT_TRUE(is_zero_on_domain_border(filter));
    }

    TEST_CASE(Plot)
    {
        const LanczosFilter2<double> filter(2.0, 3.0, Tau);

        plot(
            "unit tests/outputs/test_math_filter_lanczosfilter2.gnuplot",
            "Lanczos Filter, tau=" + pretty_scalar(Tau, 1),
            filter);
    }
}

TEST_SUITE(Foundation_Math_Filter_BlackmanHarrisFilter2)
{
    TEST_CASE(Evaluate_PointsOnDomainBorder_ReturnsZero)
    {
        const BlackmanHarrisFilter2<double> filter(2.0, 3.0);

        EXPECT_TRUE(is_zero_on_domain_border(filter));
    }

    TEST_CASE(Plot)
    {
        const BlackmanHarrisFilter2<double> filter(2.0, 3.0);

        plot(
            "unit tests/outputs/test_math_filter_blackmanharrisfilter2.gnuplot",
            "Blackman-Harris Filter",
            filter);
    }
}
