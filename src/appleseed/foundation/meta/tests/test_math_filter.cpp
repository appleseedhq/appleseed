
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
#include "foundation/math/filter.h"
#include "foundation/math/scalar.h"
#include "foundation/string/string.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

using namespace foundation;

namespace
{
    template <typename Filter1>
    class SeparableFilter2
      : public Filter2<typename Filter1::ValueType>
    {
      public:
        typedef typename Filter1::ValueType ValueType;

        SeparableFilter2(const Filter1& filter_x, const Filter1& filter_y)
          : Filter2<ValueType>(filter_x.get_radius(), filter_y.get_radius())
          , m_filter_x(filter_x)
          , m_filter_y(filter_y)
        {
        }

        ValueType evaluate(const ValueType x, const ValueType y) const override
        {
            return m_filter_x.evaluate(x) * m_filter_y.evaluate(y);
        }

      private:
        const Filter1 m_filter_x;
        const Filter1 m_filter_y;
    };

    template <typename Filter2>
    bool is_zero_on_domain_border(const Filter2& filter)
    {
        typedef typename Filter2::ValueType T;

        const T Eps = T(1.0e-6);

        return
            fz(filter.evaluate(-filter.get_xradius(), -filter.get_yradius()), Eps) &&
            fz(filter.evaluate(               T(0.0), -filter.get_yradius()), Eps) &&
            fz(filter.evaluate(+filter.get_xradius(), -filter.get_yradius()), Eps) &&
            fz(filter.evaluate(+filter.get_xradius(),                T(0.0)), Eps) &&
            fz(filter.evaluate(+filter.get_xradius(), +filter.get_yradius()), Eps) &&
            fz(filter.evaluate(               T(0.0), +filter.get_yradius()), Eps) &&
            fz(filter.evaluate(-filter.get_xradius(), +filter.get_yradius()), Eps) &&
            fz(filter.evaluate(-filter.get_xradius(),                T(0.0)), Eps);
    }

    template <typename Filter2>
    std::vector<Vector2d> make_points(const Filter2& filter)
    {
        typedef typename Filter2::ValueType T;

        const T r = filter.get_xradius();

        const size_t PointCount = 256;
        std::vector<Vector2d> points(PointCount);

        for (size_t i = 0; i < PointCount; ++i)
        {
            const T x = fit<size_t, T>(i, 0, PointCount - 1, -r - T(1.0), r + T(1.0));
            const T y = x < -r || x > r ? T(0.0) : filter.evaluate(x, T(0.0));
            points[i] = Vector2d(x, y);
        }

        return points;
    }

    template <typename Filter2>
    void plot(
        const std::string&       filepath,
        const std::string&       plot_title,
        const Filter2&           filter)
    {
        GnuplotFile plotfile;
        plotfile.set_title(plot_title + ", radius=" + pretty_scalar(filter.get_xradius(), 1));
        plotfile.new_plot().set_points(make_points(filter));
        plotfile.write(filepath);
    }

    template <typename Filter2T, typename Filter2U>
    void plot(
        const std::string&       filepath,
        const std::string&       plot_title,
        const std::string&       filter1_name,
        const Filter2T&          filter1,
        const std::string&       filter2_name,
        const Filter2U&          filter2)
    {
        GnuplotFile plotfile;
        plotfile.set_title(plot_title + ", radius=" + pretty_scalar(filter1.get_xradius(), 1));
        plotfile.new_plot().set_title(filter1_name).set_points(make_points(filter1));
        plotfile.new_plot().set_title(filter2_name).set_points(make_points(filter2));
        plotfile.write(filepath);
    }
}

TEST_SUITE(Foundation_Math_Filter_BoxFilter1)
{
    TEST_CASE(TestPropertyGetters)
    {
        const BoxFilter1<double> filter(2.0);

        EXPECT_EQ(2.0, filter.get_radius());
    }

    TEST_CASE(Plot)
    {
        const SeparableFilter2<BoxFilter1<double>> filter(
            BoxFilter1<double>(2.0),
            BoxFilter1<double>(3.0));

        plot(
            "unit tests/outputs/test_math_filter_boxfilter1.gnuplot",
            "Separable Box Reconstruction Filter",
            filter);
    }
}

TEST_SUITE(Foundation_Math_Filter_TriangleFilter1)
{
    TEST_CASE(Evaluate_PointsOnDomainBorder_ReturnsZero)
    {
        const SeparableFilter2<TriangleFilter1<double>> filter(
            TriangleFilter1<double>(2.0),
            TriangleFilter1<double>(3.0));

        EXPECT_TRUE(is_zero_on_domain_border(filter));
    }

    TEST_CASE(Plot)
    {
        const SeparableFilter2<TriangleFilter1<double>> filter(
            TriangleFilter1<double>(2.0),
            TriangleFilter1<double>(3.0));

        plot(
            "unit tests/outputs/test_math_filter_trianglefilter1.gnuplot",
            "Separable Triangle Reconstruction Filter",
            filter);
    }
}

TEST_SUITE(Foundation_Math_Filter_GaussianFilter1)
{
    const double Alpha = 4.0;

    TEST_CASE(Evaluate_PointsOnDomainBorder_ReturnsZero)
    {
        const SeparableFilter2<GaussianFilter1<double>> filter(
            GaussianFilter1<double>(2.0, Alpha),
            GaussianFilter1<double>(3.0, Alpha));

        EXPECT_TRUE(is_zero_on_domain_border(filter));
    }

    TEST_CASE(Plot)
    {
        const SeparableFilter2<GaussianFilter1<double>> filter(
            GaussianFilter1<double>(2.0, Alpha),
            GaussianFilter1<double>(3.0, Alpha));

        plot(
            "unit tests/outputs/test_math_filter_gaussianfilter1.gnuplot",
            "Separable Gaussian Reconstruction Filter",
            filter);
    }
}

TEST_SUITE(Foundation_Math_Filter_BlackmanHarrisFilter1)
{
    TEST_CASE(Evaluate_PointsOnDomainBorder_ReturnsZero)
    {
        const SeparableFilter2<BlackmanHarrisFilter1<double>> filter(
            BlackmanHarrisFilter1<double>(2.0),
            BlackmanHarrisFilter1<double>(3.0));

        EXPECT_TRUE(is_zero_on_domain_border(filter));
    }

    TEST_CASE(Plot)
    {
        const SeparableFilter2<BlackmanHarrisFilter1<double>> filter(
            BlackmanHarrisFilter1<double>(2.0),
            BlackmanHarrisFilter1<double>(3.0));

        plot(
            "unit tests/outputs/test_math_filter_blackmanfilter1.gnuplot",
            "Separable Blackman-Harris Reconstruction Filter",
            filter);
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
            "Box Reconstruction Filter",
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
            "Triangle Reconstruction Filter",
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
        const GaussianFilter2<double> accurate_filter(2.0, 3.0, Alpha);
        const FastGaussianFilter2<double> fast_filter(2.0, 3.0, Alpha);

        plot(
            "unit tests/outputs/test_math_filter_gaussianfilter2.gnuplot",
            "Gaussian Reconstruction Filter, alpha=" + pretty_scalar(Alpha, 1),
            "Accurate Variant", accurate_filter,
            "Fast Variant", fast_filter);
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
            "Mitchell Reconstruction Filter, B=" + pretty_scalar(B, 1) + ", C=" + pretty_scalar(C, 1),
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
            "Lanczos Reconstruction Filter, tau=" + pretty_scalar(Tau, 1),
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
        const BlackmanHarrisFilter2<double> accurate_filter(2.0, 3.0);
        const FastBlackmanHarrisFilter2<float> fast_filter(2.0f, 3.0f);

        plot(
            "unit tests/outputs/test_math_filter_blackmanharrisfilter2.gnuplot",
            "Blackman-Harris Reconstruction Filter",
            "Accurate Variant", accurate_filter,
            "Fast Variant", fast_filter);
    }
}
