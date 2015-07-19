
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"
#include "foundation/math/sss.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_SSS)
{
    double rd_alpha_prime_roundtrip(
        const double rd,
        const double eta)
    {
        const double two_c1 = fresnel_moment_two_c1(eta);
        const double three_c2 = fresnel_moment_three_c2(eta);

        const double alpha_prime = compute_alpha_prime(rd, two_c1, three_c2);
        return compute_rd(alpha_prime, two_c1, three_c2);
    }

    TEST_CASE(Rd_AlphaPrime_Roundtrip)
    {
        EXPECT_FEQ_EPS(0.0, rd_alpha_prime_roundtrip(0.0, 1.6), 0.001);
        EXPECT_FEQ_EPS(0.1, rd_alpha_prime_roundtrip(0.1, 1.3), 0.001);
        EXPECT_FEQ_EPS(0.2, rd_alpha_prime_roundtrip(0.2, 1.2), 0.001);
        EXPECT_FEQ_EPS(0.4, rd_alpha_prime_roundtrip(0.4, 1.3), 0.001);
        EXPECT_FEQ_EPS(0.6, rd_alpha_prime_roundtrip(0.6, 1.4), 0.001);
        EXPECT_FEQ_EPS(0.8, rd_alpha_prime_roundtrip(0.8, 1.3), 0.001);
        EXPECT_FEQ_EPS(1.0, rd_alpha_prime_roundtrip(1.0, 1.5), 0.001);
    }

    const double NormalizedDiffusionTestEps = 0.0001;

    TEST_CASE(NormalizedDiffusionA)
    {
        static const double Result[] =
        {
            4.68592, 4.11466, 3.77984, 3.60498, 3.52856, 3.5041, 3.50008,
            3.50002, 3.5024, 3.52074, 3.58352, 3.73426, 4.03144, 4.54858,
            5.37416, 6.6117, 8.37968, 10.8116, 14.056, 18.2763, 23.6511
        };

        for (size_t i = 0, e = countof(Result); i < e; ++i)
        {
            const double s = normalized_diffusion_s(static_cast<double>(i) * 0.05);
            EXPECT_FEQ_EPS(Result[i], s, NormalizedDiffusionTestEps);
        }
    }

    TEST_CASE(NormalizedDiffusionR)
    {
        static const double Result[] =
        {
            2.53511, 0.674967, 0.327967, 0.192204, 0.124137, 0.0852575,
            0.0611367, 0.0452741, 0.0343737, 0.0266197, 0.0209473, 0.0167009,
            0.0134603, 0.0109471, 0.00897108, 0.00739936, 0.00613676, 0.00511384,
            0.00427902, 0.0035934, 0.00302721
        };

        for (size_t i = 0, e = countof(Result); i < e; ++i)
        {
            const double r =
                normalized_diffusion_r(
                    static_cast<double>(i) * 0.1 + 0.05,
                    1.0,
                    3.583521,
                    0.5);

            EXPECT_FEQ_EPS(Result[i], r, NormalizedDiffusionTestEps);
        }
    }

    TEST_CASE(NormalizedDiffusionCdf)
    {
        static const double Result[] =
        {
            0.282838, 0.598244, 0.760091, 0.85267, 0.908478, 0.942885, 0.964293,
            0.97766, 0.98602, 0.99125, 0.994523, 0.996572, 0.997854, 0.998657,
            0.999159, 0.999474, 0.999671, 0.999794, 0.999871, 0.999919, 0.999949,
            0.999968, 0.99998, 0.999988, 0.999992, 0.999995, 0.999997, 0.999998,
            0.999999, 0.999999, 1
        };

        for (size_t i = 0, e = countof(Result); i < e; ++i)
        {
            const double cdf =
                normalized_diffusion_cdf(
                    static_cast<double>(i) * 0.1 + 0.05,
                    14.056001,
                    1.0);

            EXPECT_FEQ_EPS(Result[i], cdf, NormalizedDiffusionTestEps);
        }
    }

    /*
    TEST_CASE(NormalizedDiffusionCdfPdf)
    {
        const double ndiff_step = 0.0001;

        MersenneTwister rng;

        for (size_t i = 0; i < 50; ++i)
        {
            const double a = rand_double1(rng);
            const double l = rand_double1(rng, 0.0001, 10.0);
            const double s = normalized_diffusion_s(a);
            const double r = rand_double1(rng, 0.0001, 20.0);

            const double pdf = normalized_diffusion_pdf(r, s, l);

            const double pdf_ndiff =
                (normalized_diffusion_cdf(r + ndiff_step, s, l) -
                 normalized_diffusion_cdf(r, s, l)) / ndiff_step;

            EXPECT_FEQ_EPS(pdf, pdf_ndiff, ndiff_step);
        }
    }
    */

    TEST_CASE(NormalizedDiffusionSample)
    {
        MersenneTwister rng;

        for (size_t i = 0; i < 50; ++i)
        {
            const double a = rand_double1(rng);
            const double l = rand_double1(rng, 0.001, 10.0);
            const double s = normalized_diffusion_s(a);
            const double e = rand_double1(rng);
            const double r = normalized_diffusion_sample(s, l, e);
            EXPECT_FEQ_EPS(e, normalized_diffusion_cdf(r, s, l), NormalizedDiffusionTestEps);
        }
    }

    TEST_CASE(PlotNormalizedDiffusionR)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Searchlight with dmfp");
        plotfile.set_xlabel("r");
        plotfile.set_ylabel("r R(r)");
        plotfile.set_logscale_y();

        for (size_t i = 9; i >= 1; --i)
        {
            const double a = static_cast<double>(i) / 10.0;
            const double s = normalized_diffusion_s(a);

            const size_t N = 1000;
            vector<Vector2d> points;

            for (size_t j = 0; j < N; ++j)
            {
                const double r = fit<size_t, double>(j, 0, N - 1, 0.0, 8.0);
                const double y = r * normalized_diffusion_r(r, 1.0, s, a);
                points.push_back(Vector2d(r, y));
            }

            static const char* Colors[9] =
            {
                "gray",
                "orange",
                "black",
                "brown",
                "cyan",
                "magenta",
                "blue",
                "green",
                "red"
            };

            plotfile
                .new_plot()
                .set_points(points)
                .set_title("A = " + to_string(a))
                .set_color(Colors[i - 1]);
        }

        plotfile.write("unit tests/outputs/test_sss_normalized_diffusion_r.gnuplot");
    }
}
