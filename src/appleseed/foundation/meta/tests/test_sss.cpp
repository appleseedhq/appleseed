
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
#include "foundation/math/fresnel.h"
#include "foundation/math/sss.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_SSS)
{
    static double rd_alpha_prime_roundtrip(
        const double rd,
        const double eta)
    {
        const double c1 = fresnel_moment_c1(eta);
        const double c2 = fresnel_moment_c2(eta);

        const double alpha_prime = compute_alpha_prime(rd, c1, c2);
        return compute_rd(alpha_prime, 2.0 * c1, 3.0 * c2);
    }

    TEST_CASE(Rd_AlphaPrime_Roundtrip)
    {
        EXPECT_FEQ_EPS(0.1, rd_alpha_prime_roundtrip(0.1, 1.3), 0.001);
        EXPECT_FEQ_EPS(0.2, rd_alpha_prime_roundtrip(0.2, 1.2), 0.001);
        EXPECT_FEQ_EPS(0.4, rd_alpha_prime_roundtrip(0.4, 1.3), 0.001);
        EXPECT_FEQ_EPS(0.6, rd_alpha_prime_roundtrip(0.6, 1.4), 0.001);
        EXPECT_FEQ_EPS(0.8, rd_alpha_prime_roundtrip(0.8, 1.3), 0.001);
        EXPECT_FEQ_EPS(1.0, rd_alpha_prime_roundtrip(1.0, 1.5), 0.001);
    }


    TEST_CASE(NormalizedDiffusionCdfPdf)
    {
        const size_t max_iters = 50;
        const double ndiff_step = 0.0001;

        MersenneTwister rng;

        for (size_t i = 0; i < max_iters; ++i)
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

    TEST_CASE(NormalizedDiffusionSample)
    {
        const size_t max_iters = 50;
        MersenneTwister rng;

        for (size_t i = 0; i < max_iters; ++i)
        {
            const double a = rand_double1(rng);
            const double l = rand_double1(rng, 0.001, 10.0);
            const double s = normalized_diffusion_s(a);
            const double e = rand_double1(rng);
            const double r = normalized_diffusion_sample(s, l, e);
            EXPECT_FEQ_EPS(e, normalized_diffusion_cdf(r, s, l), 0.0001);
        }
    }
}
