
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

// Interface header.
#include "sss.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Texture mapping for the better dipole.
//

double compute_rd(
    const double    alpha_prime,
    const double    two_c1,
    const double    three_c2)
{
    const double cphi = 0.25 * (1.0 - two_c1);
    const double ce = 0.5 * (1.0 - three_c2);
    const double four_a = (1.0 + three_c2) / cphi;
    const double mu_tr_d = sqrt((1.0 - alpha_prime) * (2.0 - alpha_prime) * (1.0 / 3.0));
    const double myexp = exp(-four_a * mu_tr_d);
    return 0.5 * square(alpha_prime)
               * exp(-sqrt(3.0 * (1.0 - alpha_prime) / (2.0 - alpha_prime)))
               * (ce * (1.0 + myexp) + cphi / mu_tr_d * (1.0 - myexp));
}

double compute_alpha_prime(
    const double    rd,
    const double    two_c1,
    const double    three_c2)
{
    if (rd == 0.0)
        return 0.0;

    double x0 = 0.0, x1 = 1.0, xmid;

    // For now simple bisection.
    // todo: switch to faster algorithm.
    for (size_t i = 0, iters = 50; i < iters; ++i)
    {
        xmid = 0.5 * (x0 + x1);
        const double f = compute_rd(xmid, two_c1, three_c2);
        f < rd ? x0 = xmid : x1 = xmid;
    }

    return xmid;
}


//
// Normalized diffusion.
//

double normalized_diffusion_s(const double a)
{
    const double a2 = square(a - 0.33);
    return 3.5 + 100.0 * square(a2);
}

double normalized_diffusion_r(
    const double    r,
    const double    ld,
    const double    s,
    const double    a)
{
    const double d = ld / s;
    const double num = exp(-r / d) + exp(-r / (3.0 * d));
    const double denom = 8.0 * Pi * d * r;
    return a * (num / denom);
}

double normalized_diffusion_cdf(
    const double    r,
    const double    s,
    const double    ld)
{
    const double d = ld / s;
    return 1.0 - (0.25 * exp(-r / d)) - (0.75 * exp(-r / (3.0 * d)));
}

double normalized_diffusion_pdf(
    const double    e,
    const double    s,
    const double    ld)
{
    const double d = ld / s;
    return (exp(-e / d) + exp(-e / (3.0 * d))) / (4.0 * d);
}

double normalized_diffusion_sample(
    const double    s,
    const double    ld,
    const double    e)
{
    // Heuristic.
    double x1 = 10.0 + (3.0 * ld);

    if (normalized_diffusion_cdf(x1, s, ld) < e)
        return x1;

    double x0 = 0.0;
    double xmid;

    // todo: we should be doing some newton steps,
    // instead of bisection...
    for (size_t i = 0; i < 50; ++i)
    {
        xmid = 0.5 * (x0 + x1);
        const double f = normalized_diffusion_cdf(xmid, s, ld);
        f < e ? x0 = xmid : x1 = xmid;
    }

    return xmid;
}

}   // namespace renderer
