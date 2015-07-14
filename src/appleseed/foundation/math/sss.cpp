
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
#include <cmath>

using namespace std;

namespace foundation
{

//
// Texture mapping for the better dipole.
//

double compute_rd(double alpha_prime, double two_c1, double three_c2)
{
    const double cphi = 0.25 * (1.0 - two_c1);
    const double ce = 0.5 * (1.0 - three_c2);
    const double four_a = (1.0 + three_c2) / cphi;
    const double mu_tr_d = sqrt((1.0 - alpha_prime) * (2.0 - alpha_prime) / 3.0);
    const double myexp = exp(-four_a * mu_tr_d);
    return 0.5 * square(alpha_prime)
               * exp(-sqrt(3.0 * (1.0 - alpha_prime) / (2.0 - alpha_prime)))
               * (ce * (1.0 + myexp) + cphi / mu_tr_d * (1.0 - myexp));
}

double compute_alpha_prime(double rd, double c1, double c2)
{
    const double c12 = 2.0 * c1;
    const double c23 = 3.0 * c2;

    double x0 = 0.0, x1 = 1.0, xmid;

    // For now simple bisection.
    // todo: switch to faster algorithm.
    for (size_t i = 0, iters = 50; i < iters; ++i)
    {
        xmid = 0.5 * (x0 + x1);
        const double f = compute_rd(xmid, c12, c23);
        f < rd ? x0 = xmid : x1 = xmid;
    }

    return xmid;
}

}   // namespace foundation
