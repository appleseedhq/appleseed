
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
#include "foundation/math/fresnel.h"
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
// BSSRDF reparameterization implementation.
//

ComputeRd::ComputeRd(const double eta)
{
    const double rcp_eta = 1.0 / eta;
    const double rcp_eta2 = square(rcp_eta);

    // Compute the the average diffuse Fresnel reflectance.
    // The approximation comes from:
    //   Towards Realistic Image Synthesis of Scattering Materials
    //   Donner. C 2006, page 41, eq 5.27
    const double fdr =
        eta < 1
            ? -0.4933 + (0.7099 * rcp_eta) - (0.3319 * rcp_eta2) + (0.0636 * rcp_eta * rcp_eta2)
            : (-1.4933 * rcp_eta2) + (0.7099 * rcp_eta) + 0.6681 + (0.0636 * eta);

    // eq 5.29
    m_A = (1.0 + fdr) / (1.0 - fdr);
}

double ComputeRd::operator()(const double alpha_prime) const
{
    // [1] eq. 15.
    const double sqrt_3ap = sqrt(3.0 * (1.0 - alpha_prime));
    return (0.5 * alpha_prime) * (1.0 + exp(-1.25 * m_A * sqrt_3ap)) * exp(-sqrt_3ap);
}

ComputeRdBetterDipole::ComputeRdBetterDipole(const double eta)
    : m_two_c1(fresnel_moment_two_c1(eta))
    , m_three_c2(fresnel_moment_three_c2(eta))
{
}

double ComputeRdBetterDipole::operator()(const double alpha_prime) const
{
    const double cphi = 0.25 * (1.0 - m_two_c1);
    const double ce = 0.5 * (1.0 - m_three_c2);
    const double four_a = (1.0 + m_three_c2) / cphi;
    const double mu_tr_d = sqrt((1.0 - alpha_prime) * (2.0 - alpha_prime) * (1.0 / 3.0));
    const double myexp = exp(-four_a * mu_tr_d);
    return 0.5 * square(alpha_prime)
               * exp(-sqrt(3.0 * (1.0 - alpha_prime) / (2.0 - alpha_prime)))
               * (ce * (1.0 + myexp) + cphi / mu_tr_d * (1.0 - myexp));
}

double diffusion_coefficient(const double sigma_a, const double sigma_t)
{
    return (sigma_t + sigma_a) / (3.0 * square(sigma_t));
}

double diffuse_mean_free_path(const double sigma_a, const double sigma_t)
{
    const double D = diffusion_coefficient(sigma_a, sigma_t);
    return 1.0 / sqrt(sigma_a / D);
}

double reduced_extinction_coefficient(
    const double diffuse_mean_free_path,
    const double alpha_prime)
{
    return 1.0 / (sqrt(3.0 * (1.0 - alpha_prime)) * diffuse_mean_free_path);
}

//
// Normalized diffusion.
//

double normalized_diffusion_s(
    const double    a)
{
    // Equation 8.
    return 3.5 + 100.0 * square(square(a - 0.33));
}

double normalized_diffusion_r(
    const double    r,
    const double    d)
{
    // Equation 2.
    return (exp(-r / d) + exp(-r / (3.0 * d))) / (8.0 * Pi * d * r);
}

double normalized_diffusion_r(
    const double    r,
    const double    l,
    const double    s,
    const double    a)
{
    // Equation 3.
    return a * normalized_diffusion_r(r, l / s);
}

double normalized_diffusion_cdf(
    const double    r,
    const double    d)
{
    // Equation 11.
    return 1.0 - 0.25 * exp(-r / d) - 0.75 * exp(-r / (3.0 * d));
}

double normalized_diffusion_cdf(
    const double    r,
    const double    l,
    const double    s)
{
    return normalized_diffusion_cdf(r, l / s);
}

double normalized_diffusion_pdf(
    const double    r,
    const double    d)
{
    return (exp(-r / d) + exp(-r / (3.0 * d))) / (4.0 * d);
}

double normalized_diffusion_pdf(
    const double    r,
    const double    l,
    const double    s)
{
    return normalized_diffusion_pdf(r, l / s);
}

double normalized_diffusion_sample(
    const double    u,
    const double    l,
    const double    s)
{
    // Heuristic.
    double x1 = 10.0 + (3.0 * l);

    if (normalized_diffusion_cdf(x1, l, s) < u)
        return x1;

    double x0 = 0.0;
    double xmid;

    // todo: we should be doing some newton steps,
    // instead of bisection...
    for (size_t i = 0; i < 50; ++i)
    {
        xmid = 0.5 * (x0 + x1);
        const double f = normalized_diffusion_cdf(xmid, l, s);
        f < u ? x0 = xmid : x1 = xmid;
    }

    return xmid;
}

}   // namespace renderer
