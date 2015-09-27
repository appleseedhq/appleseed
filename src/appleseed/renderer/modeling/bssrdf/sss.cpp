
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
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/cdf.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <cmath>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// BSSRDF reparameterization implementation.
//

ComputeRdStandardDipole::ComputeRdStandardDipole(const double eta)
{
    const double fdr = fresnel_internal_diffuse_reflectance(eta);
    m_a = (1.0 + fdr) / (1.0 - fdr);
}

double ComputeRdStandardDipole::operator()(const double alpha_prime) const
{
    // [1] eq. 15.
    const double sqrt_3ap = sqrt(3.0 * (1.0 - alpha_prime));
    return (0.5 * alpha_prime) * (1.0 + exp(-(4.0 / 3.0) * m_a * sqrt_3ap)) * exp(-sqrt_3ap);
}

ComputeRdBetterDipole::ComputeRdBetterDipole(const double eta)
  : m_two_c1(fresnel_first_moment(eta))
  , m_three_c2(fresnel_second_moment(eta))
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

double diffusion_coefficient(
    const double    sigma_a,
    const double    sigma_t)
{
    assert(sigma_t > 0.0);

    return (sigma_t + sigma_a) / (3.0 * square(sigma_t));
}

double diffuse_mean_free_path(
    const double    sigma_a,
    const double    sigma_t)
{
    assert(sigma_a > 0.0);

    const double D = diffusion_coefficient(sigma_a, sigma_t);
    return 1.0 / sqrt(sigma_a / D);
}

double reduced_extinction_coefficient(
    const double    dmfp,
    const double    alpha_prime)
{
    assert(alpha_prime >= 0.0);
    assert(alpha_prime < 1.0);

    return 1.0 / (sqrt(3.0 * (1.0 - alpha_prime)) * dmfp);
}

double effective_extinction_coefficient(
    const double    sigma_a,
    const double    sigma_s,
    const double    anisotropy)
{
    const double sigma_s_prime = sigma_s * (1.0 - anisotropy);
    const double sigma_t_prime = sigma_s_prime + sigma_a;
    return sqrt(3.0 * sigma_a * sigma_t_prime);
}

void effective_extinction_coefficient(
    const Spectrum& sigma_a,
    const Spectrum& sigma_s,
    const double    anisotropy,
    Spectrum&       sigma_tr)
{
    assert(sigma_a.size() == sigma_s.size());

    sigma_tr.resize(sigma_a.size());

    for (size_t i = 0, e = sigma_tr.size(); i < e; ++i)
    {
        sigma_tr[i] =
            static_cast<float>(
                effective_extinction_coefficient(
                    static_cast<double>(sigma_a[i]),
                    static_cast<double>(sigma_s[i]),
                    anisotropy));
    }
}


//
// Gaussian diffusion profile implementation.
//

double gaussian_profile(
    const double    r,
    const double    v,
    const double    r_integral_threshold)
{
    return exp(-r * r / (2.0 * v)) / (TwoPi * v * r_integral_threshold);
}

double gaussian_profile_sample(
    const double    u,
    const double    v,
    const double    rmax2)
{
    return sqrt(-2.0 * v * log(1.0 - u * (1.0 - exp(-rmax2 / (2.0 * v)))));
}

double gaussian_profile_pdf(
    const double    r,
    const double    v,
    const double    r_integral_threshold)
{
    return exp(-r * r / (2.0 * v)) / (TwoPi * v * r_integral_threshold);
}


//
// Dipole diffusion profile implementation.
//

double dipole_max_radius(const double sigma_tr)
{
    // todo: some plots suggest that our estimate
    // for max radius is too conservative.
    // We could probably reduce it a bit.
    return -log(0.00001) / sigma_tr;
}


//
// Normalized diffusion profile implementation.
//

double normalized_diffusion_s(
    const double    a)
{
    // Equation 8.
    return 3.5 + 100.0 * square(square(a - 0.33));
}

double normalized_diffusion_profile(
    const double    r,
    const double    d)
{
    // Equation 2.
    return (exp(-r / d) + exp(-r / (3.0 * d))) / (8.0 * Pi * d * r);
}

double normalized_diffusion_profile(
    const double    r,
    const double    l,
    const double    s,
    const double    a)
{
    // Equation 3.
    return a * normalized_diffusion_profile(r, l / s);
}

namespace
{
    const size_t NDCDFTableSize = 128;
    const double NDCDFTableRmax = 55.0;
    const double NDCDFTableStep = NDCDFTableRmax / NDCDFTableSize;

    double nd_cdf_table[NDCDFTableSize];
    double nd_cdf_rmax;

    struct InitializeNDCDFTable
    {
        InitializeNDCDFTable()
        {
            for (size_t i = 0; i < NDCDFTableSize; ++i)
            {
                const double r = fit<size_t, double>(i, 0, NDCDFTableSize - 1, 0.0, NDCDFTableRmax);
                nd_cdf_table[i] = normalized_diffusion_cdf(r, 1.0);
            }

            // Save the real value of cdf(Rmax, 1).
            nd_cdf_rmax = nd_cdf_table[NDCDFTableSize - 1];

            // Make sure the last value is exactly 1.
            nd_cdf_table[NDCDFTableSize - 1] = 1.0;
        }
    };

    InitializeNDCDFTable initialize_nd_cdf_table;

    struct NDCDFFun
    {
        explicit NDCDFFun(const double d)
          : m_d(d)
        {
        }

        double operator()(const double r) const
        {
            return normalized_diffusion_cdf(r, m_d);
        }

        const double m_d;
    };

    struct NDPDFFun
    {
        explicit NDPDFFun(const double d)
          : m_d(d)
        {
        }

        double operator()(const double r) const
        {
            return normalized_diffusion_pdf(r, m_d);
        }

        const double m_d;
    };
}

double normalized_diffusion_sample(
    const double    u,
    const double    l,
    const double    s,
    const double    eps,
    const size_t    max_iterations)
{
    assert(u >= 0.0);
    assert(u < 1.0);

    const double d = l / s;

    // Handle the case where u is greater than the value we consider 1 in our CDF.
    if (u >= nd_cdf_rmax)
        return NDCDFTableRmax * d;

    // Use the CDF to find an initial interval for the root of cdf(r, 1) - u = 0.
    const size_t i = sample_cdf(nd_cdf_table, nd_cdf_table + NDCDFTableSize, u);
    assert(i > 0);
    assert(nd_cdf_table[i - 1] <= u);
    assert(nd_cdf_table[i] > u);

    // Transform the cdf(r, 1) interval to cdf(r, d) using the fact that cdf(r, d) == cdf(r/d, 1).
    const double rmin = fit<size_t, double>(i - 1, 0, NDCDFTableSize - 1, 0.0, NDCDFTableRmax) * d;
    const double rmax = fit<size_t, double>(i,     0, NDCDFTableSize - 1, 0.0, NDCDFTableRmax) * d;

    return invert_cdf_function(
        NDCDFFun(d),
        NDPDFFun(d),
        u,
        rmin,
        rmax,
        (rmax + rmin) * 0.5,
        eps,
        max_iterations);
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
    return r * TwoPi * normalized_diffusion_profile(r, d);
}

double normalized_diffusion_pdf(
    const double    r,
    const double    l,
    const double    s)
{
    return normalized_diffusion_pdf(r, l / s);
}

double normalized_diffusion_max_radius(
    const double    d)
{
    // todo: some plots suggest that our estimate
    // for max radius is too conservative.
    // We could probably reduce it a bit.
    return d * NDCDFTableRmax;
}

double normalized_diffusion_max_radius(
    const double    l,
    const double    s)
{
    return normalized_diffusion_max_radius(l / s);
}

}   // namespace renderer
