
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/math/cdf.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <cmath>

using namespace foundation;

namespace renderer
{

//
// BSSRDF reparameterization functions implementation.
//

ComputeRdStandardDipole::ComputeRdStandardDipole(const float eta)
{
    const float fdr = fresnel_internal_diffuse_reflectance(eta);
    m_a = (1.0f + fdr) / (1.0f - fdr);
}

float ComputeRdStandardDipole::operator()(const float alpha_prime) const
{
    // [1] eq. 15.
    const float sqrt_3ap = std::sqrt(3.0f * (1.0f - alpha_prime));
    return (0.5f * alpha_prime) * (1.0f + std::exp(-(4.0f / 3.0f) * m_a * sqrt_3ap)) * std::exp(-sqrt_3ap);
}

ComputeRdBetterDipole::ComputeRdBetterDipole(const float eta)
  : m_two_c1(fresnel_first_moment_x2(eta))
  , m_three_c2(fresnel_second_moment_x3(eta))
{
}

float ComputeRdBetterDipole::operator()(const float alpha_prime) const
{
    const float cphi = 0.25f * (1.0f - m_two_c1);
    const float ce = 0.5f * (1.0f - m_three_c2);
    const float four_a = (1.0f + m_three_c2) / cphi;
    const float mu_tr_d = std::sqrt((1.0f - alpha_prime) * (2.0f - alpha_prime) * (1.0f / 3.0f));
    const float myexp = std::exp(-four_a * mu_tr_d);
    return 0.5f * square(alpha_prime)
                * std::exp(-sqrt(3.0f * (1.0f - alpha_prime) / (2.0f - alpha_prime)))
                * (ce * (1.0f + myexp) + cphi / mu_tr_d * (1.0f - myexp));
}

float diffusion_coefficient(
    const float     sigma_a,
    const float     sigma_t)
{
    assert(sigma_t > 0.0f);

    return (sigma_t + sigma_a) / (3.0f * square(sigma_t));
}

float diffuse_mean_free_path(
    const float     sigma_a,
    const float     sigma_t)
{
    assert(sigma_a > 0.0f);

    const float D = diffusion_coefficient(sigma_a, sigma_t);
    return 1.0f / std::sqrt(sigma_a / D);
}

float reduced_extinction_coefficient(
    const float     dmfp,
    const float     alpha_prime)
{
    assert(alpha_prime >= 0.0f);
    assert(alpha_prime < 1.0f);

    return 1.0f / (std::sqrt(3.0f * (1.0f - alpha_prime)) * dmfp);
}

float effective_extinction_coefficient(
    const float     sigma_a,
    const float     sigma_s,
    const float     anisotropy)
{
    const float sigma_s_prime = sigma_s * (1.0f - anisotropy);
    const float sigma_t_prime = sigma_s_prime + sigma_a;
    return std::sqrt(3.0f * sigma_a * sigma_t_prime);
}

void effective_extinction_coefficient(
    const Spectrum& sigma_a,
    const Spectrum& sigma_s,
    const float     anisotropy,
    Spectrum&       sigma_tr)
{
    for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
    {
        sigma_tr[i] =
            effective_extinction_coefficient(
                sigma_a[i],
                sigma_s[i],
                anisotropy);
    }
}


//
// Dipole diffusion profile implementation.
//

float dipole_max_radius(const float sigma_tr)
{
    return -log(0.001f) / sigma_tr;
}


//
// Normalized diffusion profile implementation.
//

float normalized_diffusion_s_dmfp(
    const float     a)
{
    // Equation 8.
    return 3.5f + 100.0f * square(square(a - 0.33f));
}

float normalized_diffusion_s_mfp(
    const float     a)
{
    // Equation 5.
    const float x = std::abs(a - 0.8f);
    return 1.85f - a + 7.0f * (x * x * x);
}

float normalized_diffusion_profile(
    const float     r,
    const float     d)
{
    // Equation 2.
    const float exp_r3 = std::exp(-r / (3.0f * d));
    return (cube(exp_r3) + exp_r3) / (8.0f * Pi<float>() * d * r);
}

float normalized_diffusion_profile(
    const float     r,
    const float     l,
    const float     s,
    const float     a)
{
    // Equation 3.
    return a * normalized_diffusion_profile(r, l / s);
}

namespace
{
    const size_t NDCDFTableSize = 32;
    const float NDCDFTableRmax = 35.0f;

    float g_nd_cdf_table[NDCDFTableSize];
    float g_nd_cdf_rmax;

    struct InitializeNDCDFTable
    {
        InitializeNDCDFTable()
        {
            for (size_t i = 0; i < NDCDFTableSize; ++i)
            {
                const float r = fit<size_t, float>(i, 0, NDCDFTableSize - 1, 0.0f, NDCDFTableRmax);
                g_nd_cdf_table[i] = normalized_diffusion_cdf(r, 1.0f);
            }

            // Save the real value of cdf(Rmax, 1).
            g_nd_cdf_rmax = g_nd_cdf_table[NDCDFTableSize - 1];

            // Make sure the last value is exactly 1.
            g_nd_cdf_table[NDCDFTableSize - 1] = 1.0f;
        }
    };

    InitializeNDCDFTable g_initialize_nd_cdf_table;

    struct NDCDFFun
    {
        const float m_d;

        explicit NDCDFFun(const float d)
          : m_d(d)
        {
        }

        float operator()(const float r) const
        {
            return normalized_diffusion_cdf(r, m_d);
        }
    };

    struct NDPDFFun
    {
        const float m_d;

        explicit NDPDFFun(const float d)
          : m_d(d)
        {
        }

        float operator()(const float r) const
        {
            return normalized_diffusion_pdf(r, m_d);
        }
    };
}

float normalized_diffusion_sample(
    const float     u,
    const float     l,
    const float     s,
    const float     eps,
    const size_t    max_iterations)
{
    assert(u >= 0.0f);
    assert(u < 1.0f);

    const float d = l / s;

    // Handle the case where u is greater than the value we consider 1 in our CDF.
    if (u >= g_nd_cdf_rmax)
        return NDCDFTableRmax * d;

    // Use the CDF to find an initial interval for the root of cdf(r, 1) - u = 0.
    const size_t i = sample_cdf(g_nd_cdf_table, g_nd_cdf_table + NDCDFTableSize, u);
    assert(i > 0);
    assert(g_nd_cdf_table[i - 1] <= u);
    assert(g_nd_cdf_table[i] > u);

    // Transform the cdf(r, 1) interval to cdf(r, d) using the fact that cdf(r, d) == cdf(r/d, 1).
    const float rmin = fit<size_t, float>(i - 1, 0, NDCDFTableSize - 1, 0.0f, NDCDFTableRmax) * d;
    const float rmax = fit<size_t, float>(i,     0, NDCDFTableSize - 1, 0.0f, NDCDFTableRmax) * d;

    return
        invert_cdf_function(
            NDCDFFun(d),
            NDPDFFun(d),
            u,
            rmin,
            rmax,
            (rmin + rmax) * 0.5f,
            eps,
            max_iterations);
}

float normalized_diffusion_cdf(
    const float     r,
    const float     d)
{
    // Equation 11.
    const float exp_r3 = std::exp(-r / (3.0f * d));
    return 1.0f - 0.25f * cube(exp_r3) - 0.75f * exp_r3;
}

float normalized_diffusion_cdf(
    const float     r,
    const float     l,
    const float     s)
{
    return normalized_diffusion_cdf(r, l / s);
}

float normalized_diffusion_pdf(
    const float     r,
    const float     d)
{
    return normalized_diffusion_profile(r, d);
}

float normalized_diffusion_pdf(
    const float     r,
    const float     l,
    const float     s)
{
    return normalized_diffusion_pdf(r, l / s);
}

float normalized_diffusion_max_radius(
    const float     d)
{
    // todo: some plots suggest that our estimate
    // for max radius is too conservative.
    // We could probably reduce it a bit.
    return d * NDCDFTableRmax;
}

float normalized_diffusion_max_radius(
    const float     l,
    const float     s)
{
    return normalized_diffusion_max_radius(l / s);
}

}   // namespace renderer
