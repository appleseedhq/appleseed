
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2016 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_BSSRDF_SSS_H
#define APPLESEED_RENDERER_MODELING_BSSRDF_SSS_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace renderer
{

//
// BSSRDF reparameterization.
//
// References:
//
//   [1] A Rapid Hierarchical Rendering Technique for Translucent Materials
//       Henrik Wann Jensen, Juan Buhler
//       http://graphics.ucsd.edu/~henrik/papers/fast_bssrdf/fast_bssrdf.pdf
//
//   [2] Texture mapping for the Better Dipole model
//       Christophe Hery
//       http://graphics.pixar.com/library/TexturingBetterDipole/paper.pdf
//

// Compute Rd (integral of the diffusion profile R) given the reduced albedo alpha'
// using the reparameterization of the standard dipole model.
class ComputeRdStandardDipole
{
  public:
    explicit ComputeRdStandardDipole(const float eta);

    float operator()(const float alpha_prime) const;

  private:
    float m_a;
};

// Compute Rd (integral of the diffusion profile R) given the reduced albedo alpha'
// using the reparameterization of the better dipole model.
class ComputeRdBetterDipole
{
  public:
    explicit ComputeRdBetterDipole(const float eta);

    float operator()(const float alpha_prime) const;

  private:
    const float m_two_c1;
    const float m_three_c2;
};

// Numerically solve for the reduced albedo alpha' given Rd.
template <typename ComputeRdFun>
float compute_alpha_prime(
    const ComputeRdFun  rd_fun,
    const float         rd);

float diffusion_coefficient(
    const float         sigma_a,
    const float         sigma_t);

float diffuse_mean_free_path(
    const float         sigma_a,
    const float         sigma_t);

float reduced_extinction_coefficient(
    const float         dmfp,
    const float         alpha_prime);

float effective_extinction_coefficient(
    const float         sigma_a,
    const float         sigma_s,
    const float         anisotropy);

void effective_extinction_coefficient(
    const Spectrum&     sigma_a,
    const Spectrum&     sigma_s,
    const float         anisotropy,
    Spectrum&           sigma_tr);

// rd and dmfp must have the same size (both RGB or both spectral).
template <typename ComputeRdFun>
void compute_absorption_and_scattering_dmfp(
    const ComputeRdFun  rd_fun,
    const Spectrum&     rd,                     // diffuse surface reflectance
    const Spectrum&     dmfp,                   // diffuse mean free path
    const float         g,                      // anisotropy
    Spectrum&           sigma_a,                // absorption coefficient
    Spectrum&           sigma_s);               // scattering coefficient

// rd and mfp must have the same size (both RGB or both spectral).
template <typename ComputeRdFun>
void compute_absorption_and_scattering_mfp(
    const ComputeRdFun  rd_fun,
    const Spectrum&     rd,                     // diffuse surface reflectance
    const Spectrum&     mfp,                    // mean free path
    Spectrum&           sigma_a,                // absorption coefficient
    Spectrum&           sigma_s);               // scattering coefficient


//
// Gaussian diffusion profile.
//

float gaussian_profile(
    const float         r,
    const float         v,
    const float         r_integral_threshold);

float gaussian_profile_sample(
    const float         u,
    const float         v,
    const float         rmax2);

float gaussian_profile_pdf(
    const float         r,
    const float         v,
    const float         r_integral_threshold);


//
// Dipole diffusion profile.
//

float dipole_max_radius(const float sigma_tr);


//
// Normalized diffusion profile.
//
// Reference:
//
//   Approximate Reflectance Profiles for Efficient Subsurface Scattering
//   Per H. Christensen, Brent Burley
//   http://graphics.pixar.com/library/ApproxBSSRDF/paper.pdf
//

// Compute the scaling factor s for the searchlight configuration with dmfp parameterization.
float normalized_diffusion_s_dmfp(
    const float         a);                     // surface albedo

// Compute the scaling factor s for the searchlight configuration with mfp parameterization.
float normalized_diffusion_s_mfp(
    const float         a);                     // surface albedo

// Evaluate the diffuse reflectance profile R(r).
// The d parameter shapes the height and width of the curve and can be set
// based on artistic preference or determined based on physical parameters.
float normalized_diffusion_profile(
    const float         r,                      // radius
    const float         d);                     // curve shape
float normalized_diffusion_profile(
    const float         r,                      // radius
    const float         l,                      // mean free path length or diffuse mean free path length
    const float         s,                      // scaling factor
    const float         a);                     // surface albedo

// Sample the function r * R(r).
float normalized_diffusion_sample(
    const float         u,                      // uniform random sample in [0,1)
    const float         l,                      // mean free path length or diffuse mean free path length
    const float         s,                      // scaling factor
    const float         eps = 0.0001f,          // root precision
    const size_t        max_iterations = 10);   // max root refinement iterations

// Evaluate the cumulative distribution function of r * R(r).
float normalized_diffusion_cdf(
    const float         r,                      // radius
    const float         d);                     // curve shape
float normalized_diffusion_cdf(
    const float         r,                      // radius
    const float         l,                      // mean free path length or diffuse mean free path length
    const float         s);                     // scaling factor

// Evaluate the probability density of a given sample.
float normalized_diffusion_pdf(
    const float         r,                      // radius
    const float         d);                     // curve shape
float normalized_diffusion_pdf(
    const float         r,                      // radius
    const float         l,                      // mean free path length or diffuse mean free path length
    const float         s);                     // scaling factor

// Return the radius r at which we consider R(r) zero.
float normalized_diffusion_max_radius(
    const float         d);                     // curve shape
float normalized_diffusion_max_radius(
    const float         l,                      // mean free path length or diffuse mean free path length
    const float         s);                     // scaling factor


//
// BSSRDF reparameterization implementation.
//

template <typename ComputeRdFun>
inline float compute_alpha_prime(
    const ComputeRdFun  rd_fun,
    const float         rd)
{
    float x0 = 0.0f, x1 = 1.0f;

    // Simple bisection.
    for (size_t i = 0; i < 20; ++i)
    {
        const float xmid = 0.5f * (x0 + x1);
        const float x = rd_fun(xmid);
        x < rd ? x0 = xmid : x1 = xmid;
    }

    return 0.5f * (x0 + x1);
}

template <typename ComputeRdFun>
void compute_absorption_and_scattering_dmfp(
    const ComputeRdFun  rd_fun,
    const Spectrum&     rd,
    const Spectrum&     dmfp,
    const float         g,
    Spectrum&           sigma_a,
    Spectrum&           sigma_s)
{
    assert(rd.size() == dmfp.size());
    assert(g > -1.0f);
    assert(g < 1.0f);

    sigma_a.resize(rd.size());
    sigma_s.resize(rd.size());

    const float rcp_g_complement = 1.0f / (1.0f - g);

    for (size_t i = 0, e = rd.size(); i < e; ++i)
    {
        assert(rd[i] > 0.0f);
        assert(rd[i] < 1.0f);

        // Find alpha' by numerically inverting Rd(alpha').
        const float alpha_prime = compute_alpha_prime(rd_fun, rd[i]);
        assert(alpha_prime > 0.0f);
        assert(alpha_prime < 1.0f);

        // Compute reduced extinction coefficient.
        const float sigma_t_prime =
            reduced_extinction_coefficient(dmfp[i], alpha_prime);

        // Compute scattering coefficient.
        const float sigma_s_prime = alpha_prime * sigma_t_prime;
        sigma_s[i] = static_cast<float>(sigma_s_prime * rcp_g_complement);

        // Compute absorption coefficient.
        sigma_a[i] = static_cast<float>(sigma_t_prime - sigma_s_prime);
    }
}

template <typename ComputeRdFun>
void compute_absorption_and_scattering_mfp(
    const ComputeRdFun  rd_fun,
    const Spectrum&     rd,
    const Spectrum&     mfp,
    Spectrum&           sigma_a,
    Spectrum&           sigma_s)
{
    assert(rd.size() == mfp.size());

    sigma_a.resize(rd.size());
    sigma_s.resize(rd.size());

    for (size_t i = 0, e = rd.size(); i < e; ++i)
    {
        assert(rd[i] > 0.0f);
        assert(rd[i] < 1.0f);
        assert(mfp[i] > 0.0f);

        // Find alpha by numerically inverting Rd(alpha).
        const float alpha = compute_alpha_prime(rd_fun, rd[i]);
        assert(alpha > 0.0f);
        assert(alpha < 1.0f);

        const float sigma_t = 1.0f / mfp[i];

        // Compute scattering coefficient.
        sigma_s[i] = static_cast<float>(alpha * sigma_t);

        // Compute absorption coefficient.
        sigma_a[i] = static_cast<float>(sigma_t - sigma_s[i]);
    }
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSSRDF_SSS_H
