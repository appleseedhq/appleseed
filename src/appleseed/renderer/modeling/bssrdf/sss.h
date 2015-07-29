
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

#ifndef APPLESEED_RENDERER_MODELING_BSSRDF_SSS_H
#define APPLESEED_RENDERER_MODELING_BSSRDF_SSS_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// Standard headers.
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

// Compute Rd (integral of the diffusion profile R) given the reduced albedo alpha'.
class ComputeRd
{
  public:
    explicit ComputeRd(const double eta);

    double operator()(const double alpha_prime) const;

  private:
    double m_a;
};

// Compute Rd (integral of the diffusion profile R) given the reduced albedo alpha'.
class ComputeRdBetterDipole
{
  public:
    explicit ComputeRdBetterDipole(const double eta);

    double operator()(const double alpha_prime) const;

  private:
    const double m_two_c1;
    const double m_three_c2;
};

// Numerically solve for the reduced albedo alpha' given Rd.
template <typename ComputeRdFun>
double compute_alpha_prime(ComputeRdFun f, const double rd);

double diffusion_coefficient(
    const double    sigma_a,
    const double    sigma_t);

double diffuse_mean_free_path(
    const double    sigma_a,
    const double    sigma_t);

double reduced_extinction_coefficient(
    const double    diffuse_mean_free_path,
    const double    alpha_prime);

void compute_absorption_and_scattering(
    const Spectrum& rd,                     // surface albedo
    const double    dmfp,                   // diffuse mean free path
    const double    eta,                    // relative index of refraction
    const double    g,                      // anisotropy
    Spectrum&       sigma_a,                // absorption coefficient
    Spectrum&       sigma_s);               // scattering coefficient


//
// Gaussian diffusion profile.
//

double gaussian_profile(
    const double    r,
    const double    v,
    const double    r_integral_threshold);

double gaussian_profile_sample(
    const double    u,
    const double    v,
    const double    rmax2);

double gaussian_profile_pdf(
    const double    r,
    const double    v,
    const double    r_integral_threshold);


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
double normalized_diffusion_s(
    const double    a);                     // surface albedo

// Evaluate the diffuse reflectance profile R(r).
// The d parameter shapes the height and width of the curve and can be set
// based on artistic preference or determined based on physical parameters.
double normalized_diffusion_profile(
    const double    r,                      // radius
    const double    d);                     // curve shape
double normalized_diffusion_profile(
    const double    r,                      // radius
    const double    l,                      // mean free path length or diffuse mean free path length
    const double    s,                      // scaling factor
    const double    a);                     // surface albedo

// Sample the function r * R(r).
double normalized_diffusion_sample(
    const double    u,                      // uniform random sample in [0,1)
    const double    l,                      // mean free path length or diffuse mean free path length
    const double    s,                      // scaling factor
    const double    eps = 0.0001,           // root precision
    const size_t    max_iterations = 10);   // max root refinement iterations

// Evaluate the cumulative distribution function of r * R(r).
double normalized_diffusion_cdf(
    const double    r,                      // radius
    const double    d);                     // curve shape
double normalized_diffusion_cdf(
    const double    r,                      // radius
    const double    l,                      // mean free path length or diffuse mean free path length
    const double    s);                     // scaling factor

// Evaluate the probability density of a given sample.
double normalized_diffusion_pdf(
    const double    r,                      // radius
    const double    d);                     // curve shape
double normalized_diffusion_pdf(
    const double    r,                      // radius
    const double    l,                      // mean free path length or diffuse mean free path length
    const double    s);                     // scaling factor

// Return the radius r at which we consider R(r) zero.
double normalized_diffusion_max_radius(
    const double    d);                     // curve shape
double normalized_diffusion_max_radius(
    const double    l,                      // mean free path length or diffuse mean free path length
    const double    s);                     // scaling factor


//
// Attenuation sampling.
//
// Reference:
//
//   Volumetric Path Tracing, Steve Marschner, section 3.
//   http://www.cs.cornell.edu/courses/cs6630/2012sp/notes/09volpath.pdf
//

// Sample a distance by importance sampling the attenuation.
double sample_attenuation(
    const double    sigma_t,
    const double    s);

double pdf_attenuation(
    const double    dist,
    const double    sigma_t);

double max_attenuation_distance(const double sigma_t);


//
// BSSRDF reparameterization implementation.
//

template <typename ComputeRdFun>
inline double compute_alpha_prime(const ComputeRdFun f, const double rd)
{
    if (rd == 0.0)
        return 0.0;

    double x0 = 0.0, x1 = 1.0, xmid;

    // For now simple bisection.
    // todo: switch to faster algorithm.
    for (std::size_t i = 0, iters = 50; i < iters; ++i)
    {
        xmid = 0.5 * (x0 + x1);
        const double x = f(xmid);
        x < rd ? x0 = xmid : x1 = xmid;
    }

    return xmid;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSSRDF_SSS_H
