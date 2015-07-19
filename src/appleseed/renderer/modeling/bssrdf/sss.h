
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

namespace renderer
{

//
// Reference:
//
//   Texture mapping for the Better Dipole model
//   Christophe Hery
//   http://graphics.pixar.com/library/TexturingBetterDipole/paper.pdf
//

// Compute Rd (integral of the diffusion profile R) given the reduced albedo alpha'.
double compute_rd(
    const double    alpha_prime,
    const double    two_c1,
    const double    three_c2);

// Numerically solve for the reduced albedo alpha' given Rd.
double compute_alpha_prime(
    const double    rd,
    const double    two_c1,
    const double    three_c2);


//
// Reference:
//
//   Approximate Reflectance Profiles for Efficient Subsurface Scattering
//   Per H. Christensen, Brent Burley
//   http://graphics.pixar.com/library/ApproxBSSRDF/paper.pdf
//

// Compute the scaling factor s for the searchlight configuration with dmfp parameterization.
double normalized_diffusion_s(
    const double    a);                 // surface albedo

// Evaluate the diffuse reflectance profile R(r).
// The d parameter shapes the height and width of the curve and can be set
// based on artistic preference or determined based on physical parameters.
double normalized_diffusion_r(
    const double    r,                  // distance
    const double    d);                 // curve shape
double normalized_diffusion_r(
    const double    r,                  // distance
    const double    l,                  // mean free path length or diffuse mean free path length
    const double    s,                  // scaling factor
    const double    a);                 // surface albedo

// Evaluate the cumulative distribution function of R(r) * (2 * Pi * r).
double normalized_diffusion_cdf(
    const double    r,                  // distance
    const double    d);                 // curve shape
double normalized_diffusion_cdf(
    const double    r,                  // distance
    const double    l,                  // mean free path length or diffuse mean free path length
    const double    s);                 // scaling factor

// Evaluate the probability density of a given sample.
double normalized_diffusion_pdf(
    const double    r,                  // distance
    const double    d);                 // curve shape
double normalized_diffusion_pdf(
    const double    r,                  // distance
    const double    l,                  // mean free path length or diffuse mean free path length
    const double    s);                 // scaling factor

// Sample the function R(r) * (2 * Pi * r).
double normalized_diffusion_sample(
    const double    u,                  // uniform random sample in [0,1)
    const double    l,                  // mean free path length or diffuse mean free path length
    const double    s);                 // scaling factor

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSSRDF_SSS_H
