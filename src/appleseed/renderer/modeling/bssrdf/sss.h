
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

double normalized_diffusion_s(
    const double    a);

double normalized_diffusion_r(
    const double    r,
    const double    ld,
    const double    s,
    const double    a);

double normalized_diffusion_cdf(
    const double    r,
    const double    s,
    const double    ld);

double normalized_diffusion_pdf(
    const double    e,
    const double    s,
    const double    ld);

double normalized_diffusion_sample(
    const double    s,
    const double    ld,
    const double    e);

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSSRDF_SSS_H
