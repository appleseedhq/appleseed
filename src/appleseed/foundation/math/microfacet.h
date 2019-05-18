
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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

#pragma once

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <algorithm>

namespace foundation
{

//
// Blinn-Phong Microfacet Distribution Function.
//
// References:
//
//   Physically Based Rendering, first edition, pp. 444-447 and 681-684
//
//   Microfacet Models for Refraction through Rough Surfaces
//   http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
//
//   Importance Sampling Microfacet-Based BSDFs using the Distribution of Visible Normals.
//   https://hal.inria.fr/hal-00996995/en
//

class BlinnMDF
{
  public:
    static float D(
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static float G(
        const Vector3f&     wi,
        const Vector3f&     wo,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static Vector3f sample(
        const Vector3f&     v,
        const Vector2f&     s,
        const float         alpha_x,
        const float         alpha_y);

    static float pdf(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);
};


//
// Anisotropic Beckmann Microfacet Distribution Function.
//
// References:
//
//   [1] http://en.wikipedia.org/wiki/Specular_highlight#Beckmann_distribution
//
//   [2] A Microfacet Based Coupled Specular-Matte BRDF Model with Importance Sampling
//       http://sirkan.iit.bme.hu/~szirmay/scook.pdf
//
//   [3] Microfacet Models for Refraction through Rough Surfaces
//       http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
//
//   [4] Understanding the Masking-Shadowing Function in Microfacet-Based BRDFs
//       http://hal.inria.fr/docs/00/96/78/44/PDF/RR-8468.pdf
//
//   [5] Importance Sampling Microfacet-Based BSDFs using the Distribution of Visible Normals.
//       https://hal.inria.fr/hal-00996995/en
//
//   [6] An Improved Visible Normal Sampling Routine for the Beckmann Distribution.
//       http://www.mitsuba-renderer.org/~wenzel/files/visnormal.pdf
//

class BeckmannMDF
{
  public:
    static float D(
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static float G(
        const Vector3f&     wi,
        const Vector3f&     wo,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static Vector3f sample(
        const Vector3f&     v,
        const Vector2f&     s,
        const float         alpha_x,
        const float         alpha_y);

    static Vector3f sample(
        const Vector2f&     s,
        const float         alpha);

    static float pdf(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

  private:
    static Vector2f sample_slope(
        const float         cos_theta,
        const Vector2f&     s);

    static float lambda(
        const Vector3f&     v,
        const float         alpha_x,
        const float         alpha_y);
};


//
// Anisotropic GGX Microfacet Distribution Function.
//
// References:
//
//   [1] Microfacet Models for Refraction through Rough Surfaces
//       http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
//
//   [2] Understanding the Masking-Shadowing Function in Microfacet-Based BRDFs
//       http://hal.inria.fr/docs/00/96/78/44/PDF/RR-8468.pdf
//
//   [3] Importance Sampling Microfacet-Based BSDFs using the Distribution of Visible Normals.
//       https://hal.inria.fr/hal-00996995/en
//
//   [4] A Simpler and Exact Sampling Routine for the GGX Distribution of Visible Normals.
//       https://hal.archives-ouvertes.fr/hal-01509746
//

class GGXMDF
{
  public:
    static float D(
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static float G(
        const Vector3f&     wi,
        const Vector3f&     wo,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static Vector3f sample(
        const Vector3f&     v,
        const Vector2f&     s,
        const float         alpha_x,
        const float         alpha_y);

    static float pdf(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    // Isotropic versions of the above methods.
    // They are used in shading models that don't support
    // anisotropy and when computing albedo tables.

    static float D(
        const Vector3f&     m,
        const float         alpha);

    static float G(
        const Vector3f&     wi,
        const Vector3f&     wo,
        const Vector3f&     m,
        const float         alpha);

    static float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha);

    static Vector3f sample(
        const Vector3f&     v,
        const Vector2f&     s,
        const float         alpha);

    static float pdf(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha);

  private:
    static float lambda(
        const Vector3f&     v,
        const float         alpha_x,
        const float         alpha_y);

    static float lambda(
        const Vector3f&     v,
        const float         alpha);
};


//
// Ward Microfacet Distribution Function.
//
// Reference:
//
//   A Microfacet Based Coupled Specular-Matte BRDF Model with Importance Sampling
//   http://sirkan.iit.bme.hu/~szirmay/scook.pdf
//
// Note: not a true MDF as it integrates to values less than 1!
//

class WardMDF
{
  public:
    static float D(
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static float G(
        const Vector3f&     wi,
        const Vector3f&     wo,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static Vector3f sample(
        const Vector3f&     v,
        const Vector2f&     s,
        const float         alpha_x,
        const float         alpha_y);

    static float pdf(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);
};


//
// GTR1 Microfacet Distribution Function.
//
// References:
//
//   [1] Physically-Based Shading at Disney
//       https://disney-animation.s3.amazonaws.com/library/s2012_pbs_disney_brdf_notes_v2.pdf
//
//   [2] Deriving the Smith shadowing function G1 for gamma (0, 4]
//       https://docs.chaosgroup.com/download/attachments/7147732/gtr_shadowing.pdf?version=2&modificationDate=1434539612000&api=v2
//

class GTR1MDF
{
  public:
    static float D(
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static float G(
        const Vector3f&     wi,
        const Vector3f&     wo,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

    static Vector3f sample(
        const Vector3f&     v,
        const Vector2f&     s,
        const float         alpha_x,
        const float         alpha_y);

    static float pdf(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y);

  private:
    static float lambda(
        const Vector3f&     v,
        const float         alpha_x,
        const float         alpha_y);
};


//
// Student's t-Distribution MDF.
//
// Reference:
//
//   STD: Student's t-Distribution of Slopes for Microfacet Based BSDFs
//   Mickael Ribardiere, Benjamin Bringier, Daniel Meneveaux, Lionel Simonot
//   https://mribar03.bitbucket.io/projects/eg_2017/
//

class StdMDF
{
  public:
    static float D(
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma);

    static float G(
        const Vector3f&     wi,
        const Vector3f&     wo,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma);

    static float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma);

    static Vector3f sample(
        const Vector3f&     v,
        const Vector2f&     s,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma);

    static float pdf(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma);

  private:
    static float lambda(
        const Vector3f&     v,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma);

    static float S2(const float cot_theta, const float gamma);
};

}   // namespace foundation
