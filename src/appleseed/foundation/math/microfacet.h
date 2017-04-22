
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_MICROFACET_H
#define APPLESEED_FOUNDATION_MATH_MICROFACET_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <algorithm>

namespace foundation
{

//
// MDF: Base class for microfacet distribution functions.
//

class MDF
  : public NonCopyable
{
  public:
    virtual ~MDF();

    virtual float D(
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const = 0;

    virtual float G(
        const Vector3f&     incoming,
        const Vector3f&     outgoing,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const = 0;

    virtual float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const = 0;

    virtual Vector3f sample(
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const = 0;

    virtual float pdf(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const = 0;
};


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
  : public MDF
{
  public:
    virtual float D(
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float G(
        const Vector3f&     incoming,
        const Vector3f&     outgoing,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual Vector3f sample(
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float pdf(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;
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
  : public MDF
{
  public:
    virtual float D(
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float G(
        const Vector3f&     incoming,
        const Vector3f&     outgoing,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual Vector3f sample(
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float pdf(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    Vector2f sample11(
        const float         cos_theta,
        const Vector3f&     s,
        const float         gamma) const;

    float lambda(
        const Vector3f&     v,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const;
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

class GGXMDF
  : public MDF
{
  public:
    virtual float D(
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float G(
        const Vector3f&     incoming,
        const Vector3f&     outgoing,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual Vector3f sample(
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float pdf(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    Vector2f sample11(
        const float         cos_theta,
        const Vector3f&     s,
        const float         gamma) const;

    float lambda(
        const Vector3f&     v,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const;
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
  : public MDF
{
  public:
    virtual float D(
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float G(
        const Vector3f&     incoming,
        const Vector3f&     outgoing,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual Vector3f sample(
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float pdf(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;
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
  : public MDF
{
  public:
    virtual float D(
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float G(
        const Vector3f&     incoming,
        const Vector3f&     outgoing,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual Vector3f sample(
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float pdf(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

  private:
    float lambda(
        const Vector3f&     v,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const;
};


//
// Student's t-Distribution MDF.
//
// Reference:
//
//   https://bv.univ-poitiers.fr/access/content/user/mribar03/website/projects/eg_2017/distribution.pdf
//

class StdMDF
  : public MDF
{
  public:
    virtual float D(
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float G(
        const Vector3f&     incoming,
        const Vector3f&     outgoing,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float G1(
        const Vector3f&     v,
        const Vector3f&     m,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual Vector3f sample(
        const Vector3f&     v,
        const Vector3f&     s,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

    virtual float pdf(
        const Vector3f&     v,
        const Vector3f&     h,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const APPLESEED_OVERRIDE;

  private:
    float lambda(
        const Vector3f&     v,
        const float         alpha_x,
        const float         alpha_y,
        const float         gamma) const;

    float S2(const float cot_theta, const float gamma) const;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_MICROFACET_H
