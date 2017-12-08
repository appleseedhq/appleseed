
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_FRESNEL_H
#define APPLESEED_RENDERER_MODELING_BSDF_FRESNEL_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cmath>

namespace renderer
{

class NoFresnelFun
{
  public:
    NoFresnelFun(
        const Spectrum&             reflectance,
        const float                 reflectance_multiplier)
      : m_reflectance(reflectance)
      , m_reflectance_multiplier(reflectance_multiplier)
    {
    }

    void operator()(
        const foundation::Vector3f& o,
        const foundation::Vector3f& h,
        const foundation::Vector3f& n,
        Spectrum&                   value) const
    {
        value = m_reflectance;
        value *= m_reflectance_multiplier;
    }

  private:
    const Spectrum& m_reflectance;
    const float     m_reflectance_multiplier;
};

class FresnelDielectricFun
{
  public:
    FresnelDielectricFun(
        const Spectrum&             reflectance,
        const float                 reflectance_multiplier,
        const float                 eta,
        const float                 weight)
      : m_reflectance(reflectance)
      , m_reflectance_multiplier(reflectance_multiplier)
      , m_eta(eta)
      , m_weight(weight)
    {
    }

    void operator()(
        const foundation::Vector3f& o,
        const foundation::Vector3f& h,
        const foundation::Vector3f& n,
        Spectrum&                   value) const
    {
        const float cos_oh = std::abs(foundation::dot(o, h));

        float f;
        foundation::fresnel_reflectance_dielectric(f, m_eta, cos_oh);
        f = foundation::lerp(1.0f, f, m_weight);

        value = m_reflectance;
        value *= f * m_reflectance_multiplier;
    }

  private:
    const Spectrum& m_reflectance;
    const float     m_reflectance_multiplier;
    const float     m_eta;
    const float     m_weight;
};

class FresnelDielectricSchlickFun
{
  public:
    FresnelDielectricSchlickFun(
        const Spectrum&             reflectance,
        const float                 fr_multiplier)
      : m_reflectance(reflectance)
      , m_fr_multiplier(fr_multiplier)
    {
    }

    void operator()(
        const foundation::Vector3f& o,
        const foundation::Vector3f& h,
        const foundation::Vector3f& n,
        Spectrum&                   value) const
    {
        const float cos_on = std::abs(foundation::dot(o, n));

        foundation::fresnel_reflectance_dielectric_schlick(
            value,
            m_reflectance,
            cos_on,
            m_fr_multiplier);
    }

  private:
    const Spectrum& m_reflectance;
    const float     m_fr_multiplier;
};

class FresnelConductorFun
{
  public:
    FresnelConductorFun(
        const Spectrum&             nt,
        const Spectrum&             kt,
        const float                 ni,
        const float                 reflectance_multiplier)
      : m_nt(nt)
      , m_kt(kt)
      , m_ni(ni)
      , m_reflectance_multiplier(reflectance_multiplier)
    {
    }

    void operator()(
        const foundation::Vector3f& o,
        const foundation::Vector3f& h,
        const foundation::Vector3f& n,
        Spectrum&                   value) const
    {
        const float cos_oh = std::abs(foundation::dot(o, h));

        foundation::fresnel_reflectance_conductor(value, m_nt, m_kt, m_ni, cos_oh);

        value *= m_reflectance_multiplier;
    }

  private:
    const Spectrum& m_nt;
    const Spectrum& m_kt;
    const float     m_ni;
    const float     m_reflectance_multiplier;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_FRESNEL_H
