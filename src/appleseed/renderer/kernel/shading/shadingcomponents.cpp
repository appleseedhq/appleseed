
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "shadingcomponents.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/directshadingcomponents.h"

using namespace foundation;

namespace renderer
{

//
// ShadingComponents class implementation.
//

ShadingComponents::ShadingComponents()
  : m_beauty(0.0f)
  , m_diffuse(0.0f)
  , m_glossy(0.0f)
  , m_volume(0.0f)
  , m_emission(0.0f)
  , m_indirect_diffuse(0.0f)
  , m_indirect_glossy(0.0f)
  , m_indirect_volume(0.0f)
{
}

bool ShadingComponents::is_valid() const
{
    return
        is_finite_non_neg(m_beauty) &&
        is_finite_non_neg(m_diffuse) &&
        is_finite_non_neg(m_glossy) &&
        is_finite_non_neg(m_volume) &&
        is_finite_non_neg(m_emission) &&
        is_finite_non_neg(m_indirect_diffuse) &&
        is_finite_non_neg(m_indirect_glossy) &&
        is_finite_non_neg(m_indirect_volume);
}

void ShadingComponents::add_shadowcatcher_emission(
    const size_t                    path_length,
    const ScatteringMode::Mode      scattering_mode,
    const Spectrum& value)
{
    if (path_length == 1)
    {
        m_beauty += value;
        m_emission += value;
    }
    else
    {
        switch (scattering_mode)
        {
        case ScatteringMode::Diffuse:
            m_indirect_diffuse += value;
            break;

        case ScatteringMode::Glossy:
        case ScatteringMode::Specular:
            m_indirect_glossy += value;
            break;

        case ScatteringMode::Volume:
            m_indirect_volume += value;
            break;

            assert_otherwise;
        }
    }
}

void ShadingComponents::add_emission(
    const size_t                    path_length,
    const ScatteringMode::Mode      scattering_mode,
    const Spectrum&                 value)
{
    m_beauty += value;

    if (path_length == 1)
        m_emission += value;
    else
    {
        switch (scattering_mode)
        {
          case ScatteringMode::Diffuse:
            m_indirect_diffuse += value;
            break;

          case ScatteringMode::Glossy:
          case ScatteringMode::Specular:
            m_indirect_glossy += value;
            break;

          case ScatteringMode::Volume:
            m_indirect_volume += value;
            break;

          assert_otherwise;
        }
    }
}

void ShadingComponents::add(
    const size_t                    path_length,
    const ScatteringMode::Mode      scattering_mode,
    const DirectShadingComponents&  value)
{
    m_beauty += value.m_beauty;

    if (path_length == 1)
    {
        m_diffuse += value.m_diffuse;
        m_glossy += value.m_glossy;
        m_volume += value.m_volume;
        m_emission += value.m_emission;
    }
    else
    {
        switch (scattering_mode)
        {
          case ScatteringMode::Diffuse:
            m_indirect_diffuse += value.m_beauty;
            break;

          case ScatteringMode::Glossy:
          case ScatteringMode::Specular:
            m_indirect_glossy += value.m_beauty;
            break;

          case ScatteringMode::Volume:
            m_indirect_volume += value.m_beauty;
            break;

          assert_otherwise;
        }
    }
}

ShadingComponents& operator*=(ShadingComponents& lhs, const float rhs)
{
    lhs.m_beauty *= rhs;
    lhs.m_diffuse *= rhs;
    lhs.m_glossy *= rhs;
    lhs.m_volume *= rhs;
    lhs.m_emission *= rhs;
    lhs.m_indirect_diffuse *= rhs;
    lhs.m_indirect_glossy *= rhs;
    lhs.m_indirect_volume *= rhs;
    return lhs;
}

ShadingComponents& operator/=(ShadingComponents& lhs, const float rhs)
{
    const float rcp_rhs = 1.0f / rhs;
    lhs *= rcp_rhs;
    return lhs;
}

}   // namespace renderer
