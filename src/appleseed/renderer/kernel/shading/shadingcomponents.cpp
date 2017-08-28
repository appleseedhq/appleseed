
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/utility/otherwise.h"

namespace renderer
{

//
// ShadingComponents class implementation.
//

ShadingComponents::ShadingComponents()
{
    set(0.0f);
}

void ShadingComponents::set(const float val)
{
    m_beauty.set(0.0f);
    m_diffuse.set(0.0f);
    m_glossy.set(0.0f);
    m_volume.set(0.0f);
    m_emission.set(0.0f);
}

void ShadingComponents::add_to_component(
    const ScatteringMode::Mode  scattering_mode,
    const Spectrum&             value)
{
    switch (scattering_mode)
    {
      case ScatteringMode::Diffuse:
        m_diffuse += value;
        break;

      case ScatteringMode::Glossy:
      case ScatteringMode::Specular:
        m_glossy += value;
        break;

      case ScatteringMode::Volume:
        m_volume += value;
        break;

      assert_otherwise;
    }
}

void ShadingComponents::add_to_component(
    const ScatteringMode::Mode  scattering_mode,
    const ShadingComponents&    value)
{
    m_beauty += value.m_beauty;
    add_to_component(scattering_mode, value.m_beauty);
}

void ShadingComponents::add_emission(
    const size_t                path_length,
    const ScatteringMode::Mode  scattering_mode,
    const Spectrum&             value)
{
    m_beauty += value;

    if (path_length == 1)
        m_emission += value;
    else
        add_to_component(scattering_mode, value);
}

ShadingComponents& operator+=(ShadingComponents& lhs, const ShadingComponents& rhs)
{
    lhs.m_beauty += rhs.m_beauty;
    lhs.m_diffuse += rhs.m_diffuse;
    lhs.m_glossy += rhs.m_glossy;
    lhs.m_volume += rhs.m_volume;
    lhs.m_emission += rhs.m_emission;
    return lhs;
}

ShadingComponents& operator*=(ShadingComponents& lhs, const float rhs)
{
    lhs.m_beauty *= rhs;
    lhs.m_diffuse *= rhs;
    lhs.m_glossy *= rhs;
    lhs.m_volume *= rhs;
    lhs.m_emission *= rhs;
    return lhs;
}

ShadingComponents& operator/=(ShadingComponents& lhs, const float rhs)
{
    const float rcp_rhs = 1.0f / rhs;
    lhs *= rcp_rhs;
    return lhs;
}

ShadingComponents& operator*=(ShadingComponents& lhs, const Spectrum& rhs)
{
    lhs.m_beauty *= rhs;
    lhs.m_diffuse *= rhs;
    lhs.m_glossy *= rhs;
    lhs.m_volume *= rhs;
    lhs.m_emission *= rhs;
    return lhs;
}

void madd(ShadingComponents& a, const ShadingComponents& b, const float c)
{
    madd(a.m_beauty, b.m_beauty, c);
    madd(a.m_diffuse, b.m_diffuse, c);
    madd(a.m_glossy, b.m_glossy, c);
    madd(a.m_volume, b.m_volume, c);
    madd(a.m_emission, b.m_emission, c);
}

void madd(ShadingComponents& a, const ShadingComponents& b, const Spectrum& c)
{
    madd(a.m_beauty, b.m_beauty, c);
    madd(a.m_diffuse, b.m_diffuse, c);
    madd(a.m_glossy, b.m_glossy, c);
    madd(a.m_volume, b.m_volume, c);
    madd(a.m_emission, b.m_emission, c);
}

}   // namespace renderer
