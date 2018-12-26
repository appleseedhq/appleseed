
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
#include "directshadingcomponents.h"

namespace renderer
{

//
// DirectShadingComponents class implementation.
//

DirectShadingComponents::DirectShadingComponents()
{
    set(0.0f);
}

void DirectShadingComponents::set(const float val)
{
    m_beauty.set(0.0f);
    m_diffuse.set(0.0f);
    m_glossy.set(0.0f);
    m_volume.set(0.0f);
    m_emission.set(0.0f);
}

DirectShadingComponents& operator+=(DirectShadingComponents& lhs, const DirectShadingComponents& rhs)
{
    lhs.m_beauty += rhs.m_beauty;
    lhs.m_diffuse += rhs.m_diffuse;
    lhs.m_glossy += rhs.m_glossy;
    lhs.m_volume += rhs.m_volume;
    lhs.m_emission += rhs.m_emission;
    return lhs;
}

DirectShadingComponents& operator*=(DirectShadingComponents& lhs, const float rhs)
{
    lhs.m_beauty *= rhs;
    lhs.m_diffuse *= rhs;
    lhs.m_glossy *= rhs;
    lhs.m_volume *= rhs;
    lhs.m_emission *= rhs;
    return lhs;
}

DirectShadingComponents& operator*=(DirectShadingComponents& lhs, const Spectrum& rhs)
{
    lhs.m_beauty *= rhs;
    lhs.m_diffuse *= rhs;
    lhs.m_glossy *= rhs;
    lhs.m_volume *= rhs;
    lhs.m_emission *= rhs;
    return lhs;
}

DirectShadingComponents& operator/=(DirectShadingComponents& lhs, const float rhs)
{
    return lhs *= 1.0f / rhs;
}

void madd(DirectShadingComponents& a, const DirectShadingComponents& b, const float c)
{
    madd(a.m_beauty, b.m_beauty, c);
    madd(a.m_diffuse, b.m_diffuse, c);
    madd(a.m_glossy, b.m_glossy, c);
    madd(a.m_volume, b.m_volume, c);
    madd(a.m_emission, b.m_emission, c);
}

void madd(DirectShadingComponents& a, const DirectShadingComponents& b, const Spectrum& c)
{
    madd(a.m_beauty, b.m_beauty, c);
    madd(a.m_diffuse, b.m_diffuse, c);
    madd(a.m_glossy, b.m_glossy, c);
    madd(a.m_volume, b.m_volume, c);
    madd(a.m_emission, b.m_emission, c);
}

}   // namespace renderer
