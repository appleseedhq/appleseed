
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

#ifndef APPLESEED_RENDERER_KERNEL_SHADING_SHADINGCOMPONENTS_H
#define APPLESEED_RENDERER_KERNEL_SHADING_SHADINGCOMPONENTS_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"

namespace renderer
{

class ShadingComponents
{
  public:
    Spectrum m_beauty;
    Spectrum m_diffuse;
    Spectrum m_glossy;
    Spectrum m_volume;
    Spectrum m_emission;

    // Constructor.
    explicit ShadingComponents(const Spectrum::Intent intent = Spectrum::Reflectance);

    void set(const float val);

    void add_to_component(
        const ScatteringMode::Mode  scattering_mode,
        const Spectrum&             value);

    void add_to_component(
        const ScatteringMode::Mode  scattering_mode,
        const ShadingComponents&    value);

    void add_emission(
        const size_t                path_length,
        const ScatteringMode::Mode  scattering_mode,
        const Spectrum&             value);
};

ShadingComponents& operator+=(ShadingComponents& lhs, const ShadingComponents& rhs);

ShadingComponents& operator*=(ShadingComponents& lhs, const float rhs);
ShadingComponents& operator/=(ShadingComponents& lhs, const float rhs);

ShadingComponents& operator*=(ShadingComponents& lhs, const Spectrum& rhs);

void madd(ShadingComponents& a, const ShadingComponents& b, const float c);
void madd(ShadingComponents& a, const ShadingComponents& b, const Spectrum& c);

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_SHADINGCOMPONENTS_H
