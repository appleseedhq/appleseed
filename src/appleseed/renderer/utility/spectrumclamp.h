
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Kevin Masson, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/directshadingcomponents.h"

namespace renderer
{

//
// Clamp the contribution of a given spectrum.
//

inline void clamp_contribution(Spectrum& radiance, const float high)
{
    const float avg = foundation::average_value(radiance);

    if (avg > high)
        radiance *= high / avg;
}

inline void clamp_contribution(DirectShadingComponents& radiance, const float high)
{
    // Clamp all components.
    clamp_contribution(radiance.m_diffuse, high);
    clamp_contribution(radiance.m_glossy, high);
    clamp_contribution(radiance.m_volume, high);
    clamp_contribution(radiance.m_emission, high);

    // Rebuild the beauty component.
    radiance.m_beauty  = radiance.m_diffuse;
    radiance.m_beauty += radiance.m_glossy;
    radiance.m_beauty += radiance.m_volume;
    radiance.m_beauty += radiance.m_emission;
}

}   // namespace renderer
