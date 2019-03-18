
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"

// Standard headers.
#include <cstddef>

namespace renderer
{

//
// NPRShading closure input values.
//

APPLESEED_DECLARE_INPUT_VALUES(NPRShadingInputValues)
{
};


//
// NPRContour closure input values.
//

enum class NPRContourFeatures : unsigned int
{
    None                    = 0u,
    All                     = ~0u,

    ObjectInstanceID        = 1u << 1,
    MaterialID              = 1u << 2,
    AllIDFeatures           = ObjectInstanceID | MaterialID,

    OcclusionEdges          = 1u << 3,
    CreaseEdges             = 1u << 4,
    AllDifferenceFeatures   = OcclusionEdges | CreaseEdges,
};

APPLESEED_DECLARE_INPUT_VALUES(NPRContourInputValues)
{
    foundation::Color3f     m_color;
    float                   m_opacity;
    float                   m_width;
    float                   m_occlusion_threshold;
    float                   m_cos_crease_threshold;
    unsigned int            m_features;
    size_t                  m_quality;
};

}   // namespace renderer
