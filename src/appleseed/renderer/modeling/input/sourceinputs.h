
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Sergo Pogosyan, The appleseedhq Organization
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

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace renderer
{

class APPLESEED_DLLSYMBOL SourceInputs
{
  public:
    // Texture coordinates from UV set #0.
    float   m_uv_x;
    float   m_uv_y;

    // World space intersection point.
    double  m_point_x;
    double  m_point_y;
    double  m_point_z;

    // Constructor.
    explicit SourceInputs(const foundation::Vector2f& uv);
};


//
// SourceInputs class implementation.
//

inline SourceInputs::SourceInputs(const foundation::Vector2f& uv)
  : m_uv_x(uv.x)
  , m_uv_y(uv.y)
  , m_point_x(0.0)
  , m_point_y(0.0)
  , m_point_z(0.0)
{
}

}   // namespace renderer
