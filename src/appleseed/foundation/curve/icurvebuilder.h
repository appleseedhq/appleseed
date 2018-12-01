
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Girish Ramesh, The appleseedhq Organization
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
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/curve/curvebasis.h"
#include "foundation/image/color.h"
#include "foundation/math/vector.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

//
// Curve builder interface.
//

class APPLESEED_DLLSYMBOL ICurveBuilder
  : public NonCopyable
{
  public:
    // Destructor.
    virtual ~ICurveBuilder() {}

    // Begin the definition of a curve object.
    virtual void begin_curve_object(const CurveBasis basis, const size_t count = 0) = 0;

    // Begin the definition of a curve.
    virtual void begin_curve() = 0;

    // Append a vertex to the curve.
    virtual void push_vertex(const Vector3f& v) = 0;

    // Append a width to the vertex of a curve.
    virtual void push_vertex_width(const float w) = 0;

    // Append a color value to the vertex of a curve.
    virtual void push_vertex_color(const Color3f& c) = 0;

    // Append an opacity value to the vertex of a curve.
    virtual void push_vertex_opacity(const float o) = 0;

    // End the definition of a curve.
    virtual void end_curve() = 0;

    // End the definition of a curve object.
    virtual void end_curve_object() = 0;
};

}   // namespace foundation
