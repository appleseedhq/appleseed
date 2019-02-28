
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
// Curve walker interface.
//

class APPLESEED_DLLSYMBOL ICurveWalker
  : public NonCopyable
{
  public:
    // Destructor.
    virtual ~ICurveWalker() {}

    // Return the basis of the curve.
    virtual CurveBasis get_basis() const = 0;

    // Return the number of curves.
    virtual size_t get_curve_count() const = 0;

    // Return the number of vertices in curve.
    virtual size_t get_vertex_count(const size_t i) const = 0;

    // Return vertex location on curve.
    virtual Vector3f get_vertex(const size_t i) const = 0;

    // Return vertex width on curve.
    virtual float get_vertex_width(const size_t i) const = 0;

    // Return vertex opacity on curve.
    virtual float get_vertex_opacity(const size_t i) const = 0;

    // Return vertex color on curve.
    virtual Color3f get_vertex_color(const size_t i) const = 0;
};

}   // namespace foundation
