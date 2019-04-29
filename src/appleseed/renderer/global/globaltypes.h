
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#ifdef APPLESEED_WITH_SPECTRAL_SUPPORT
#include "renderer/utility/dynamicspectrum.h"
#else
#include "renderer/utility/rgbspectrum.h"
#endif

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/aabb.h"
#include "foundation/math/ray.h"
#include "foundation/math/rng/xoroshiro128plus.h"
#include "foundation/math/sampling/qmcsamplingcontext.h"
#include "foundation/math/vector.h"

namespace renderer
{

//
// Globally defined types.
//

// In-memory geometry storage type.
typedef float GScalar;

// GScalar-derived types.
typedef foundation::Vector<GScalar, 2> GVector2;
typedef foundation::Vector<GScalar, 3> GVector3;
typedef foundation::AABB<GScalar, 3> GAABB3;
typedef foundation::AABB<GScalar, 1> GAABB1;
typedef foundation::Ray<GScalar, 3> GRay3;
typedef foundation::RayInfo<GScalar, 3> GRayInfo3;

// Spectrum representation.
#ifdef APPLESEED_WITH_SPECTRAL_SUPPORT
typedef DynamicSpectrum31f Spectrum;
#else
typedef RGBSpectrumf Spectrum;
#endif

// Alpha channel representation.
typedef foundation::Color<float, 1> Alpha;

// Sampling context.
typedef foundation::QMCSamplingContext<
    foundation::Xoroshiro128plus
> SamplingContext;

}   // namespace renderer
