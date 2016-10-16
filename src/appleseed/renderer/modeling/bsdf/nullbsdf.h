
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_NULLBSDF_H
#define APPLESEED_RENDERER_MODELING_BSDF_NULLBSDF_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/modeling/bsdf/bsdf.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

namespace renderer
{

//
// A dummy BSDF.
//

class NullBSDF
  : public BSDF
{
  public:
    NullBSDF()
      : BSDF("null_bsdf", Reflective, ScatteringMode::Absorption, ParamArray())
    {
    }

    virtual void release() APPLESEED_OVERRIDE
    {
        delete this;
    }

    virtual const char* get_model() const APPLESEED_OVERRIDE
    {
        return "null_bsdf";
    }

    virtual void sample(
        SamplingContext&                sampling_context,
        const void*                     data,
        const bool                      adjoint,
        const bool                      cosine_mult,
        BSDFSample&                     sample) const APPLESEED_OVERRIDE
    {
    }

    virtual float evaluate(
        const void*                     data,
        const bool                      adjoint,
        const bool                      cosine_mult,
        const foundation::Vector3f&     geometric_normal,
        const foundation::Basis3f&      shading_basis,
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        const int                       modes,
        Spectrum&                       value) const APPLESEED_OVERRIDE
    {
        return 0.0f;
    }

    virtual float evaluate_pdf(
        const void*                     data,
        const foundation::Vector3f&     geometric_normal,
        const foundation::Basis3f&      shading_basis,
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        const int                       modes) const APPLESEED_OVERRIDE
    {
        return 0.0f;
    }
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_NULLBSDF_H
