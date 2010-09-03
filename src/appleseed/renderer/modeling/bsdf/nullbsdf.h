
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/global/global.h"
#include "renderer/modeling/bsdf/bsdf.h"

namespace renderer
{

//
// A dummy BSDF 
//

class NullBSDF
  : public BSDF
{
  public:
    NullBSDF()
      : BSDF(ParamArray())
    {
    }

    virtual void release()
    {
        delete this;
    }

    virtual const char* get_model() const
    {
        return "null_bsdf";
    }

    virtual const char* get_name() const
    {
        return "null_bsdf";
    }

    virtual void sample(
        const void*                 data,                       // input values
        const foundation::Vector3d& geometric_normal,           // world space geometric normal, unit-length
        const foundation::Basis3d&  shading_basis,              // world space orthonormal basis around shading normal
        const foundation::Vector3d& s,                          // sample in [0,1)^3
        const foundation::Vector3d& outgoing,                   // world space outgoing direction, unit-length
        foundation::Vector3d&       incoming,                   // world space incoming direction, unit-length
        Spectrum&                   value,                      // BSDF value divided by PDF value
        double&                     probability,                // PDF value
        Mode&                       mode) const                 // scattering mode
    {
        mode = None;
    }

    virtual void evaluate(
        const void*                 data,                       // input values
        const foundation::Vector3d& geometric_normal,           // world space geometric normal, unit-length
        const foundation::Basis3d&  shading_basis,              // world space orthonormal basis around shading normal
        const foundation::Vector3d& outgoing,                   // world space outgoing direction, unit-length
        const foundation::Vector3d& incoming,                   // world space incoming direction, unit-length
        Spectrum&                   value) const                // BSDF value for this pair of directions
    {
        value.set(0.0f);
    }

    virtual double evaluate_pdf(
        const void*                 data,                       // input values
        const foundation::Vector3d& geometric_normal,           // world space geometric normal, unit-length
        const foundation::Basis3d&  shading_basis,              // world space orthonormal basis around shading normal
        const foundation::Vector3d& outgoing,                   // world space outgoing direction, unit-length
        const foundation::Vector3d& incoming) const             // world space incoming direction, unit-length
    {
        return 0.0;
    }
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_NULLBSDF_H
