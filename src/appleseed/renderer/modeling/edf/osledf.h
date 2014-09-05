
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_EDF_OSLEDF_H
#define APPLESEED_RENDERER_MODELING_EDF_OSLEDF_H

// appleseed.renderer headers.
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/edf/iedffactory.h"
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace renderer      { class ParamArray; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{


//
// OSL EDF.
//

class OSLEDF
  : public EDF
{
  public:
    virtual void release() OVERRIDE;

    virtual const char* get_model() const OVERRIDE;

    virtual bool is_osl_edf() const OVERRIDE;

    virtual bool on_frame_begin(
        const Project&              project,
        const Assembly&             assembly,
        foundation::AbortSwitch*    abort_switch) OVERRIDE;

    virtual void on_frame_end(
        const Project&              project,
        const Assembly&             assembly);

    virtual void evaluate_inputs(
        InputEvaluator&             input_evaluator,
        const foundation::Vector2d& uv) const OVERRIDE;

    void evaluate_osl_inputs(
        InputEvaluator&             input_evaluator,
        const ShadingPoint&         shading_point) const;

    virtual void sample(
        SamplingContext&            sampling_context,
        const void*                 data,
        const foundation::Vector3d& geometric_normal,
        const foundation::Basis3d&  shading_basis,
        const foundation::Vector2d& s,
        foundation::Vector3d&       outgoing,
        Spectrum&                   value,
        double&                     probability) const OVERRIDE;

    virtual void evaluate(
        const void*                 data,
        const foundation::Vector3d& geometric_normal,
        const foundation::Basis3d&  shading_basis,
        const foundation::Vector3d& outgoing,
        Spectrum&                   value) const OVERRIDE;

    virtual void evaluate(
        const void*                 data,
        const foundation::Vector3d& geometric_normal,
        const foundation::Basis3d&  shading_basis,
        const foundation::Vector3d& outgoing,
        Spectrum&                   value,
        double&                     probability) const OVERRIDE;

    virtual double evaluate_pdf(
        const void*                 data,
        const foundation::Vector3d& geometric_normal,
        const foundation::Basis3d&  shading_basis,
        const foundation::Vector3d& outgoing) const OVERRIDE;

  private:
    friend class OSLEDFFactory;

    OSLEDF(
        const char*         name,
        const ParamArray&   params);

    foundation::auto_release_ptr<EDF> m_diffuse_edf;
};


//
// OSL EDF factory.
//

class OSLEDFFactory
{
  public:
    // Create a new EDF instance.
    foundation::auto_release_ptr<EDF> create() const;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_EDF_OSLEDF_H
