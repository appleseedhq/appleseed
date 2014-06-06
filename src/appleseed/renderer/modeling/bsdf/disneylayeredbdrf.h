
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_DISNEYLAYEREDBRDF_H
#define APPLESEED_RENDERER_MODELING_BSDF_DISNEYLAYEREDBRDF_H

// Interface header.
#include "bsdf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace renderer      { class DisneyMaterial; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// Disney Layered BRDF class.
//

class DisneyLayeredBRDF
  : public BSDF
{
  public:
    // Constructor.
    DisneyLayeredBRDF();
    
    // Delete this instance.    
    virtual void release() OVERRIDE;
    
    // Return a string identifying the model of this entity.    
    virtual const char* get_model() const OVERRIDE;
    
    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&              project,
        const Assembly&             assembly,
        foundation::AbortSwitch*    abort_switch = 0) OVERRIDE;

    // This method is called once after rendering each frame.
    virtual void on_frame_end(
        const Project&              project,
        const Assembly&             assembly) OVERRIDE;

    // Compute the cumulated size in bytes of the values of all inputs of
    // this BSDF and its child BSDFs, if any.
    virtual size_t compute_input_data_size(
        const Assembly&             assembly) const OVERRIDE;

    // Evaluate the inputs of this BSDF and of its child BSDFs, if any.
    // Input values are stored in the input evaluator. This method is called
    // once per shading point and pair of incoming/outgoing directions.
    virtual void evaluate_inputs(
        InputEvaluator&             input_evaluator,
        const ShadingPoint&         shading_point,
        const size_t                offset = 0) const OVERRIDE;

    // Given an outgoing direction, sample the BSDF and compute the incoming
    // direction, its probability density and the value of the BSDF for this
    // pair of directions. Return the scattering mode. If the scattering mode
    // is Absorption, the BSDF and PDF values are undefined.    
    virtual BSDF::Mode sample(
            SamplingContext&                sampling_context,
            const void*                     data,
            const bool                      adjoint,
            const bool                      cosine_mult,
            const foundation::Vector3d&     geometric_normal,
            const foundation::Basis3d&      shading_basis,
            const foundation::Vector3d&     outgoing,
            foundation::Vector3d&           incoming,
            Spectrum&                       value,
            double&                         probability) const OVERRIDE;

    // Evaluate the BSDF for a given pair of directions. Return the PDF value
    // for this pair of directions. If the returned probability is zero, the
    // BSDF value is undefined.    
    virtual double evaluate(
            const void*                     data,
            const bool                      adjoint,
            const bool                      cosine_mult,
            const foundation::Vector3d&     geometric_normal,
            const foundation::Basis3d&      shading_basis,
            const foundation::Vector3d&     outgoing,
            const foundation::Vector3d&     incoming,
            const int                       modes,
            Spectrum&                       value) const OVERRIDE;
    
    // Evaluate the PDF for a given pair of directions.    
    virtual double evaluate_pdf(
            const void*                     data,
            const foundation::Vector3d&     geometric_normal,
            const foundation::Basis3d&      shading_basis,
            const foundation::Vector3d&     outgoing,
            const foundation::Vector3d&     incoming,
            const int                       modes) const OVERRIDE;

  private:
    friend class DisneyMaterial;

    foundation::auto_release_ptr<BSDF> m_brdf;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_DISNEYBRDF_H
