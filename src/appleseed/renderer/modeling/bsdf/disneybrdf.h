
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_DISNEYBRDF_H
#define APPLESEED_RENDERER_MODELING_BSDF_DISNEYBRDF_H

// Interface header.
#include "bsdf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/ibsdffactory.h"
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace foundation    { template <typename T> class MDF; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// Disney BRDF input values.
//

DECLARE_INPUT_VALUES(DisneyBRDFInputValues)
{
    Spectrum    m_base_color;
    double      m_subsurface;
    double      m_metallic;
    double      m_specular;
    double      m_specular_tint;
    double      m_anisotropic;
    double      m_roughness;
    double      m_sheen;
    double      m_sheen_tint;
    double      m_clearcoat;
    double      m_clearcoat_gloss;

    // This is not a real param of the BRDF.
    // Instead, it's used to hold a temporary value.
    Spectrum    m_tint_color;
};

//
// Disney BRDF class.
//

class DisneyBRDFImpl
  : public BSDF
{
  public:
    DisneyBRDFImpl(
            const char*         name,
            const ParamArray&   params);

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
    typedef DisneyBRDFInputValues InputValues;

    static foundation::LightingConditions   m_lighting_conditions;
    static Spectrum                         m_white_spectrum;
    foundation::auto_release_ptr<BSDF>      m_diffuse_brdf;    
    foundation::MDF<double>*                m_specular_mdf;
    foundation::MDF<double>*                m_clearcoat_mdf;
    
    virtual ~DisneyBRDFImpl();
    
    void compute_component_weights(
        const DisneyBRDFInputValues*    values,
        double                          weights[3]) const;

    void specular_roughness(
        const DisneyBRDFInputValues*    values,
        double&                         alpha_x,
        double&                         alpha_y,
        double&                         alpha_g) const;

    void specular_f(
        const DisneyBRDFInputValues*    values,
        const double                    cos_oh,
        Spectrum&                       f) const;

    double clearcoat_roughness(const DisneyBRDFInputValues* values) const;

    double clearcoat_f(const double clearcoat, const double cos_oh) const;
};

typedef BSDFWrapper<DisneyBRDFImpl> DisneyBRDF;

//
// Disney BRDF factory.
//

class DLLSYMBOL DisneyBRDFFactory
  : public IBSDFFactory
{
  public:
    // Return a string identifying this BSDF model.
    virtual const char* get_model() const OVERRIDE;

    // Return a human-readable string identifying this BSDF model.
    virtual const char* get_human_readable_model() const OVERRIDE;

    // Return a set of input metadata for this BSDF model.
    virtual foundation::DictionaryArray get_input_metadata() const OVERRIDE;

    // Create a new BSDF instance.
    virtual foundation::auto_release_ptr<BSDF> create(
        const char*         name,
        const ParamArray&   params) const OVERRIDE;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_DISNEYBRDF_H
