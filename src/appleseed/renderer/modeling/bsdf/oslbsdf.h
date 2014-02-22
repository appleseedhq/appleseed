
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_OSLBSDF_H
#define APPLESEED_RENDERER_MODELING_BSDF_OSLBSDF_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/modeling/bsdf/bsdf.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"

// Forward declarations.
namespace renderer  { class Material; }

namespace renderer
{

//
// OSL closure tree -> appleseed BSDFs adapter.
//

class OSLBSDF
  : public BSDF
{
  public:
    virtual void release() OVERRIDE
    {
        delete this;
    }

    virtual const char* get_model() const OVERRIDE
    {
        return "osl_bsdf";
    }

    virtual bool on_frame_begin(
        const Project&                      project,
        const Assembly&                     assembly) OVERRIDE;

    virtual void on_frame_end(
        const Project&                      project,
        const Assembly&                     assembly) OVERRIDE;

    virtual size_t compute_input_data_size(
        const Assembly&                     assembly) const OVERRIDE;

    virtual void evaluate_inputs(
        InputEvaluator&                     input_evaluator,
        const ShadingPoint&                 shading_point,
        const size_t                        offset = 0) const OVERRIDE;

    virtual Mode sample(
        SamplingContext&                    sampling_context,
        const void*                         data,
        const bool                          adjoint,
        const bool                          cosine_mult,
        const foundation::Vector3d&         geometric_normal,
        const foundation::Basis3d&          shading_basis,
        const foundation::Vector3d&         outgoing,
        foundation::Vector3d&               incoming,
        Spectrum&                           value,
        double&                             probability) const OVERRIDE;

    virtual double evaluate(
        const void*                         data,
        const bool                          adjoint,
        const bool                          cosine_mult,
        const foundation::Vector3d&         geometric_normal,
        const foundation::Basis3d&          shading_basis,
        const foundation::Vector3d&         outgoing,
        const foundation::Vector3d&         incoming,
        const int                           modes,
        Spectrum&                           value) const OVERRIDE;

    virtual double evaluate_pdf(
        const void*                         data,
        const foundation::Vector3d&         geometric_normal,
        const foundation::Basis3d&          shading_basis,
        const foundation::Vector3d&         outgoing,
        const foundation::Vector3d&         incoming,
        const int                           modes) const OVERRIDE;

private:
    friend class Material;

    foundation::auto_release_ptr<BSDF>      m_ashikhmin_shirley_brdf;
    foundation::auto_release_ptr<BSDF>      m_diffuse_btdf;
    foundation::auto_release_ptr<BSDF>      m_lambertian_brdf;
    foundation::auto_release_ptr<BSDF>      m_microfacet_beckmann_brdf;
    foundation::auto_release_ptr<BSDF>      m_microfacet_blinn_brdf;
    foundation::auto_release_ptr<BSDF>      m_microfacet_ggx_brdf;
    foundation::auto_release_ptr<BSDF>      m_microfacet_ward_brdf;
    foundation::auto_release_ptr<BSDF>      m_specular_brdf;
    foundation::auto_release_ptr<BSDF>      m_specular_btdf;
    BSDF*                                   m_all_bsdfs[NumClosuresIDs];

    OSLBSDF();

    void create_bsdf(
        foundation::auto_release_ptr<BSDF>& ptr,
        const char*                         model,
        ClosureID                           cid,
        const char*                         name,
        const ParamArray&                   params = ParamArray());

    const BSDF* bsdf_to_closure_id(const ClosureID cid) const;
    BSDF* bsdf_to_closure_id(const ClosureID cid);
};


//
// OSLBSDF class implementation.
//

inline const BSDF* OSLBSDF::bsdf_to_closure_id(const ClosureID cid) const
{
    const BSDF* bsdf = m_all_bsdfs[cid];
    assert(bsdf);
    return bsdf;
}

inline BSDF *OSLBSDF::bsdf_to_closure_id(const ClosureID cid)
{
    BSDF* bsdf = m_all_bsdfs[cid];
    assert(bsdf);
    return bsdf;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_OSLBSDF_H
