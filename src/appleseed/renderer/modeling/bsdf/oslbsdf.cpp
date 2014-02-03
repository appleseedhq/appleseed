
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

// Interface header.
#include "oslbsdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/bsdf/ibsdffactory.h"
#include "renderer/modeling/bsdf/bsdffactoryregistrar.h"

// foundation.renderer headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"

// Standard headers.
#include <cmath>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

//
// OSLBSDF class implementation.
//

OSLBSDF::OSLBSDF()
  : BSDF("osl_bsdf", Reflective, Absorption, ParamArray())
{
    m_bsdfs.insert( BSDFFactoryRegistrar().lookup("ashikhmin_brdf")->create("osl_ashikhmin_shirley",
                                                                            ParamArray()));

    /*    
    m_diffuse_btdf      = BSDFFactoryRegistrar().lookup("diffuse_btdf")->create("osl_translucent", 
                                                                                ParamArray());

    m_lambertian_brdf   = BSDFFactoryRegistrar().lookup("lambertian_brdf")->create("osl_lambert", 
                                                                                   ParamArray());
    
    m_specular_brdf     = BSDFFactoryRegistrar().lookup("specular_brdf")->create("osl_reflection",
                                                                                 ParamArray());

    m_specular_btdf     = BSDFFactoryRegistrar().lookup("specular_btdf")->create("osl_refraction", 
                                                                                 ParamArray());    

    // microfacet models
    {
        ParamArray params;
        params.insert("mdf", "beckmann");
        m_microfacet_beckmann_brdf = BSDFFactoryRegistrar().lookup("microfacet_brdf")->create("osl_beckmann",
                                                                                                params);
    }

    {
        ParamArray params;    
        params.insert("mdf", "blinn");
        m_microfacet_blinn_brdf = BSDFFactoryRegistrar().lookup("microfacet_brdf")->create("osl_blinn",
                                                                                            params);
    }

    {
        ParamArray params;
        params.insert("mdf", "ggx");
        m_microfacet_ggx_brdf = BSDFFactoryRegistrar().lookup("microfacet_brdf")->create("osl_ggx",
                                                                                         params);
    }

    {
        ParamArray params;
        params.insert("mdf", "ward");
        m_microfacet_ward_brdf = BSDFFactoryRegistrar().lookup("microfacet_brdf")->create("osl_ward",
                                                                                          params);
    }
    */
}

bool OSLBSDF::on_frame_begin(
    const Project&              project,
    const Assembly&             assembly)
{
    if (!BSDF::on_frame_begin(project, assembly))
        return false;

    /*
    for (int i = FirstBSDFClosureID; i < NumClosuresID; ++i)
    {
        if (!BSDF_for_closureID(static_cast<ClosureID>(i))->on_frame_begin(project, assembly))
            return false;
    }
    */

    return true;
}

void OSLBSDF::on_frame_end(
    const Project&              project,
    const Assembly&             assembly)
{
    BSDF::on_frame_end(project, assembly);

    /*
    for (int i = FirstBSDFClosureID; i < NumClosuresID; ++i)
        BSDF_for_closureID(static_cast<ClosureID>(i))->on_frame_end(project, assembly);
    */
}

size_t OSLBSDF::compute_input_data_size(
    const Assembly&             assembly) const
{
    return sizeof(CompositeClosure);
}

void OSLBSDF::evaluate_inputs(
    InputEvaluator&     input_evaluator,
    const ShadingPoint& shading_point,
    const size_t        offset) const
{
    /*
    #ifndef NDEBUG
        assert(shading_point.m_osl_shader_executed);
    #endif

    CompositeClosure *c = reinterpret_cast<CompositeClosure*>(const_cast<void*>(input_evaluator.data()));
    
    #ifndef NDEBUG
        std::memset(c, 0, sizeof(CompositeClosure));
    #endif
    
    new (c) CompositeClosure(shading_point.get_osl_shader_globals().Ci);
    */
}

/*
namespace
{

Basis3d make_osl_basis(const CompositeClosure *c, size_t index, const Basis3d& original_basis)
{
    return Basis3d( c->normal(index),  
                    c->has_tangent(index) ? c->tangent(index) : original_basis.get_tangent_u());
}

}
*/

BSDF::Mode OSLBSDF::sample(
    SamplingContext&    sampling_context,
    const void*         data,
    const bool          adjoint,
    const bool          cosine_mult,
    const Vector3d&     geometric_normal,
    const Basis3d&      shading_basis,
    const Vector3d&     outgoing,
    Vector3d&           incoming,
    Spectrum&           value,
    double&             probability) const
{
    /*
    const CompositeClosure *c = reinterpret_cast<const CompositeClosure*>(data);
    
    if(c->num_closures())
    {
        sampling_context.split_in_place(1, 1);
        const double s = sampling_context.next_double2();
        int closure_index = c->choose_closure(s);
        Basis3d new_shading_basis = make_osl_basis(c, closure_index, shading_basis);
        return BSDF_for_closureID(c->closure_type(closure_index))->sample(sampling_context,
                                                                          c->input_values(closure_index),
                                                                          adjoint,
                                                                          false,
                                                                          geometric_normal,
                                                                          new_shading_basis,
                                                                          outgoing,
                                                                          incoming,
                                                                          value,
                                                                          probability);        
    }
    */

    // no closures case
    value.set(0.0f);
    probability = 0.0;
    return Absorption;
}

double OSLBSDF::evaluate(
    const void*         data,
    const bool          adjoint,
    const bool          cosine_mult,
    const Vector3d&     geometric_normal,
    const Basis3d&      shading_basis,
    const Vector3d&     outgoing,
    const Vector3d&     incoming,
    const int           modes,
    Spectrum&           value) const
{
    /*
    const CompositeClosure *c = reinterpret_cast<const CompositeClosure*>(data);
    double prob = 0.0;
    value.set(0.0f);

    for(int i = 0, e = c->num_closures(); i < e; ++i)
    {
        Spectrum s;
        Basis3d new_shading_basis = make_osl_basis(c, i, shading_basis);
        double bsdf_prob = BSDF_for_closureID(c->closure_type(i))->evaluate(c->input_values(i),
                                                                            adjoint,
                                                                            false,
                                                                            geometric_normal,
                                                                            new_shading_basis,
                                                                            outgoing,
                                                                            incoming,
                                                                            modes,
                                                                            s);
        if (bsdf_prob > 0.0)
            value += s * static_cast<float>(c->weight(i));
        
        prob += bsdf_prob * c->weight(i);
    }

    return prob;
    */
    return 0.0;
}

double OSLBSDF::evaluate_pdf(
    const void*         data,
    const Vector3d&     geometric_normal,
    const Basis3d&      shading_basis,
    const Vector3d&     outgoing,
    const Vector3d&     incoming,
    const int           modes) const
{
    /*
    const CompositeClosure *c = reinterpret_cast<const CompositeClosure*>(data);
    double prob = 0.0;
    
    for(int i = 0, e = c->num_closures(); i < e; ++i)
    {
        Basis3d new_shading_basis = make_osl_basis(c, i, shading_basis);
        double bsdf_prob = BSDF_for_closureID(c->closure_type(i))->evaluate_pdf(c->input_values(i),
                                                                                geometric_normal,
                                                                                new_shading_basis,
                                                                                outgoing,
                                                                                incoming,
                                                                                modes);
        prob += bsdf_prob * c->weight(i); 
    }

    return prob;
    */
    return 0.0;
}

}   // namespace renderer
