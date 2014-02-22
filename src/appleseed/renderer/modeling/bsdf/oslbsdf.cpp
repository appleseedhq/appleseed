
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

// Standard headers.
#include <cstring>

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
    memset(m_all_bsdfs, 0, sizeof(BSDF*) * NumClosuresIDs);

    create_bsdf(
        m_ashikhmin_shirley_brdf,
        "ashikhmin_brdf",
        AshikhminShirleyID,
        "osl_ashikhmin_shirley");

    create_bsdf(
        m_diffuse_btdf,
        "diffuse_btdf",
        TranslucentID,
        "osl_translucent");

    create_bsdf(
        m_lambertian_brdf,
        "lambertian_brdf",
        LambertID,
        "osl_lambert");

    create_bsdf(
        m_specular_brdf,
        "specular_brdf",
        ReflectionID,
        "osl_reflection");

    create_bsdf(
        m_specular_btdf,
        "specular_btdf",
        RefractionID,
        "osl_refraction");

    create_bsdf(
        m_microfacet_beckmann_brdf,
        "microfacet_brdf",
        MicrofacetBeckmannID,
        "osl_beckmann",
        ParamArray().insert("mdf", "beckmann"));

    create_bsdf(
        m_microfacet_blinn_brdf,
        "microfacet_brdf",
        MicrofacetBlinnID,
        "osl_blinn",
        ParamArray().insert("mdf", "blinn"));

    create_bsdf(
        m_microfacet_ggx_brdf,
        "microfacet_brdf",
        MicrofacetGGXID,
        "osl_ggx",
        ParamArray().insert("mdf", "ggx"));

    create_bsdf(
        m_microfacet_ward_brdf,
        "microfacet_brdf",
        MicrofacetWardID,
        "osl_ward",
        ParamArray().insert("mdf", "ward"));
}

void OSLBSDF::release()
{
    delete this;
}

const char* OSLBSDF::get_model() const
{
    return "osl_bsdf";
}

bool OSLBSDF::on_frame_begin(
    const Project&          project,
    const Assembly&         assembly,
    AbortSwitch&            abort_switch)
{
    if (!BSDF::on_frame_begin(project, assembly))
        return false;

    for (int i = 0; i < NumClosuresIDs; ++i)
    {
        if (BSDF* bsdf = m_all_bsdfs[i])
        {
            if (!bsdf->on_frame_begin(project, assembly))
                return false;
        }
    }

    return true;
}

void OSLBSDF::on_frame_end(
    const Project&          project,
    const Assembly&         assembly)
{
    BSDF::on_frame_end(project, assembly);

    for (int i = 0; i < NumClosuresIDs; ++i)
    {
        if (BSDF* bsdf = m_all_bsdfs[i])
            bsdf->on_frame_end(project, assembly);
    }
}

size_t OSLBSDF::compute_input_data_size(const Assembly& assembly) const
{
    return sizeof(CompositeClosure);
}

void OSLBSDF::evaluate_inputs(
    InputEvaluator&         input_evaluator,
    const ShadingPoint&     shading_point,
    const size_t            offset) const
{
    CompositeClosure* c = reinterpret_cast<CompositeClosure*>(input_evaluator.data());
    new (c) CompositeClosure(shading_point.get_osl_shader_globals().Ci);
}

namespace
{
    Basis3d make_osl_basis(
        const CompositeClosure* c, 
        const size_t            index,
        const Basis3d&          original_basis)
    {
        return
            Basis3d(
                c->closure_normal(index),
                c->closure_has_tangent(index)
                    ? c->closure_tangent(index)
                    : original_basis.get_tangent_u());
    }
}

BSDF::Mode OSLBSDF::sample(
    SamplingContext&        sampling_context,
    const void*             data,
    const bool              adjoint,
    const bool              cosine_mult,
    const Vector3d&         geometric_normal,
    const Basis3d&          shading_basis,
    const Vector3d&         outgoing,
    Vector3d&               incoming,
    Spectrum&               value,
    double&                 probability) const
{
    const CompositeClosure* c = reinterpret_cast<const CompositeClosure*>(data);

    if (c->num_closures() > 0)
    {
        sampling_context.split_in_place(1, 1);
        const double s = sampling_context.next_double2();

        const int closure_index = c->choose_closure(s);
        const Basis3d new_shading_basis = make_osl_basis(c, closure_index, shading_basis);

        return
            bsdf_to_closure_id(c->closure_type(closure_index))->sample(
                sampling_context,
                c->closure_input_values(closure_index),
                adjoint,
                false,
                geometric_normal,
                new_shading_basis,
                outgoing,
                incoming,
                value,
                probability);
    }
    else
    {
        value.set(0.0f);
        probability = 0.0;
        return Absorption;
    }
}

double OSLBSDF::evaluate(
    const void*             data,
    const bool              adjoint,
    const bool              cosine_mult,
    const Vector3d&         geometric_normal,
    const Basis3d&          shading_basis,
    const Vector3d&         outgoing,
    const Vector3d&         incoming,
    const int               modes,
    Spectrum&               value) const
{
    double prob = 0.0;
    value.set(0.0f);

    const CompositeClosure* c = reinterpret_cast<const CompositeClosure*>(data);

    for (size_t i = 0, e = c->num_closures(); i < e; ++i)
    {
        Spectrum s;
        const Basis3d new_shading_basis = make_osl_basis(c, i, shading_basis);

        const double bsdf_prob =
            bsdf_to_closure_id(c->closure_type(i))->evaluate(
                c->closure_input_values(i),
                adjoint,
                false,
                geometric_normal,
                new_shading_basis,
                outgoing,
                incoming,
                modes,
                s);

        if (bsdf_prob > 0.0)
        {
            s *= static_cast<float>(c->closure_weight(i));
            value += s;
        }

        prob += bsdf_prob * c->closure_weight(i);
    }

    return prob;
}

double OSLBSDF::evaluate_pdf(
    const void*             data,
    const Vector3d&         geometric_normal,
    const Basis3d&          shading_basis,
    const Vector3d&         outgoing,
    const Vector3d&         incoming,
    const int               modes) const
{
    const CompositeClosure* c = reinterpret_cast<const CompositeClosure*>(data);
    double prob = 0.0;

    for (size_t i = 0, e = c->num_closures(); i < e; ++i)
    {
        const Basis3d new_shading_basis = make_osl_basis(c, i, shading_basis);

        const double bsdf_prob =
            bsdf_to_closure_id(c->closure_type(i))->evaluate_pdf(
                c->closure_input_values(i),
                geometric_normal,
                new_shading_basis,
                outgoing,
                incoming,
                modes);

        prob += bsdf_prob * c->closure_weight(i); 
    }

    return prob;
}

void OSLBSDF::create_bsdf(
    auto_release_ptr<BSDF>& ptr,
    const char*             model,
    ClosureID               cid,
    const char*             name,
    const ParamArray&       params)
{
    ptr.reset(BSDFFactoryRegistrar().lookup(model)->create(name, params).release());
    m_all_bsdfs[cid] = ptr.get();
}

}   // namespace renderer
