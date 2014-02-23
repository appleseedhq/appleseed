
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
#include "closures.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/otherwise.h"

// OSL headers.
#include <OSL/genclosure.h>
#include <OSL/oslclosure.h>

using namespace foundation;
using namespace renderer;

namespace renderer
{
namespace
{
    //
    // Closure parameters.
    //

    struct EmptyClosureParams {};

    struct AshikhminShirleyClosureParams
    {
        OSL::Vec3   N;
        OSL::Vec3   T;
        OSL::Color3 kd;
        OSL::Color3 ks;
        float       nu;
        float       nv;
    };

    struct LambertClosureParams
    {
        OSL::Vec3   N;
    };

    struct MicrofacetBRDFClosureParams
    {
        OSL::Vec3   N;
        float       glossiness;
    };

    struct RefractionClosureParams
    {
        OSL::Vec3   N;
        float       from_ior;
        float       to_ior;
    };

    struct ReflectionClosureParams
    {
        OSL::Vec3   N;
    };

    struct TranslucentClosureParams
    {
        OSL::Vec3   N;
    };
}


//
// CompositeClosure class implementation.
// 

CompositeClosure::CompositeClosure(
    const OSL::ClosureColor* ci)
  : m_num_closures(0)
  , m_num_bytes(0)
{
    process_closure_tree(ci, Color3f(1.0f));

    // Compute sum of weights.
    double total_weight = 0.0;
    for (size_t i = 0, e = num_closures(); i != e; ++i)
        total_weight += m_weights[i];

    // Normalize weights.
    for (size_t i = 0, e = num_closures(); i != e; ++i)
        m_weights[i] /= total_weight;

    // Build CDF.
    if (num_closures() != 0)
    {
        m_cdf.reserve(num_closures());
        for (size_t i = 0, e = num_closures(); i != e; ++i)
            m_cdf.insert(i, closure_weight(i));
        m_cdf.prepare();
    }
}

size_t CompositeClosure::choose_closure(const double w) const
{
    return m_cdf.sample(w).first;
}

void CompositeClosure::process_closure_tree(
    const OSL::ClosureColor*    closure, 
    const Color3f&              weight)
{
    if (closure == 0)
        return;

    switch (closure->type)
    {
      case OSL::ClosureColor::MUL:
        {
            const OSL::ClosureMul* c = reinterpret_cast<const OSL::ClosureMul*>(closure);
            const Color3f w = weight * Color3f(c->weight.x, c->weight.y, c->weight.z);
            process_closure_tree(c->closure, w);
        }
        break;

      case OSL::ClosureColor::ADD:
        {
            const OSL::ClosureAdd* c = reinterpret_cast<const OSL::ClosureAdd*>(closure);
            process_closure_tree(c->closureA, weight);
            process_closure_tree(c->closureB, weight);
        }
        break;

      case OSL::ClosureColor::COMPONENT:
        {
            const OSL::ClosureComponent* c = reinterpret_cast<const OSL::ClosureComponent*>(closure);
            const Color3f w = weight * Color3f(c->w.x, c->w.y, c->w.z);

            switch (c->id)
            {
              case AshikhminShirleyID:
                {
                    const AshikhminShirleyClosureParams *p = 
                        reinterpret_cast<const AshikhminShirleyClosureParams*>(c->data());

                    AshikminBRDFInputValues values;
                    Color3f total_w = w * Color3f(p->kd.x, p->kd.y, p->kd.z);
                    linear_rgb_reflectance_to_spectrum(total_w, values.m_rd);
                    values.m_rd_alpha = Alpha(1.0);
                    values.m_rd_multiplier = 1.0;
                    total_w = w * Color3f(p->ks.x, p->ks.y, p->ks.z);
                    linear_rgb_reflectance_to_spectrum(total_w, values.m_rg);
                    values.m_rg_alpha = Alpha(1.0);
                    values.m_rg_multiplier = 1.0;
                    values.m_fr_multiplier = 1.0;
                    values.m_nu = p->nu;
                    values.m_nv = p->nv;
                    add_closure<AshikminBRDFInputValues>(
                        static_cast<ClosureID>(c->id), 
                        w,
                        Vector3d(p->N.x, p->N.y, p->N.z),
                        Vector3d(p->T.x, p->T.y, p->T.z),
                        values);
                }
                break;

              case LambertID:
                {
                    const LambertClosureParams* p = 
                        reinterpret_cast<const LambertClosureParams*>(c->data());

                    LambertianBRDFInputValues values;
                    linear_rgb_reflectance_to_spectrum(w, values.m_reflectance);
                    values.m_reflectance_alpha = Alpha(1.0);
                    values.m_reflectance_multiplier = 1.0;
                    add_closure<LambertianBRDFInputValues>(
                        static_cast<ClosureID>(c->id),
                        w,
                        Vector3d(p->N.x, p->N.y, p->N.z),
                        values);
                }
                break;

              case MicrofacetBeckmannID:
              case MicrofacetBlinnID:
              case MicrofacetGGXID:
              case MicrofacetWardID:
                {
                    const MicrofacetBRDFClosureParams *p = 
                        reinterpret_cast<const MicrofacetBRDFClosureParams*>(c->data());

                    MicrofacetBRDFInputValues values;
                    linear_rgb_reflectance_to_spectrum(w,values.m_reflectance);
                    values.m_reflectance_alpha = Alpha(1.0);
                    values.m_reflectance_multiplier = 1.0;
                    values.m_glossiness = p->glossiness;
                    values.m_glossiness_multiplier = 1.0;
                    values.m_fr_multiplier = 1.0;
                    add_closure<MicrofacetBRDFInputValues>(
                        static_cast<ClosureID>(c->id), 
                        w,
                        Vector3d(p->N.x, p->N.y, p->N.z),
                        values);
                }
                break;

              case ReflectionID:
                {
                    const ReflectionClosureParams *p = 
                        reinterpret_cast<const ReflectionClosureParams*>(c->data());

                    SpecularBRDFInputValues values;
                    linear_rgb_reflectance_to_spectrum(w,values.m_reflectance);
                    values.m_reflectance_alpha = Alpha(1.0);
                    values.m_reflectance_multiplier = 1.0;
                    add_closure<SpecularBRDFInputValues>(
                        static_cast<ClosureID>(c->id), 
                        w, 
                        Vector3d(p->N.x, p->N.y, p->N.z),
                        values);
                }
                break;

              case RefractionID:
                {
                    const RefractionClosureParams *p = 
                        reinterpret_cast<const RefractionClosureParams*>(c->data());

                    SpecularBTDFInputValues values;
                    linear_rgb_reflectance_to_spectrum(w,values.m_reflectance);
                    values.m_from_ior = p->from_ior;
                    values.m_to_ior = p->to_ior;
                    values.m_reflectance_alpha = Alpha(1.0);
                    values.m_reflectance_multiplier = 1.0;
                    add_closure<SpecularBTDFInputValues>(
                        static_cast<ClosureID>(c->id), 
                        w, 
                        Vector3d(p->N.x, p->N.y, p->N.z),
                        values);
                }
                break;

              case TranslucentID:
                {
                    const TranslucentClosureParams *p = 
                        reinterpret_cast<const TranslucentClosureParams*>(c->data());

                    DiffuseBTDFInputValues values;
                    linear_rgb_reflectance_to_spectrum(w,values.m_transmittance);
                    values.m_transmittance_alpha = Alpha(1.0);
                    values.m_transmittance_multiplier = 1.0;
                    add_closure<DiffuseBTDFInputValues>(
                        static_cast<ClosureID>(c->id), 
                        w, 
                        Vector3d(p->N.x, p->N.y, p->N.z),
                        values);
                }
                break;

              case EmissionID:
              case HoldoutID:
              case TransparentID:
                {
                    assert(!"Not implemented yet.");
                }
                break;

              assert_otherwise;
            }
        }
        break;
    }
}

template <typename InputValues>
void CompositeClosure::add_closure(
    const ClosureID             closure_type,
    const Color3f&              weight,
    const Vector3d&             normal,
    const InputValues&          params)
{
    do_add_closure<InputValues>(
        closure_type,
        weight,
        normal,
        false,
        Vector3d(0, 0, 0),
        params);
}

template <typename InputValues>
void CompositeClosure::add_closure(
    const ClosureID             closure_type,
    const Color3f&              weight,
    const Vector3d&             normal,
    const Vector3d&             tangent,
    const InputValues&          params)
{
    do_add_closure<InputValues>(
        closure_type,
        weight,
        normal,
        true,
        tangent,
        params);
}

template <typename InputValues>
void CompositeClosure::do_add_closure(
    const ClosureID             closure_type,
    const Color3f&              weight,
    const Vector3d&             normal,
    const bool                  has_tangent,
    const Vector3d&             tangent,
    const InputValues&          params)
{
    // make sure we have enough space;
    if (num_closures() >= MaxClosureEntries)
    {
        RENDERER_LOG_WARNING("maximum number of closures in OSL shadergroup exceeded; ignoring closure.");
        return;
    }

    assert(m_num_bytes + sizeof(InputValues) <= MaxPoolSize);

    // We use the luminance of the weight as the BSDF weight.
    const double w = luminance(weight);
    if (w <= 0.0)
    {
        // This can generate millions of warnings. It's better to disable it.
        //RENDERER_LOG_WARNING("closure with negative or zero weight found; ignoring closure.");
        return;
    }

    m_weights[m_num_closures] = w;
    m_normals[m_num_closures] = normalize(normal);
    m_has_tangent[m_num_closures] = has_tangent;

    if (has_tangent)
        m_tangents[m_num_closures] = normalize(tangent);

    m_closure_types[m_num_closures] = closure_type;
    new (m_pool + m_num_bytes) InputValues(params);
    m_input_values[m_num_closures] = m_pool + m_num_bytes;
    m_num_bytes += sizeof(InputValues);
    ++m_num_closures;
}

}   // namespace renderer

// We probably want to reuse OSL macros to declare closure params 
// and register the closures. We can use them only inside the OSL namespace.
OSL_NAMESPACE_ENTER

void register_appleseed_closures(OSL::ShadingSystem& shading_system)
{
    // Describe the memory layout of each closure type to the OSL runtime.
    const size_t MaxParams = 32;
    struct BuiltinClosures
    {
        const char*     name;
        int             id;
        ClosureParam    params[MaxParams];
    };

    static const BuiltinClosures builtins[] =
    {
        { "ashikhmin_shirley", AshikhminShirleyID, { CLOSURE_VECTOR_PARAM(AshikhminShirleyClosureParams, N),
                                                     CLOSURE_VECTOR_PARAM(AshikhminShirleyClosureParams, T),
                                                     CLOSURE_COLOR_PARAM(AshikhminShirleyClosureParams, kd),
                                                     CLOSURE_COLOR_PARAM(AshikhminShirleyClosureParams, ks),
                                                     CLOSURE_FLOAT_PARAM(AshikhminShirleyClosureParams, nu),
                                                     CLOSURE_FLOAT_PARAM(AshikhminShirleyClosureParams, nv),
                                                     CLOSURE_FINISH_PARAM(AshikhminShirleyClosureParams) } },

        { "diffuse", LambertID, { CLOSURE_VECTOR_PARAM(LambertClosureParams, N),
                                  CLOSURE_FINISH_PARAM(LambertClosureParams) } },

        { "emission", EmissionID, { CLOSURE_FINISH_PARAM(EmptyClosureParams) } },

        { "holdout", HoldoutID, { CLOSURE_FINISH_PARAM(EmptyClosureParams) } },
        
        { "microfacet_beckmann", MicrofacetBeckmannID, { CLOSURE_VECTOR_PARAM(MicrofacetBRDFClosureParams, N),
                                                         CLOSURE_FLOAT_PARAM(MicrofacetBRDFClosureParams, glossiness),
                                                         CLOSURE_FINISH_PARAM(MicrofacetBRDFClosureParams) } },

        { "microfacet_blinn", MicrofacetBlinnID, { CLOSURE_VECTOR_PARAM(MicrofacetBRDFClosureParams, N),
                                                   CLOSURE_FLOAT_PARAM(MicrofacetBRDFClosureParams, glossiness),
                                                   CLOSURE_FINISH_PARAM(MicrofacetBRDFClosureParams) } },

        { "microfacet_ggx", MicrofacetGGXID, { CLOSURE_VECTOR_PARAM(MicrofacetBRDFClosureParams, N),
                                               CLOSURE_FLOAT_PARAM(MicrofacetBRDFClosureParams, glossiness),
                                               CLOSURE_FINISH_PARAM(MicrofacetBRDFClosureParams) } },

        { "microfacet_ward", MicrofacetWardID, { CLOSURE_VECTOR_PARAM(MicrofacetBRDFClosureParams, N),
                                                 CLOSURE_FLOAT_PARAM(MicrofacetBRDFClosureParams, glossiness),
                                                 CLOSURE_FINISH_PARAM(MicrofacetBRDFClosureParams) } },

        { "reflection", ReflectionID, { CLOSURE_VECTOR_PARAM(ReflectionClosureParams, N),
                                        CLOSURE_FINISH_PARAM(ReflectionClosureParams) } },

        { "refraction", RefractionID, { CLOSURE_VECTOR_PARAM(RefractionClosureParams, N),
                                        CLOSURE_FLOAT_PARAM(RefractionClosureParams, from_ior),
                                        CLOSURE_FLOAT_PARAM(RefractionClosureParams, to_ior),
                                        CLOSURE_FINISH_PARAM(RefractionClosureParams) } },

        { "translucent", TranslucentID, { CLOSURE_VECTOR_PARAM(LambertClosureParams, N),
                                          CLOSURE_FINISH_PARAM(LambertClosureParams) } },

        { "transparency", TransparentID, { CLOSURE_FINISH_PARAM(EmptyClosureParams) } },

        { 0, 0, {} }    // mark end of the array
    };

    for (size_t i = 0; builtins[i].name != 0; ++i)
    {
        shading_system.register_closure(
            builtins[i].name,
            builtins[i].id,
            builtins[i].params,
            0,
            0);

        RENDERER_LOG_INFO("registered OSL closure %s.", builtins[i].name);
    }
}

OSL_NAMESPACE_EXIT

namespace renderer
{

void register_closures(OSL::ShadingSystem& shading_system)
{
    OSL::register_appleseed_closures(shading_system);
}

}   // namespace renderer
