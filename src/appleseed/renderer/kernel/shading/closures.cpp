
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/otherwise.h"

// OSL headers.
#include "foundation/platform/oslheaderguards.h"
BEGIN_OSL_INCLUDES
#include "OSL/genclosure.h"
#include "OSL/oslclosure.h"
END_OSL_INCLUDES

// Boost headers.
#include "boost/mpl/contains.hpp"
#include "boost/static_assert.hpp"

// Standard headers.
#include <algorithm>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace renderer
{
namespace
{
    //
    // Closure parameters.
    //

    struct EmptyClosureParams {};

    struct AshikhminShirleyBRDFClosureParams
    {
        OSL::Vec3       N;
        OSL::Vec3       T;
        float           nu;
        float           nv;
    };

    struct DebugClosureParams
    {
        OSL::ustring    tag;
    };

    struct DisneyBRDFClosureParams
    {
        OSL::Vec3       N;
        OSL::Vec3       T;
        OSL::Color3     base_color;
        float           subsurface;
        float           metallic;
        float           specular;
        float           specular_tint;
        float           anisotropic;
        float           roughness;
        float           sheen;
        float           sheen_tint;
        float           clearcoat;
        float           clearcoat_gloss;
    };

    struct DiffuseBSDFClosureParams
    {
        OSL::Vec3       N;
    };

    OSL::ustring beckmann_mdf_name("beckmann");
    OSL::ustring blinn_mdf_name("blinn");
    OSL::ustring ggx_mdf_name("ggx");

    struct MicrofacetClosureParams
    {
        OSL::ustring    dist;
        OSL::Vec3       N;
        OSL::Vec3       T;
        float           xalpha;
        float           yalpha;
        float           eta;
        int             refract;
    };

    struct OrenNayarBRDFClosureParams
    {
        OSL::Vec3       N;
        float           roughness;
    };

    struct ReflectionBRDFClosureParams
    {
        OSL::Vec3       N;
    };

    struct RefractionBTDFClosureParams
    {
        OSL::Vec3       N;
        float           eta;
    };

    struct VelvetBRDFClosureParams
    {
        OSL::Vec3   N;
        float       alpha;
    };
}


//
// CompositeSurfaceClosure class implementation.
//

BOOST_STATIC_ASSERT(sizeof(CompositeSurfaceClosure) <= InputEvaluator::DataSize);

CompositeSurfaceClosure::CompositeSurfaceClosure(
    const OSL::ClosureColor*    ci)
  : m_num_closures(0)
  , m_num_bytes(0)
{
    assert(is_aligned(m_pool, InputValuesAlignment));

    process_closure_tree(ci, Color3f(1.0f));

    const size_t closure_count = get_num_closures();
    if (closure_count > 0)
    {
        double total_weight = 0.0;
        for (size_t i = 0; i < closure_count; ++i)
        {
            total_weight += m_pdf_weights[i];
            m_cdf[i] = total_weight;
        }

        const double rcp_total_weight = 1.0 / total_weight;

        for (size_t i = 0; i < closure_count; ++i)
            m_pdf_weights[i] *= rcp_total_weight;

        for (size_t i = 0; i < closure_count - 1; ++i)
            m_cdf[i] *= rcp_total_weight;

        m_cdf[closure_count - 1] = 1.0;
    }
}

size_t CompositeSurfaceClosure::choose_closure(const double w) const
{
    assert(w >= 0.0);
    assert(w < 1.0);

    const double* i =
        upper_bound(
            m_cdf,
            m_cdf + get_num_closures(),
            w);

    assert(i < m_cdf + get_num_closures());

    return i - m_cdf;
}

void CompositeSurfaceClosure::process_closure_tree(
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
            const Color3f w = weight * Color3f(c->weight);
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
            const Color3f w = weight * Color3f(c->w);

            switch (c->id)
            {
              case AshikhminShirleyID:
                {
                    const AshikhminShirleyBRDFClosureParams* p =
                        reinterpret_cast<const AshikhminShirleyBRDFClosureParams*>(c->data());

                    OSLAshikhminBRDFInputValues values;
                    values.m_nu = max(p->nu, 0.01f);
                    values.m_nv = max(p->nv, 0.01f);

                    add_closure<OSLAshikhminBRDFInputValues>(
                        static_cast<ClosureID>(c->id),
                        w,
                        Vector3d(p->N),
                        Vector3d(p->T),
                        values);
                }
                break;

              case DisneyID:
                {
                    const DisneyBRDFClosureParams* p =
                        reinterpret_cast<const DisneyBRDFClosureParams*>(c->data());

                    DisneyBRDFInputValues values;
                    values.m_base_color = Color3f(p->base_color);
                    values.m_subsurface = saturate(p->subsurface);
                    values.m_metallic = saturate(p->metallic);
                    values.m_specular = max(p->specular, 0.0f);
                    values.m_specular_tint = saturate(p->specular_tint);
                    values.m_anisotropic = saturate(p->anisotropic);
                    values.m_roughness = clamp(p->roughness, 0.0001f, 1.0f);
                    values.m_sheen = saturate(p->sheen);
                    values.m_sheen_tint = saturate(p->sheen_tint);
                    values.m_clearcoat = max(p->clearcoat, 0.0f);
                    values.m_clearcoat_gloss = clamp(p->clearcoat_gloss, 0.0001f, 1.0f);
                    values.precompute_tint_color();

                    add_closure<DisneyBRDFInputValues>(
                        static_cast<ClosureID>(c->id),
                        w,
                        Vector3d(p->N),
                        Vector3d(p->T),
                        values);
                }
                break;

              case LambertID:
                {
                    const DiffuseBSDFClosureParams* p =
                        reinterpret_cast<const DiffuseBSDFClosureParams*>(c->data());

                    LambertianBRDFInputValues values;
                    values.m_reflectance.set(1.0f);
                    values.m_reflectance_multiplier = 1.0;

                    add_closure<LambertianBRDFInputValues>(
                        static_cast<ClosureID>(c->id),
                        w,
                        Vector3d(p->N),
                        values);
                }
                break;

              case MicrofacetID:
                {
                    const MicrofacetClosureParams* p =
                        reinterpret_cast<const MicrofacetClosureParams*>(c->data());

                    if (!p->refract)
                    {
                        OSLMicrofacetBRDFInputValues values;
                        values.m_ax = max(p->xalpha, 0.0001f);
                        values.m_ay = max(p->yalpha, 0.0001f);

                        if (p->dist == blinn_mdf_name)
                        {
                            add_closure<OSLMicrofacetBRDFInputValues>(
                                MicrofacetBlinnReflectionID,
                                w,
                                Vector3d(p->N),
                                Vector3d(p->T),
                                values);
                        }
                        else if (p->dist == ggx_mdf_name)
                        {
                            add_closure<OSLMicrofacetBRDFInputValues>(
                                MicrofacetGGXReflectionID,
                                w,
                                Vector3d(p->N),
                                Vector3d(p->T),
                                values);
                        }
                        else    // Beckmann by default
                        {
                            add_closure<OSLMicrofacetBRDFInputValues>(
                                MicrofacetBeckmannReflectionID,
                                w,
                                Vector3d(p->N),
                                Vector3d(p->T),
                                values);
                        }
                    }
                    else
                    {
                        // Assume one of the media is air.
                        double from_ior, to_ior;
                        if (p->eta > 1.0)
                        {
                            from_ior = 1.0;
                            to_ior = 1.0 / p->eta;
                        }
                        else
                        {
                            from_ior = p->eta;
                            to_ior = 1.0;
                        }

                        OSLMicrofacetBTDFInputValues values;
                        values.m_ax = max(p->xalpha, 0.0001f);
                        values.m_ay = max(p->yalpha, 0.0001f);
                        values.m_from_ior = from_ior;
                        values.m_to_ior = to_ior;

                        if (p->dist == ggx_mdf_name)
                        {
                            add_closure<OSLMicrofacetBTDFInputValues>(
                                MicrofacetGGXRefractionID,
                                w,
                                Vector3d(p->N),
                                Vector3d(p->T),
                                values);
                        }
                        else    // Beckmann by default
                        {
                            add_closure<OSLMicrofacetBTDFInputValues>(
                                MicrofacetBeckmannRefractionID,
                                w,
                                Vector3d(p->N),
                                Vector3d(p->T),
                                values);
                        }
                    }
                }
                break;

              case OrenNayarID:
                {
                    const OrenNayarBRDFClosureParams* p =
                        reinterpret_cast<const OrenNayarBRDFClosureParams*>(c->data());

                    OrenNayarBRDFInputValues values;
                    values.m_reflectance.set(1.0f);
                    values.m_reflectance_multiplier = 1.0;
                    values.m_roughness = max(p->roughness, 0.0f);

                    add_closure<OrenNayarBRDFInputValues>(
                        static_cast<ClosureID>(c->id),
                        w,
                        Vector3d(p->N),
                        values);
                }
                break;

              case ReflectionID:
                {
                    const ReflectionBRDFClosureParams* p =
                        reinterpret_cast<const ReflectionBRDFClosureParams*>(c->data());

                    SpecularBRDFInputValues values;
                    values.m_reflectance.set(1.0f);
                    values.m_reflectance_multiplier = 1.0;

                    add_closure<SpecularBRDFInputValues>(
                        static_cast<ClosureID>(c->id),
                        w,
                        Vector3d(p->N),
                        values);
                }
                break;

              case RefractionID:
                {
                    const RefractionBTDFClosureParams* p =
                        reinterpret_cast<const RefractionBTDFClosureParams*>(c->data());

                    // Assume on of the mediums is air.
                    double from_ior, to_ior;
                    if (p->eta > 1.0f)
                    {
                        from_ior = 1.0;
                        to_ior = 1.0f / p->eta;
                    }
                    else
                    {
                        from_ior = p->eta;
                        to_ior = 1.0;
                    }

                    SpecularBTDFInputValues values;
                    values.m_reflectance.set(0.0f);
                    values.m_reflectance_multiplier = 0.0;
                    values.m_transmittance.set(1.0f);
                    values.m_transmittance_multiplier = 1.0;
                    values.m_fresnel_multiplier = 0.0;
                    values.m_from_ior = from_ior;
                    values.m_to_ior = to_ior;

                    add_closure<SpecularBTDFInputValues>(
                        static_cast<ClosureID>(c->id),
                        w,
                        Vector3d(p->N),
                        values);
                }
                break;

              case TranslucentID:
                {
                    const DiffuseBSDFClosureParams* p =
                        reinterpret_cast<const DiffuseBSDFClosureParams*>(c->data());

                    DiffuseBTDFInputValues values;
                    values.m_transmittance.set(1.0f);
                    values.m_transmittance_multiplier = 1.0;

                    add_closure<DiffuseBTDFInputValues>(
                        static_cast<ClosureID>(c->id),
                        w,
                        Vector3d(p->N),
                        values);
                }
                break;

              case VelvetID:
              {
                  const VelvetBRDFClosureParams* p =
                      reinterpret_cast<const VelvetBRDFClosureParams*>(c->data());

                  VelvetBRDFInputValues values;
                  values.m_roughness = p->alpha;
                  values.m_roughness_multiplier = 1.0;
                  values.m_reflectance.set(1.0f);
                  values.m_reflectance_multiplier = 1.0;
                  values.m_fresnel_normal_reflectance = 1.0;
                  values.m_fresnel_multiplier = 1.0;

                  add_closure<VelvetBRDFInputValues>(
                      static_cast<ClosureID>(c->id),
                      w,
                      Vector3d(p->N),
                      values);
              }
              break;

            }
        }
        break;

      assert_otherwise;
    }
}

template <typename InputValues>
void CompositeSurfaceClosure::add_closure(
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
        Vector3d(0.0),
        params);
}

template <typename InputValues>
void CompositeSurfaceClosure::add_closure(
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
void CompositeSurfaceClosure::do_add_closure(
    const ClosureID             closure_type,
    const Color3f&              weight,
    const Vector3d&             normal,
    bool                        has_tangent,
    const Vector3d&             tangent,
    const InputValues&          params)
{
    // Check that InputValues is included in our type list.
    typedef typename boost::mpl::contains<InputValuesTypeList, InputValues>::type value_in_list;
    BOOST_STATIC_ASSERT(value_in_list::value);

    // Make sure we have enough space.
    if (get_num_closures() >= MaxClosureEntries)
    {
        RENDERER_LOG_WARNING("maximum number of closures in osl shader group exceeded; ignoring closure.");
        return;
    }

    assert(m_num_bytes + sizeof(InputValues) <= MaxPoolSize);

    // We use the luminance of the weight as the BSDF weight.
    const double w = luminance(weight);

    // Ignore zero or negative weights.
    if (w <= 0.0)
        return;

    m_pdf_weights[m_num_closures] = w;
    m_weights[m_num_closures] = weight;
    m_normals[m_num_closures] = normalize(normal);

    // If the tangent is zero, ignore it.
    // This can happen when using the isotropic microfacet closure overloads, for example.
    if (square_norm(tangent) == 0.0)
        has_tangent = false;

    m_has_tangent[m_num_closures] = has_tangent;

    if (has_tangent)
        m_tangents[m_num_closures] = normalize(tangent);

    m_closure_types[m_num_closures] = closure_type;

    char* values_ptr = m_pool + m_num_bytes;
    assert(is_aligned(values_ptr, InputValuesAlignment));
    new (values_ptr) InputValues(params);
    m_input_values[m_num_closures] = values_ptr;
    m_num_bytes += align(sizeof(InputValues), InputValuesAlignment);
    ++m_num_closures;
}


//
// CompositeEmissionClosure class implementation.
//

BOOST_STATIC_ASSERT(sizeof(CompositeEmissionClosure) <= InputEvaluator::DataSize);

CompositeEmissionClosure::CompositeEmissionClosure(
    const OSL::ClosureColor*    ci)
{
    m_total_weight = Color3f(0.0f);
    process_closure_tree(ci, Color3f(1.0f));

    const float max_comp = max_value(m_total_weight);

    if (max_comp != 0.0f)
    {
        m_total_weight /= max_comp;
        m_edf_values.m_radiance = m_total_weight;
        m_edf_values.m_radiance_multiplier = static_cast<double>(max_comp);
    }
    else
    {
        m_edf_values.m_radiance.set(0.0f);
        m_edf_values.m_radiance_multiplier = 1.0;
    }
}

void CompositeEmissionClosure::process_closure_tree(
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
            const Color3f w = weight * Color3f(c->weight);
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
            const Color3f w = weight * Color3f(c->w);

            if (c->id == EmissionID)
                m_total_weight += w;
        }
        break;

      assert_otherwise;
    }
}


//
// Utility functions implementation.
//

namespace
{
    Color3f do_process_closure_id_tree(
        const OSL::ClosureColor*    closure,
        const int                   closure_id)
    {
        if (closure)
        {
            switch (closure->type)
            {
              case OSL::ClosureColor::MUL:
                {
                    const OSL::ClosureMul* c = reinterpret_cast<const OSL::ClosureMul*>(closure);
                    return Color3f(c->weight) * do_process_closure_id_tree(c->closure, closure_id);
                }
                break;

              case OSL::ClosureColor::ADD:
                {
                    const OSL::ClosureAdd* c = reinterpret_cast<const OSL::ClosureAdd*>(closure);
                    return do_process_closure_id_tree(c->closureA, closure_id) +
                           do_process_closure_id_tree(c->closureB, closure_id);
                }
                break;

              case OSL::ClosureColor::COMPONENT:
                {
                    const OSL::ClosureComponent* c = reinterpret_cast<const OSL::ClosureComponent*>(closure);
                    return c->id == closure_id ? Color3f(c->w) : Color3f(0.0f);
                }
                break;

              assert_otherwise;
            }
        }

        return Color3f(0.0f);
    }
}

void process_transparency_tree(const OSL::ClosureColor* ci, Alpha& alpha)
{
    // Convert from transparency to opacity.
    const float transparency = saturate(luminance(do_process_closure_id_tree(ci, TransparentID)));
    alpha.set(1.0f - transparency);
}

float process_holdout_tree(const OSL::ClosureColor* ci)
{
    return saturate(luminance(do_process_closure_id_tree(ci, HoldoutID)));
}

Color3f process_background_tree(const OSL::ClosureColor* ci)
{
    return do_process_closure_id_tree(ci, BackgroundID);
}

}   // namespace renderer

// We want to reuse OSL macros to declare closure params and register the closures.
// We can use them only inside the OSL namespace.
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
        { "as_ashikhmin_shirley", AshikhminShirleyID, { CLOSURE_VECTOR_PARAM(AshikhminShirleyBRDFClosureParams, N),
                                                        CLOSURE_VECTOR_PARAM(AshikhminShirleyBRDFClosureParams, T),
                                                        CLOSURE_FLOAT_PARAM(AshikhminShirleyBRDFClosureParams, nu),
                                                        CLOSURE_FLOAT_PARAM(AshikhminShirleyBRDFClosureParams, nv),
                                                        CLOSURE_FINISH_PARAM(AshikhminShirleyBRDFClosureParams) } },

        { "as_disney", DisneyID, { CLOSURE_VECTOR_PARAM(DisneyBRDFClosureParams, N),
                                   CLOSURE_VECTOR_PARAM(DisneyBRDFClosureParams, T),
                                   CLOSURE_COLOR_PARAM(DisneyBRDFClosureParams, base_color),
                                   CLOSURE_FLOAT_PARAM(DisneyBRDFClosureParams, subsurface),
                                   CLOSURE_FLOAT_PARAM(DisneyBRDFClosureParams, metallic),
                                   CLOSURE_FLOAT_PARAM(DisneyBRDFClosureParams, specular),
                                   CLOSURE_FLOAT_PARAM(DisneyBRDFClosureParams, specular_tint),
                                   CLOSURE_FLOAT_PARAM(DisneyBRDFClosureParams, anisotropic),
                                   CLOSURE_FLOAT_PARAM(DisneyBRDFClosureParams, roughness),
                                   CLOSURE_FLOAT_PARAM(DisneyBRDFClosureParams, sheen),
                                   CLOSURE_FLOAT_PARAM(DisneyBRDFClosureParams, sheen_tint),
                                   CLOSURE_FLOAT_PARAM(DisneyBRDFClosureParams, clearcoat),
                                   CLOSURE_FLOAT_PARAM(DisneyBRDFClosureParams, clearcoat_gloss),
                                   CLOSURE_FINISH_PARAM(DisneyBRDFClosureParams) } },

        { "as_velvet", VelvetID, { CLOSURE_VECTOR_PARAM(VelvetBRDFClosureParams, N),
                                   CLOSURE_FLOAT_PARAM(VelvetBRDFClosureParams, alpha),
                                   CLOSURE_FINISH_PARAM(VelvetBRDFClosureParams) } },

        { "oren_nayar", OrenNayarID, { CLOSURE_VECTOR_PARAM(OrenNayarBRDFClosureParams, N),
                                       CLOSURE_FLOAT_PARAM(OrenNayarBRDFClosureParams, roughness),
                                       CLOSURE_FINISH_PARAM(OrenNayarBRDFClosureParams) } },

        { "background", BackgroundID, { CLOSURE_FINISH_PARAM(EmptyClosureParams) } },

        { "debug", DebugID, { CLOSURE_STRING_PARAM(DebugClosureParams, tag),
                              CLOSURE_FINISH_PARAM(DebugClosureParams) } },

        { "diffuse", LambertID, { CLOSURE_VECTOR_PARAM(DiffuseBSDFClosureParams, N),
                                  CLOSURE_FINISH_PARAM(DiffuseBSDFClosureParams) } },

        { "emission", EmissionID, { CLOSURE_FINISH_PARAM(EmptyClosureParams) } },

        { "holdout", HoldoutID, { CLOSURE_FINISH_PARAM(EmptyClosureParams) } },

        { "microfacet", MicrofacetID, { CLOSURE_STRING_PARAM(MicrofacetClosureParams, dist),
                                        CLOSURE_VECTOR_PARAM(MicrofacetClosureParams, N),
                                        CLOSURE_VECTOR_PARAM(MicrofacetClosureParams, T),
                                        CLOSURE_FLOAT_PARAM(MicrofacetClosureParams, xalpha),
                                        CLOSURE_FLOAT_PARAM(MicrofacetClosureParams, yalpha),
                                        CLOSURE_FLOAT_PARAM(MicrofacetClosureParams, eta),
                                        CLOSURE_INT_PARAM(MicrofacetClosureParams, refract),
                                        CLOSURE_FINISH_PARAM(MicrofacetClosureParams) } },

        { "reflection", ReflectionID, { CLOSURE_VECTOR_PARAM(ReflectionBRDFClosureParams, N),
                                        CLOSURE_FINISH_PARAM(ReflectionBRDFClosureParams) } },

        { "refraction", RefractionID, { CLOSURE_VECTOR_PARAM(RefractionBTDFClosureParams, N),
                                        CLOSURE_FLOAT_PARAM(RefractionBTDFClosureParams, eta),
                                        CLOSURE_FINISH_PARAM(RefractionBTDFClosureParams) } },

        { "translucent", TranslucentID, { CLOSURE_VECTOR_PARAM(DiffuseBSDFClosureParams, N),
                                          CLOSURE_FINISH_PARAM(DiffuseBSDFClosureParams) } },

        { "transparent", TransparentID, { CLOSURE_FINISH_PARAM(EmptyClosureParams) } },

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

        RENDERER_LOG_INFO("registered osl closure %s.", builtins[i].name);
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
