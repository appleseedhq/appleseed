
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
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/otherwise.h"

// OSL headers.
#include "OSL/genclosure.h"
#include "OSL/oslclosure.h"

// boost headers.
#include <boost/mpl/contains.hpp>
#include <boost/static_assert.hpp>

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
        OSL::Vec3       N;
        OSL::Vec3       T;
        float           kd;
        OSL::Color3     Cd;
        float           ks;
        OSL::Color3     Cs;
        float           nu;
        float           nv;
    };

    struct DebugClosureParams
    {
        OSL::ustring    tag;
    };

    struct EmissionClosureParams
    {
        float           inner_angle;
        float           outer_angle;
    };

    struct LambertClosureParams
    {
        OSL::Vec3       N;
    };

    struct OrenNayarBRDFClosureParams
    {
        OSL::Vec3       N;
        float           roughness;
    };

    struct ReflectionClosureParams
    {
        OSL::Vec3       N;
    };
    
    struct RefractionClosureParams
    {
        OSL::Vec3       N;
        float           from_ior;
        float           to_ior;
    };

    struct TranslucentClosureParams
    {
        OSL::Vec3       N;
    };

    OSL::ustring beckmann_mdf_name("beckmann");
    OSL::ustring blinn_mdf_name("blinn");
    OSL::ustring ggx_mdf_name("ggx");

    struct MicrofacetBSDFClosureParams
    {
        OSL::ustring    dist;
        OSL::Vec3       N;
        OSL::Vec3       T;
        float           xalpha;
        float           yalpha;
        float           eta;
        int             refract;
    };
}

BOOST_STATIC_ASSERT(sizeof(CompositeClosure) <= InputEvaluator::DataSize);

//
// CompositeClosure class implementation.
//

CompositeClosure::CompositeClosure(
    const OSL::ClosureColor* ci)
  : m_num_closures(0)
  , m_num_bytes(0)
{
    assert(is_aligned(m_pool, InputValuesAlignment));

    process_closure_tree(ci, Color3f(1.0f));

    if (get_num_closures())
        m_cdf.prepare();
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
                    const AshikhminShirleyClosureParams* p =
                        reinterpret_cast<const AshikhminShirleyClosureParams*>(c->data());

                    AshikminBRDFInputValues values;

                    linear_rgb_reflectance_to_spectrum_unclamped(Color3f(p->Cd), values.m_rd);
                    values.m_rd_multiplier = p->kd;

                    linear_rgb_reflectance_to_spectrum_unclamped(Color3f(p->Cs), values.m_rg);
                    values.m_rg_multiplier = p->ks;
                    values.m_fr_multiplier = 1.0;
                    values.m_nu = p->nu;
                    values.m_nv = p->nv;

                    add_closure<AshikminBRDFInputValues>(
                        static_cast<ClosureID>(c->id),
                        w,
                        Vector3d(p->N),
                        Vector3d(p->T),
                        values);
                }
                break;

              case LambertID:
                {
                    const LambertClosureParams* p =
                        reinterpret_cast<const LambertClosureParams*>(c->data());

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
                    const MicrofacetBSDFClosureParams* p =
                        reinterpret_cast<const MicrofacetBSDFClosureParams*>(c->data());

                    if (p->refract)
                    {
                        // TODO: implement this
                        // for now, we ignore Microfacet BTDFs
                    }
                    else
                    {
                        Microfacet2BRDFInputValues values;
                        values.m_ax = p->xalpha;
                        values.m_ay = p->yalpha;
                        values.m_eta = p->eta;

                        if (p->dist == blinn_mdf_name)
                        {
                            add_closure<Microfacet2BRDFInputValues>(
                                MicrofacetBlinnReflectionID,
                                w,
                                Vector3d(p->N),
                                Vector3d(p->T),
                                values);
                        }
                        else if (p->dist == ggx_mdf_name)
                        {
                            add_closure<Microfacet2BRDFInputValues>(
                                MicrofacetGGXReflectionID,
                                w,
                                Vector3d(p->N),
                                Vector3d(p->T),
                                values);
                        }
                        else // beckmann by default
                        {
                            add_closure<Microfacet2BRDFInputValues>(
                                MicrofacetBeckmannReflectionID,
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
                    values.m_roughness = p->roughness;

                    add_closure<OrenNayarBRDFInputValues>(
                        static_cast<ClosureID>(c->id),
                        w,
                        Vector3d(p->N),
                        values);
                }
                break;

              case ReflectionID:
                {
                    const ReflectionClosureParams* p =
                        reinterpret_cast<const ReflectionClosureParams*>(c->data());

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
                    const RefractionClosureParams* p =
                        reinterpret_cast<const RefractionClosureParams*>(c->data());

                    SpecularBTDFInputValues values;
                    values.m_reflectance.set(0.0f);
                    values.m_reflectance_multiplier = 0.0;
                    values.m_transmittance.set(1.0f);
                    values.m_transmittance_multiplier = 1.0;
                    values.m_fresnel_multiplier = 0.0;
                    values.m_from_ior = p->from_ior;
                    values.m_to_ior = p->to_ior;

                    add_closure<SpecularBTDFInputValues>(
                        static_cast<ClosureID>(c->id),
                        w,
                        Vector3d(p->N),
                        values);
                }
                break;

              case TranslucentID:
                {
                    const TranslucentClosureParams* p =
                        reinterpret_cast<const TranslucentClosureParams*>(c->data());

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

              // These are handled in another place.
              case BackgroundID:
              case DebugID:
              case EmissionID:
              case HoldoutID:
              case TransparentID:
                break;

              assert_otherwise;
            }
        }
        break;

      assert_otherwise;
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
        Vector3d(0.0),
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
    bool                        has_tangent,
    const Vector3d&             tangent,
    const InputValues&          params)
{
    // Check that InputValues is included in our type list.
    typedef typename boost::mpl::contains<InputValuesTypeList,InputValues>::type value_in_list;
    BOOST_STATIC_ASSERT( value_in_list::value);

    // Make sure we have enough space.
    if (get_num_closures() >= MaxClosureEntries)
    {
        RENDERER_LOG_WARNING("maximum number of closures in OSL shadergroup exceeded; ignoring closure.");
        return;
    }

    assert(m_num_bytes + sizeof(InputValues) <= MaxPoolSize);

    // We use the luminance of the weight as the BSDF weight.
    const double w = luminance(weight);

    // Ignore zero or negative weights.
    if (w <= 0.0)
        return;

    m_cdf.insert(get_num_closures(), w);
    linear_rgb_reflectance_to_spectrum_unclamped(weight, m_spectrum_multipliers[m_num_closures]);
    m_normals[m_num_closures] = normalize(normal);

    // If the tangent is zero, ignore it.
    // This can happen when using the isotropic microfacet closure overload, for example.
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

namespace
{

    Color3f do_process_closure_id_tree(
        const OSL::ClosureColor*    closure,
        const int                   closure_id)
    {
        if (closure == 0)
            return Color3f(0.0f);

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

                if (c->id == closure_id)
                    return Color3f(c->w);

                return Color3f(0.0f);
            }
            break;

          assert_otherwise;
        }
    }

}

void process_transparency_tree(const OSL::ClosureColor* ci, Alpha& alpha)
{
    // Convert from transparency to opacity.
    float transp = luminance(do_process_closure_id_tree(ci, TransparentID));
    alpha.set(1.0f - clamp(transp, 0.0f, 1.0f));
}

float process_holdout_tree(const OSL::ClosureColor* ci)
{
    return clamp(luminance(do_process_closure_id_tree(ci, HoldoutID)), 0.0f, 1.0f);
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
        { "as_ashikhmin_shirley", AshikhminShirleyID, { CLOSURE_VECTOR_PARAM(AshikhminShirleyClosureParams, N),
                                                        CLOSURE_VECTOR_PARAM(AshikhminShirleyClosureParams, T),
                                                        CLOSURE_FLOAT_PARAM(AshikhminShirleyClosureParams, kd),
                                                        CLOSURE_COLOR_PARAM(AshikhminShirleyClosureParams, Cd),
                                                        CLOSURE_FLOAT_PARAM(AshikhminShirleyClosureParams, ks),
                                                        CLOSURE_COLOR_PARAM(AshikhminShirleyClosureParams, Cs),
                                                        CLOSURE_FLOAT_PARAM(AshikhminShirleyClosureParams, nu),
                                                        CLOSURE_FLOAT_PARAM(AshikhminShirleyClosureParams, nv),
                                                        CLOSURE_FINISH_PARAM(AshikhminShirleyClosureParams) } },

        { "as_oren_nayar", OrenNayarID, { CLOSURE_VECTOR_PARAM(OrenNayarBRDFClosureParams, N),
                                          CLOSURE_FLOAT_PARAM(OrenNayarBRDFClosureParams, roughness),
                                          CLOSURE_FINISH_PARAM(OrenNayarBRDFClosureParams) } },

        { "background", BackgroundID, { CLOSURE_FINISH_PARAM(EmptyClosureParams) } },

        { "debug", DebugID, { CLOSURE_STRING_PARAM(DebugClosureParams, tag),
                              CLOSURE_FINISH_PARAM(DebugClosureParams) } },

        { "diffuse", LambertID, { CLOSURE_VECTOR_PARAM(LambertClosureParams, N),
                                  CLOSURE_FINISH_PARAM(LambertClosureParams) } },

        { "emission", EmissionID, { CLOSURE_FLOAT_PARAM(EmissionClosureParams, inner_angle),
                                    CLOSURE_FLOAT_PARAM(EmissionClosureParams, outer_angle),
                                    CLOSURE_FINISH_PARAM(EmissionClosureParams) } },

        { "holdout", HoldoutID, { CLOSURE_FINISH_PARAM(EmptyClosureParams) } },

        { "microfacet", MicrofacetID, { CLOSURE_STRING_PARAM(MicrofacetBSDFClosureParams, dist),
                                        CLOSURE_VECTOR_PARAM(MicrofacetBSDFClosureParams, N),
                                        CLOSURE_VECTOR_PARAM(MicrofacetBSDFClosureParams, T),
                                        CLOSURE_FLOAT_PARAM(MicrofacetBSDFClosureParams, xalpha),
                                        CLOSURE_FLOAT_PARAM(MicrofacetBSDFClosureParams, yalpha),
                                        CLOSURE_FLOAT_PARAM(MicrofacetBSDFClosureParams, eta),
                                        CLOSURE_FINISH_PARAM(MicrofacetBSDFClosureParams) } },
        
        { "reflection", ReflectionID, { CLOSURE_VECTOR_PARAM(ReflectionClosureParams, N),
                                        CLOSURE_FINISH_PARAM(ReflectionClosureParams) } },

        { "refraction", RefractionID, { CLOSURE_VECTOR_PARAM(RefractionClosureParams, N),
                                        CLOSURE_FLOAT_PARAM(RefractionClosureParams, from_ior),
                                        CLOSURE_FLOAT_PARAM(RefractionClosureParams, to_ior),
                                        CLOSURE_FINISH_PARAM(RefractionClosureParams) } },

        { "translucent", TranslucentID, { CLOSURE_VECTOR_PARAM(LambertClosureParams, N),
                                          CLOSURE_FINISH_PARAM(LambertClosureParams) } },

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
