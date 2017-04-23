
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/bsdf/alsurfacelayerbrdf.h"
#include "renderer/modeling/bsdf/ashikhminbrdf.h"
#include "renderer/modeling/bsdf/blinnbrdf.h"
#include "renderer/modeling/bsdf/diffusebtdf.h"
#include "renderer/modeling/bsdf/disneybrdf.h"
#include "renderer/modeling/bsdf/glassbsdf.h"
#include "renderer/modeling/bsdf/glossybrdf.h"
#include "renderer/modeling/bsdf/metalbrdf.h"
#include "renderer/modeling/bsdf/orennayarbrdf.h"
#include "renderer/modeling/bsdf/sheenbrdf.h"
#include "renderer/modeling/bssrdf/dipolebssrdf.h"
#include "renderer/modeling/bssrdf/directionaldipolebssrdf.h"
#include "renderer/modeling/bssrdf/gaussianbssrdf.h"
#ifdef APPLESEED_WITH_NORMALIZED_DIFFUSION_BSSRDF
#include "renderer/modeling/bssrdf/normalizeddiffusionbssrdf.h"
#endif
#include "renderer/modeling/edf/diffuseedf.h"

// appleseed.foundation headers.
#include "foundation/math/cdf.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/arena.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/otherwise.h"

// OSL headers.
#include "foundation/platform/oslheaderguards.h"
BEGIN_OSL_INCLUDES
#include "OSL/genclosure.h"
#include "OSL/oslclosure.h"
#include "OSL/oslversion.h"
END_OSL_INCLUDES

// Standard headers.
#include <algorithm>

using namespace foundation;
using namespace renderer;
using namespace std;

using OSL::TypeDesc;

namespace renderer
{
namespace
{

    //
    // Global ustrings.
    //

    const OIIO::ustring g_beckmann_str("beckmann");
    const OIIO::ustring g_ggx_str("ggx");
    const OIIO::ustring g_std_str("std");

    const OIIO::ustring g_standard_dipole_profile_str("standard_dipole");
    const OIIO::ustring g_better_dipole_profile_str("better_dipole");
    const OIIO::ustring g_directional_dipole_profile_str("directional_dipole");
    const OIIO::ustring g_normalized_diffusion_profile_str("normalized_diffusion");
    const OIIO::ustring g_gaussian_profile_str("gaussian");

    //
    // Closure functions.
    //

    typedef void(*convert_closure_fun)(
        CompositeSurfaceClosure&    composite_closure,
        const Basis3f&              shading_basis,
        const void*                 osl_params,
        const Color3f&              weight,
        Arena&                      arena);

    convert_closure_fun g_closure_convert_funs[NumClosuresIDs];

    void convert_closure_nop(
        CompositeSurfaceClosure&    composite_closure,
        const Basis3f&              shading_basis,
        const void*                 osl_params,
        const Color3f&              weight,
        Arena&                      arena)
    {
    }

    //
    // Closures.
    //

    struct AshikhminShirleyClosure
    {
        struct Params
        {
            OSL::Vec3   N;
            OSL::Vec3   T;
            OSL::Color3 diffuse_reflectance;
            OSL::Color3 glossy_reflectance;
            float       exponent_u;
            float       exponent_v;
            float       fresnel_multiplier;
        };

        static const char* name()
        {
            return "as_ashikhmin_shirley";
        }

        static ClosureID id()
        {
            return AshikhminShirleyID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_VECTOR_PARAM(Params, T),
                CLOSURE_COLOR_PARAM(Params, diffuse_reflectance),
                CLOSURE_COLOR_PARAM(Params, glossy_reflectance),
                CLOSURE_FLOAT_PARAM(Params, exponent_u),
                CLOSURE_FLOAT_PARAM(Params, exponent_v),
                CLOSURE_FLOAT_PARAM(Params, fresnel_multiplier),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            AshikhminBRDFInputValues* values =
                composite_closure.add_closure<AshikhminBRDFInputValues>(
                    id(),
                    shading_basis,
                    weight,
                    p->N,
                    p->T,
                    arena);

            values->m_rd = Color3f(p->diffuse_reflectance);
            values->m_rd_multiplier = 1.0f;
            values->m_rg = Color3f(p->glossy_reflectance);
            values->m_rg_multiplier = 1.0f;
            values->m_nu = max(p->exponent_u, 0.01f);
            values->m_nv = max(p->exponent_v, 0.01f);
            values->m_fr_multiplier = p->fresnel_multiplier;
        }
    };

    struct BackgroundClosure
    {
        struct Params
        {
        };

        static const char* name()
        {
            return "background";
        }

        static ClosureID id()
        {
            return BackgroundID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);
        }
    };

    struct BlinnClosure
    {
        struct Params
        {
            OSL::Vec3       N;
            float           exponent;
            float           ior;
        };

        static const char* name()
        {
            return "as_blinn";
        }

        static ClosureID id()
        {
            return BlinnID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FLOAT_PARAM(Params, exponent),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            BlinnBRDFInputValues* values =
                composite_closure.add_closure<BlinnBRDFInputValues>(
                    BlinnID,
                    shading_basis,
                    weight,
                    p->N,
                    arena);

            values->m_exponent = max(p->exponent, 0.001f);
            values->m_ior = max(p->ior, 0.001f);
        }
    };

    struct DebugClosure
    {
        struct Params
        {
            OSL::ustring tag;
        };

        static const char* name()
        {
            return "debug";
        }

        static ClosureID id()
        {
            return DebugID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_STRING_PARAM(Params, tag),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);
        }
    };

    struct DiffuseClosure
    {
        struct Params
        {
            OSL::Vec3   N;
        };

        static const char* name()
        {
            return "diffuse";
        }

        static ClosureID id()
        {
            return DiffuseID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            OrenNayarBRDFInputValues* values =
                composite_closure.add_closure<OrenNayarBRDFInputValues>(
                    OrenNayarID,
                    shading_basis,
                    weight,
                    p->N,
                    arena);

            values->m_reflectance.set(1.0f);
            values->m_reflectance_multiplier = 1.0f;
            values->m_roughness = 0.0f;
        }
    };

    struct DisneyClosure
    {
        struct Params
        {
            OSL::Vec3   N;
            OSL::Vec3   T;
            OSL::Color3 base_color;
            float       subsurface;
            float       metallic;
            float       specular;
            float       specular_tint;
            float       anisotropic;
            float       roughness;
            float       sheen;
            float       sheen_tint;
            float       clearcoat;
            float       clearcoat_gloss;
        };

        static const char* name()
        {
            return "as_disney";
        }

        static ClosureID id()
        {
            return DisneyID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_VECTOR_PARAM(Params, T),
                CLOSURE_COLOR_PARAM(Params, base_color),
                CLOSURE_FLOAT_PARAM(Params, subsurface),
                CLOSURE_FLOAT_PARAM(Params, metallic),
                CLOSURE_FLOAT_PARAM(Params, specular),
                CLOSURE_FLOAT_PARAM(Params, specular_tint),
                CLOSURE_FLOAT_PARAM(Params, anisotropic),
                CLOSURE_FLOAT_PARAM(Params, roughness),
                CLOSURE_FLOAT_PARAM(Params, sheen),
                CLOSURE_FLOAT_PARAM(Params, sheen_tint),
                CLOSURE_FLOAT_PARAM(Params, clearcoat),
                CLOSURE_FLOAT_PARAM(Params, clearcoat_gloss),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            DisneyBRDFInputValues* values =
                composite_closure.add_closure<DisneyBRDFInputValues>(
                    id(),
                    shading_basis,
                    weight,
                    p->N,
                    p->T,
                    arena);

            values->m_base_color = Color3f(p->base_color);
            values->m_subsurface = saturate(p->subsurface);
            values->m_metallic = saturate(p->metallic);
            values->m_specular = max(p->specular, 0.0f);
            values->m_specular_tint = saturate(p->specular_tint);
            values->m_anisotropic = clamp(p->anisotropic, -1.0f, 1.0f);
            values->m_roughness = clamp(p->roughness, 0.0001f, 1.0f);
            values->m_sheen = saturate(p->sheen);
            values->m_sheen_tint = saturate(p->sheen_tint);
            values->m_clearcoat = max(p->clearcoat, 0.0f);
            values->m_clearcoat_gloss = clamp(p->clearcoat_gloss, 0.0001f, 1.0f);
        }
    };

    struct EmissionClosure
    {
        struct Params
        {
        };

        static const char* name()
        {
            return "emission";
        }

        static ClosureID id()
        {
            return EmissionID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);
        }

        static void convert_closure(
            CompositeEmissionClosure&   composite_closure,
            const void*                 osl_params,
            const Color3f&              weight,
            const float                 max_weight_component,
            Arena&                      arena)
        {
            DiffuseEDFInputValues* values =
                composite_closure.add_closure<DiffuseEDFInputValues>(
                    id(),
                    weight,
                    max_weight_component,
                    arena);

            values->m_radiance.set_intent(Spectrum::Illuminance);
            values->m_radiance = weight / max_weight_component;
            values->m_radiance_multiplier = max_weight_component;
        }
    };

    struct GlassClosure
    {
        struct Params
        {
            OSL::ustring    dist;
            OSL::Vec3       N;
            OSL::Vec3       T;
            OSL::Color3     surface_transmittance;
            OSL::Color3     reflection_tint;
            OSL::Color3     refraction_tint;
            float           roughness;
            float           highlight_falloff;
            float           anisotropy;
            float           ior;
            OSL::Color3     volume_transmittance;
            float           volume_transmittance_distance;
        };

        static const char* name()
        {
            return "as_glass";
        }

        static ClosureID id()
        {
            return GlassID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_STRING_PARAM(Params, dist),
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_VECTOR_PARAM(Params, T),
                CLOSURE_COLOR_PARAM(Params, surface_transmittance),
                CLOSURE_COLOR_PARAM(Params, reflection_tint),
                CLOSURE_COLOR_PARAM(Params, refraction_tint),
                CLOSURE_FLOAT_PARAM(Params, roughness),
                CLOSURE_FLOAT_PARAM(Params, highlight_falloff),
                CLOSURE_FLOAT_PARAM(Params, anisotropy),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_COLOR_PARAM(Params, volume_transmittance),
                CLOSURE_FLOAT_PARAM(Params, volume_transmittance_distance),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            GlassBSDFInputValues* values;
            ClosureID cid;

            if (p->dist == g_ggx_str)
                cid = GlassGGXID;
            else if (p->dist == g_beckmann_str)
                cid = GlassBeckmannID;
            else if (p->dist == g_std_str)
                cid = GlassSTDID;
            else
            {
                string msg("invalid microfacet distribution function: ");
                msg += p->dist.c_str();
                throw ExceptionOSLRuntimeError(msg.c_str());
            }

            values =
                composite_closure.add_closure<GlassBSDFInputValues>(
                    cid,
                    shading_basis,
                    weight,
                    p->N,
                    p->T,
                    arena);

            values->m_surface_transmittance = Color3f(p->surface_transmittance);
            values->m_surface_transmittance_multiplier = 1.0f;
            values->m_reflection_tint = Color3f(p->reflection_tint);
            values->m_refraction_tint = Color3f(p->refraction_tint);
            values->m_roughness = max(p->roughness, 0.0001f);
            values->m_highlight_falloff = saturate(p->highlight_falloff);
            values->m_anisotropy = clamp(p->anisotropy, -1.0f, 1.0f);
            values->m_ior = max(p->ior, 0.001f);
            values->m_volume_transmittance = Color3f(p->volume_transmittance);
            values->m_volume_transmittance_distance = p->volume_transmittance_distance;
            composite_closure.add_ior(weight, values->m_ior);
        }
    };

    struct GlossyClosure
    {
        struct Params
        {
            OSL::ustring    dist;
            OSL::Vec3       N;
            OSL::Vec3       T;
            float           roughness;
            float           highlight_falloff;
            float           anisotropy;
            float           ior;
        };

        static const char* name()
        {
            return "as_glossy";
        }

        static ClosureID id()
        {
            return GlossyID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_STRING_PARAM(Params, dist),
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_VECTOR_PARAM(Params, T),
                CLOSURE_FLOAT_PARAM(Params, roughness),
                CLOSURE_FLOAT_PARAM(Params, highlight_falloff),
                CLOSURE_FLOAT_PARAM(Params, anisotropy),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            GlossyBRDFInputValues* values;
            ClosureID cid;

            if (p->dist == g_ggx_str)
                cid = GlossyGGXID;
            else if (p->dist == g_beckmann_str)
                cid = GlossyBeckmannID;
            else if (p->dist == g_std_str)
                cid = GlossySTDID;
            else
            {
                string msg("invalid microfacet distribution function: ");
                msg += p->dist.c_str();
                throw ExceptionOSLRuntimeError(msg.c_str());
            }

            values =
                composite_closure.add_closure<GlossyBRDFInputValues>(
                    cid,
                    shading_basis,
                    weight,
                    p->N,
                    p->T,
                    arena);

            values->m_reflectance.set(1.0f);
            values->m_reflectance_multiplier = 1.0f;
            values->m_roughness = max(p->roughness, 0.0f);
            values->m_highlight_falloff = saturate(p->highlight_falloff);
            values->m_anisotropy = clamp(p->anisotropy, -1.0f, 1.0f);
            values->m_ior = max(p->ior, 0.001f);
        }
    };

    struct HoldoutClosure
    {
        struct Params
        {
        };

        static const char* name()
        {
            return "holdout";
        }

        static ClosureID id()
        {
            return HoldoutID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);
        }
    };

    struct MetalClosure
    {
        struct Params
        {
            OSL::ustring    dist;
            OSL::Vec3       N;
            OSL::Vec3       T;
            OSL::Color3     normal_reflectance;
            OSL::Color3     edge_tint;
            float           roughness;
            float           highlight_falloff;
            float           anisotropy;
        };

        static const char* name()
        {
            return "as_metal";
        }

        static ClosureID id()
        {
            return MetalID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_STRING_PARAM(Params, dist),
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_VECTOR_PARAM(Params, T),
                CLOSURE_COLOR_PARAM(Params, normal_reflectance),
                CLOSURE_COLOR_PARAM(Params, edge_tint),
                CLOSURE_FLOAT_PARAM(Params, roughness),
                CLOSURE_FLOAT_PARAM(Params, highlight_falloff),
                CLOSURE_FLOAT_PARAM(Params, anisotropy),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            MetalBRDFInputValues* values;
            ClosureID cid;

            if (p->dist == g_ggx_str)
                cid = MetalGGXID;
            else if (p->dist == g_beckmann_str)
                cid = MetalBeckmannID;
            else if (p->dist == g_std_str)
                cid = MetalSTDID;
            else
            {
                string msg("invalid microfacet distribution function: ");
                msg += p->dist.c_str();
                throw ExceptionOSLRuntimeError(msg.c_str());
            }

            values =
                composite_closure.add_closure<MetalBRDFInputValues>(
                    cid,
                    shading_basis,
                    weight,
                    p->N,
                    p->T,
                    arena);

            values->m_normal_reflectance = Color3f(p->normal_reflectance);
            values->m_edge_tint = Color3f(p->edge_tint);
            values->m_reflectance_multiplier = 1.0f;
            values->m_roughness = max(p->roughness, 0.0f);
            values->m_highlight_falloff = saturate(p->highlight_falloff);
            values->m_anisotropy = clamp(p->anisotropy, -1.0f, 1.0f);
        }
    };

    struct OrenNayarClosure
    {
        struct Params
        {
            OSL::Vec3   N;
            float       roughness;
        };

        static const char* name()
        {
            return "oren_nayar";
        }

        static ClosureID id()
        {
            return OrenNayarID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FLOAT_PARAM(Params, roughness),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            OrenNayarBRDFInputValues* values =
                composite_closure.add_closure<OrenNayarBRDFInputValues>(
                    id(),
                    shading_basis,
                    weight,
                    p->N,
                    arena);

            values->m_reflectance.set(1.0f);
            values->m_reflectance_multiplier = 1.0f;
            values->m_roughness = max(p->roughness, 0.0f);
        }
    };

    struct PhongClosure
    {
        struct Params
        {
            OSL::Vec3   N;
            float       exponent;
        };

        static const char* name()
        {
            return "phong";
        }

        static ClosureID id()
        {
            return PhongID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FLOAT_PARAM(Params, exponent),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            AshikhminBRDFInputValues* values =
                composite_closure.add_closure<AshikhminBRDFInputValues>(
                    AshikhminShirleyID,
                    shading_basis,
                    weight,
                    p->N,
                    arena);

            values->m_rd = Color3f(1.0f);
            values->m_rd_multiplier = 1.0f;
            values->m_rg = Color3f(1.0f);
            values->m_rg_multiplier = 1.0f;
            values->m_nu = max(p->exponent, 0.01f);
            values->m_nv = max(p->exponent, 0.01f);
            values->m_fr_multiplier = 1.0f;
        }
    };

    struct ReflectionClosure
    {
        struct Params
        {
            OSL::Vec3       N;
            float           ior;
        };

        static const char* name()
        {
            return "reflection";
        }

        static ClosureID id()
        {
            return ReflectionID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            GlossyBRDFInputValues* values =
                composite_closure.add_closure<GlossyBRDFInputValues>(
                    GlossyBeckmannID,
                    shading_basis,
                    weight,
                    p->N,
                    arena);

            values->m_reflectance.set(1.0f);
            values->m_reflectance_multiplier = 1.0f;
            values->m_roughness = 0.0f;
            values->m_anisotropy = 0.0f;
            values->m_ior = max(p->ior, 0.001f);
        }
    };

    struct SheenClosure
    {
        struct Params
        {
            OSL::Vec3 N;
        };

        static const char* name()
        {
            return "as_sheen";
        }

        static ClosureID id()
        {
            return SheenID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            SheenBRDFInputValues* values =
                composite_closure.add_closure<SheenBRDFInputValues>(
                    id(),
                    shading_basis,
                    weight,
                    p->N,
                    arena);

            values->m_reflectance.set(1.0f);
            values->m_reflectance_multiplier = 1.0f;
        }
    };

    struct SubsurfaceClosure
    {
        struct Params
        {
            OSL::ustring    profile;
            OSL::Vec3       N;
            OSL::Color3     reflectance;
            OSL::Color3     mean_free_path;
            float           ior;
            float           fresnel_weight;
        };

        static const char* name()
        {
            return "as_subsurface";
        }

        static ClosureID id()
        {
            return SubsurfaceID;
        }

        static void prepare_closure(
            OSL::RendererServices*      render_services,
            int                         id,
            void*                       data)
        {
            // Initialize keyword parameter defaults.
            Params* params = new (data) Params();
            params->fresnel_weight = 1.0f;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_STRING_PARAM(Params, profile),
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_COLOR_PARAM(Params, reflectance),
                CLOSURE_COLOR_PARAM(Params, mean_free_path),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_FLOAT_KEYPARAM(Params, fresnel_weight, "fresnel_weight"),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, &prepare_closure, 0);
        }

        static void convert_closure(
            CompositeSubsurfaceClosure& composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            if (p->profile == g_normalized_diffusion_profile_str)
            {
#ifdef APPLESEED_WITH_NORMALIZED_DIFFUSION_BSSRDF
                NormalizedDiffusionBSSRDFInputValues* values =
                    composite_closure.add_closure<NormalizedDiffusionBSSRDFInputValues>(
                        SubsurfaceNormalizedDiffusionID,
                        shading_basis,
                        weight,
                        p->N,
                        arena);

                copy_parameters(p, values);
#else
                throw ExceptionOSLRuntimeError("unknown subsurface profile: normalized_diffusion");
#endif
            }
            else if (p->profile == g_gaussian_profile_str)
            {
                GaussianBSSRDFInputValues* values =
                    composite_closure.add_closure<GaussianBSSRDFInputValues>(
                        SubsurfaceGaussianID,
                        shading_basis,
                        weight,
                        p->N,
                        arena);

                copy_parameters(p, values);
            }
            else
            {
                DipoleBSSRDFInputValues* values;

                if (p->profile == g_better_dipole_profile_str)
                {
                    values =
                        composite_closure.add_closure<DipoleBSSRDFInputValues>(
                            SubsurfaceBetterDipoleID,
                            shading_basis,
                            weight,
                            p->N,
                            arena);
                }
                else if (p->profile == g_standard_dipole_profile_str)
                {
                    values =
                        composite_closure.add_closure<DipoleBSSRDFInputValues>(
                            SubsurfaceStandardDipoleID,
                            shading_basis,
                            weight,
                            p->N,
                            arena);
                }
                else if (p->profile == g_directional_dipole_profile_str)
                {
                    values =
                        composite_closure.add_closure<DipoleBSSRDFInputValues>(
                            SubsurfaceDirectionalDipoleID,
                            shading_basis,
                            weight,
                            p->N,
                            arena);
                }
                else
                {
                    string msg = "unknown subsurface profile: ";
                    msg += p->profile.c_str();
                    throw ExceptionOSLRuntimeError(msg.c_str());
                }

                copy_parameters(p, values);
            }
        }

        template <typename InputValues>
        static void copy_parameters(
            const Params*               p,
            InputValues*                values)
        {
            values->m_weight = 1.0f;
            values->m_reflectance = Color3f(p->reflectance);
            values->m_reflectance_multiplier = 1.0f;
            values->m_mfp = Color3f(p->mean_free_path);
            values->m_mfp_multiplier = 1.0f;
            values->m_ior = p->ior;
            values->m_fresnel_weight = saturate(p->fresnel_weight);
        }
    };

    struct TranslucentClosure
    {
        struct Params
        {
            OSL::Vec3 N;
        };

        static const char* name()
        {
            return "translucent";
        }

        static ClosureID id()
        {
            return TranslucentID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            DiffuseBTDFInputValues* values =
                composite_closure.add_closure<DiffuseBTDFInputValues>(
                    id(),
                    shading_basis,
                    weight,
                    p->N,
                    arena);

            values->m_transmittance.set(1.0f);
            values->m_transmittance_multiplier = 1.0f;
        }
    };

    struct TransparentClosure
    {
        struct Params
        {
        };

        static const char* name()
        {
            return "transparent";
        }

        static ClosureID id()
        {
            return TransparentID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);
        }
    };

    //
    // Layered closures.
    //

    struct LayeredClosureBaseParams
    {
        void* substrate;
    };

    struct AlSurfaceLayerClosure
    {
        typedef AlSurfaceLayerBRDFInputValues InputValues;

        struct Params
          : public LayeredClosureBaseParams
        {
            int             distribution;
            OSL::Vec3       N;
            OSL::Vec3       T;
            OSL::Color3     reflectance;
            float           roughness;
            float           anisotropy;
            int             fresnel_mode;
            float           ior;
            OSL::Color3     normal_reflectance;
            OSL::Color3     edge_tint;
        };

        static const char* name()
        {
            return "as_alsurface_layer";
        }

        static ClosureID id()
        {
            return AlSurfaceLayerID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_CLOSURE_PARAM(Params, substrate),
                CLOSURE_INT_PARAM(Params, distribution),
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_VECTOR_PARAM(Params, T),
                CLOSURE_COLOR_PARAM(Params, reflectance),
                CLOSURE_FLOAT_PARAM(Params, roughness),
                CLOSURE_FLOAT_PARAM(Params, anisotropy),
                CLOSURE_INT_PARAM(Params, fresnel_mode),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_COLOR_PARAM(Params, normal_reflectance),
                CLOSURE_COLOR_PARAM(Params, edge_tint),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            InputValues* values =
                composite_closure.add_closure<InputValues>(
                    AlSurfaceLayerID,
                    shading_basis,
                    weight,
                    p->N,
                    p->T,
                    arena);

            values->m_substrate = p->substrate;
            values->m_substrate_closure_data = 0;
            values->m_osl_bsdf = 0;

            values->m_distribution = p->distribution;
            values->m_reflectance = p->reflectance;
            values->m_roughness = p->roughness;
            values->m_anisotropy = saturate(p->anisotropy);

            values->m_fresnel_mode = p->fresnel_mode;
            values->m_ior = p->ior;
            values->m_normal_reflectance = p->normal_reflectance;
            values->m_edge_tint = p->edge_tint;
        }
    };

    const OSL::ClosureColor* get_nested_closure_color(
        const size_t    closure_id,
        const void*     params)
    {
        assert(closure_id >= FirstLayeredClosure);

        const LayeredClosureBaseParams* p =
            static_cast<const LayeredClosureBaseParams*>(params);

        return static_cast<const OSL::ClosureColor*>(p->substrate);
    }
}


//
// CompositeClosure class implementation.
//

CompositeClosure::CompositeClosure()
  : m_closure_count(0)
{
}

void CompositeClosure::compute_cdf()
{
    const size_t closure_count = get_closure_count();

    if (closure_count == 1)
    {
        m_pdf_weights[0] = 1.0f;
        m_cdf[0] = 1.0f;
    }
    else if (closure_count > 1)
    {
        float total_weight = 0.0f;
        for (size_t i = 0; i < closure_count; ++i)
        {
            total_weight += m_pdf_weights[i];
            m_cdf[i] = total_weight;
        }

        const float rcp_total_weight = 1.0f / total_weight;

        for (size_t i = 0; i < closure_count; ++i)
            m_pdf_weights[i] *= rcp_total_weight;

        for (size_t i = 0; i < closure_count - 1; ++i)
            m_cdf[i] *= rcp_total_weight;

        m_cdf[closure_count - 1] = 1.0f;
    }
}

size_t CompositeClosure::choose_closure(SamplingContext& sampling_context) const
{
    assert(get_closure_count() > 0);

    if (get_closure_count() == 1)
        return 0;

    sampling_context.split_in_place(1, 1);
    const float s = sampling_context.next2<float>();
    return choose_closure(s);
}

size_t CompositeClosure::choose_closure(const float w) const
{
    return sample_cdf_linear_search(m_cdf, w);
}

void CompositeClosure::compute_closure_shading_basis(
    const Vector3f& normal,
    const Basis3f&  original_shading_basis)
{
    const float normal_square_norm = square_norm(normal);
    if APPLESEED_LIKELY(normal_square_norm != 0.0f)
    {
        const float rcp_normal_norm = 1.0f / sqrt(normal_square_norm);
        m_bases[m_closure_count] =
            Basis3f(
                normal * rcp_normal_norm,
                original_shading_basis.get_tangent_u());
    }
    else
    {
        // Fallback to the original shading basis if the normal is zero.
        m_bases[m_closure_count] = original_shading_basis;
    }
}

void CompositeClosure::compute_closure_shading_basis(
    const Vector3f& normal,
    const Vector3f& tangent,
    const Basis3f&  original_shading_basis)
{
    const float tangent_square_norm = square_norm(tangent);
    if APPLESEED_LIKELY(tangent_square_norm != 0.0f)
    {
        const float normal_square_norm = square_norm(normal);
        if APPLESEED_LIKELY(normal_square_norm != 0.0f)
        {
            const float rcp_normal_norm = 1.0f / sqrt(normal_square_norm);
            const float rcp_tangent_norm = 1.0f / sqrt(tangent_square_norm);
            m_bases[m_closure_count] =
                Basis3f(
                    normal * rcp_normal_norm,
                    tangent * rcp_tangent_norm);
        }
        else
        {
            // Fallback to the original shading basis if the normal is zero.
            m_bases[m_closure_count] = original_shading_basis;
        }
    }
    else
    {
        // If the tangent is zero, ignore it.
        // This can happen when using the isotropic microfacet closure overloads, for example.
        compute_closure_shading_basis(normal, original_shading_basis);
    }
}

template <typename InputValues>
InputValues* CompositeClosure::add_closure(
    const ClosureID             closure_type,
    const Basis3f&              original_shading_basis,
    const Color3f&              weight,
    const Vector3f&             normal,
    Arena&                      arena)
{
    return do_add_closure<InputValues>(
        closure_type,
        original_shading_basis,
        weight,
        normal,
        false,
        Vector3f(0.0f),
        arena);
}

template <typename InputValues>
InputValues* CompositeClosure::add_closure(
    const ClosureID             closure_type,
    const Basis3f&              original_shading_basis,
    const Color3f&              weight,
    const Vector3f&             normal,
    const Vector3f&             tangent,
    Arena&                      arena)
{
    return do_add_closure<InputValues>(
        closure_type,
        original_shading_basis,
        weight,
        normal,
        true,
        tangent,
        arena);
}

template <typename InputValues>
InputValues* CompositeClosure::do_add_closure(
    const ClosureID             closure_type,
    const Basis3f&              original_shading_basis,
    const Color3f&              weight,
    const Vector3f&             normal,
    const bool                  has_tangent,
    const Vector3f&             tangent,
    Arena&                      arena)
{
    // Make sure we have enough space.
    if APPLESEED_UNLIKELY(get_closure_count() >= MaxClosureEntries)
    {
        throw ExceptionOSLRuntimeError(
            "maximum number of closures in osl shader group exceeded");
    }

    // We use the luminance of the weight as the BSDF weight.
    const float w = luminance(weight);
    assert(w > 0.0f);

    m_pdf_weights[m_closure_count] = w;
    m_weights[m_closure_count] = weight;

    if (!has_tangent)
        compute_closure_shading_basis(normal, original_shading_basis);
    else compute_closure_shading_basis(normal, tangent, original_shading_basis);

    m_closure_types[m_closure_count] = closure_type;

    InputValues* values = arena.allocate<InputValues>();
    m_input_values[m_closure_count] = values;

    ++m_closure_count;

    return values;
}


//
// CompositeSurfaceClosure class implementation.
//

CompositeSurfaceClosure::CompositeSurfaceClosure(
    const Basis3f&              original_shading_basis,
    const OSL::ClosureColor*    ci,
    Arena&                      arena)
  : m_ior_count(0)
{
    process_closure_tree(ci, original_shading_basis, Color3f(1.0f), arena);
    compute_cdf();

    if (m_ior_count == 0)
    {
        m_ior_count = 1;
        m_iors[0] = 1.0f;
        return;
    }

    // Build the IOR CDF in place if needed.
    if (m_ior_count > 1)
    {
        float total_weight = m_ior_cdf[0];
        for (size_t i = 1; i < m_ior_count; ++i)
        {
            total_weight += m_ior_cdf[i];
            m_ior_cdf[i] = total_weight;
        }

        const float rcp_total_weight = 1.0f / total_weight;

        for (size_t i = 0; i < m_ior_count - 1; ++i)
            m_ior_cdf[i] *= rcp_total_weight;

        m_ior_cdf[m_ior_count - 1] = 1.0f;
    }
}

void CompositeSurfaceClosure::add_ior(
    const foundation::Color3f&  weight,
    const float                 ior)
{
    // We use the luminance of the weight as the IOR weight.
    const float w = luminance(weight);
    assert(w > 0.0f);

    m_iors[m_ior_count] = ior;
    m_ior_cdf[m_ior_count] = w;
    ++m_ior_count;
}

float CompositeSurfaceClosure::choose_ior(const float w) const
{
    assert(m_ior_count > 0);

    if APPLESEED_LIKELY(m_ior_count == 1)
        return m_iors[0];

    const size_t index = sample_cdf_linear_search(m_ior_cdf, w);
    return m_iors[index];
}

void CompositeSurfaceClosure::process_closure_tree(
    const OSL::ClosureColor*    closure,
    const Basis3f&              original_shading_basis,
    const Color3f&              weight,
    Arena&                      arena)
{
    if (closure == 0)
        return;

    switch (closure->id)
    {
      case OSL::ClosureColor::MUL:
        {
            const OSL::ClosureMul* c = reinterpret_cast<const OSL::ClosureMul*>(closure);
            const Color3f w = weight * Color3f(c->weight);
            process_closure_tree(c->closure, original_shading_basis, w, arena);
        }
        break;

      case OSL::ClosureColor::ADD:
        {
            const OSL::ClosureAdd* c = reinterpret_cast<const OSL::ClosureAdd*>(closure);
            process_closure_tree(c->closureA, original_shading_basis, weight, arena);
            process_closure_tree(c->closureB, original_shading_basis, weight, arena);
        }
        break;

      default:
        {
            const OSL::ClosureComponent* c = reinterpret_cast<const OSL::ClosureComponent*>(closure);
            const Color3f w = weight * Color3f(c->w);

            if (luminance(w) > 0.0f)
                g_closure_convert_funs[c->id](*this, original_shading_basis, c->data(), w, arena);
        }
        break;
    }
}


//
// CompositeSubsurfaceClosure class implementation.
//

CompositeSubsurfaceClosure::CompositeSubsurfaceClosure(
    const Basis3f&              original_shading_basis,
    const OSL::ClosureColor*    ci,
    Arena&                      arena)
{
    process_closure_tree(ci, original_shading_basis, Color3f(1.0f), arena);
    compute_cdf();
}

void CompositeSubsurfaceClosure::process_closure_tree(
    const OSL::ClosureColor*    closure,
    const Basis3f&              original_shading_basis,
    const foundation::Color3f&  weight,
    Arena&                      arena)
{
    if (closure == 0)
        return;

    switch (closure->id)
    {
      case OSL::ClosureColor::MUL:
        {
            const OSL::ClosureMul* c = reinterpret_cast<const OSL::ClosureMul*>(closure);
            process_closure_tree(c->closure, original_shading_basis, weight * Color3f(c->weight), arena);
        }
        break;

      case OSL::ClosureColor::ADD:
        {
            const OSL::ClosureAdd* c = reinterpret_cast<const OSL::ClosureAdd*>(closure);
            process_closure_tree(c->closureA, original_shading_basis, weight, arena);
            process_closure_tree(c->closureB, original_shading_basis, weight, arena);
        }
        break;

      default:
        {
            const OSL::ClosureComponent* c = reinterpret_cast<const OSL::ClosureComponent*>(closure);

            if (c->id == SubsurfaceID)
            {
                const Color3f w = weight * Color3f(c->w);
                if (luminance(w) > 0.0f)
                {
                    SubsurfaceClosure::convert_closure(
                        *this,
                        original_shading_basis,
                        c->data(),
                        w,
                        arena);
                }
            }
            else if (c->id >= FirstLayeredClosure)
            {
                // For now, we just recurse.
                const OSL::ClosureColor* nested = get_nested_closure_color(c->id, c->data());
                process_closure_tree(nested, original_shading_basis, weight * Color3f(c->w), arena);
            }
        }
        break;
    }
}


//
// CompositeEmissionClosure class implementation.
//

CompositeEmissionClosure::CompositeEmissionClosure(
    const OSL::ClosureColor*    ci,
    Arena&                      arena)
{
    process_closure_tree(ci, Color3f(1.0f), arena);
    compute_cdf();
}

template <typename InputValues>
InputValues* CompositeEmissionClosure::add_closure(
    const ClosureID             closure_type,
    const Color3f&              weight,
    const float                 max_weight_component,
    Arena&                      arena)
{
    // Make sure we have enough space.
    if APPLESEED_UNLIKELY(get_closure_count() >= MaxClosureEntries)
    {
        throw ExceptionOSLRuntimeError(
            "maximum number of closures in osl shader group exceeded");
    }

    m_closure_types[m_closure_count] = closure_type;
    m_weights[m_closure_count] = weight;
    m_pdf_weights[m_closure_count] = max_weight_component;

    InputValues* values = arena.allocate<InputValues>();
    m_input_values[m_closure_count] = values;

    ++m_closure_count;

    return values;
}

void CompositeEmissionClosure::process_closure_tree(
    const OSL::ClosureColor*    closure,
    const Color3f&              weight,
    Arena&                      arena)
{
    if (closure == 0)
        return;

    switch (closure->id)
    {
      case OSL::ClosureColor::MUL:
        {
            const OSL::ClosureMul* c = reinterpret_cast<const OSL::ClosureMul*>(closure);
            process_closure_tree(c->closure, weight * Color3f(c->weight), arena);
        }
        break;

      case OSL::ClosureColor::ADD:
        {
            const OSL::ClosureAdd* c = reinterpret_cast<const OSL::ClosureAdd*>(closure);
            process_closure_tree(c->closureA, weight, arena);
            process_closure_tree(c->closureB, weight, arena);
        }
        break;

      default:
        {
            const OSL::ClosureComponent* c = reinterpret_cast<const OSL::ClosureComponent*>(closure);

            const Color3f w = weight * Color3f(c->w);
            const float max_weight_component = max_value(w);

            if (max_weight_component > 0.0f)
            {
                if (c->id == EmissionID)
                {
                    EmissionClosure::convert_closure(
                        *this,
                        c->data(),
                        w,
                        max_weight_component,
                        arena);
                }
                else if (c->id >= FirstLayeredClosure)
                {
                    // For now, we just recurse.
                    const OSL::ClosureColor* nested = get_nested_closure_color(c->id, c->data());
                    process_closure_tree(nested, w, arena);
                }
            }
        }
        break;
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
            switch (closure->id)
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

              default:
                {
                    const OSL::ClosureComponent* c = reinterpret_cast<const OSL::ClosureComponent*>(closure);

                    if (c->id == closure_id)
                        return Color3f(c->w);
                    else if (c->id >= FirstLayeredClosure)
                    {
                        // Recurse inside the layered closure.
                        const OSL::ClosureColor* nested = get_nested_closure_color(c->id, c->data());
                        return Color3f(c->w) * do_process_closure_id_tree(nested, closure_id);
                    }
                    else return Color3f(0.0f);
                }
                break;
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

void inject_layered_closure_values(
    const size_t    closure_id,
    const BSDF*     osl_bsdf,
    void*           data)
{
    assert(closure_id >= FirstLayeredClosure);

    switch (closure_id)
    {
      case AlSurfaceLayerID:
      {
          AlSurfaceLayerBRDFInputValues* values =
              static_cast<AlSurfaceLayerBRDFInputValues*>(data);
          values->m_osl_bsdf = osl_bsdf;
      }
      break;

      assert_otherwise;
    }
}

namespace
{
    template <typename ClosureType>
    void register_closure(OSL::ShadingSystem& shading_system)
    {
        ClosureType::register_closure(shading_system);
        RENDERER_LOG_DEBUG("registered osl closure %s.", ClosureType::name());
    }
}

void register_closures(OSL::ShadingSystem& shading_system)
{
    for (size_t i = 0; i < NumClosuresIDs; ++i)
        g_closure_convert_funs[i] = &convert_closure_nop;

    register_closure<AlSurfaceLayerClosure>(shading_system);
    register_closure<AshikhminShirleyClosure>(shading_system);
    register_closure<BackgroundClosure>(shading_system);
    register_closure<BlinnClosure>(shading_system);
    register_closure<DebugClosure>(shading_system);
    register_closure<DiffuseClosure>(shading_system);
    register_closure<DisneyClosure>(shading_system);
    register_closure<EmissionClosure>(shading_system);
    register_closure<GlassClosure>(shading_system);
    register_closure<GlossyClosure>(shading_system);
    register_closure<HoldoutClosure>(shading_system);
    register_closure<MetalClosure>(shading_system);
    register_closure<OrenNayarClosure>(shading_system);
    register_closure<PhongClosure>(shading_system);
    register_closure<ReflectionClosure>(shading_system);
    register_closure<SheenClosure>(shading_system);
    register_closure<SubsurfaceClosure>(shading_system);
    register_closure<TranslucentClosure>(shading_system);
    register_closure<TransparentClosure>(shading_system);
}

}   // namespace renderer
