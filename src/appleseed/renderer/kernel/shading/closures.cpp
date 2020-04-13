
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/kernel/npr/nprclosures.h"
#include "renderer/kernel/shading/oslshadingsystem.h"
#include "renderer/modeling/bsdf/ashikhminbrdf.h"
#include "renderer/modeling/bsdf/blinnbrdf.h"
#include "renderer/modeling/bsdf/diffusebtdf.h"
#include "renderer/modeling/bsdf/disneybrdf.h"
#include "renderer/modeling/bsdf/glassbsdf.h"
#include "renderer/modeling/bsdf/glossybrdf.h"
#include "renderer/modeling/bsdf/glossylayerbsdf.h"
#include "renderer/modeling/bsdf/hairbsdf.h"
#include "renderer/modeling/bsdf/metalbrdf.h"
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/modeling/bsdf/orennayarbrdf.h"
#include "renderer/modeling/bsdf/plasticbrdf.h"
#include "renderer/modeling/bsdf/sheenbrdf.h"
#include "renderer/modeling/bssrdf/dipolebssrdf.h"
#include "renderer/modeling/bssrdf/directionaldipolebssrdf.h"
#include "renderer/modeling/bssrdf/gaussianbssrdf.h"
#include "renderer/modeling/bssrdf/normalizeddiffusionbssrdf.h"
#include "renderer/modeling/bssrdf/randomwalkbssrdf.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/edf/diffuseedf.h"

// appleseed.foundation headers.
#include "foundation/math/cdf.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"
#include "foundation/memory/arena.h"
#include "foundation/memory/memory.h"
#include "foundation/utility/otherwise.h"

// OSL headers.
#include "foundation/platform/_beginoslheaders.h"
#include "OSL/genclosure.h"
#include "OSL/oslclosure.h"
#include "OSL/oslversion.h"
#include "foundation/platform/_endoslheaders.h"

// Standard headers.
#include <algorithm>
#include <cmath>

using namespace foundation;
using namespace renderer;

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
    const OIIO::ustring g_gtr1_str("gtr1");
    const OIIO::ustring g_std_str("std");

    const OIIO::ustring g_standard_dipole_profile_str("standard_dipole");
    const OIIO::ustring g_better_dipole_profile_str("better_dipole");
    const OIIO::ustring g_directional_dipole_profile_str("directional_dipole");
    const OIIO::ustring g_normalized_diffusion_profile_str("normalized_diffusion");
    const OIIO::ustring g_gaussian_profile_str("gaussian");
    const OIIO::ustring g_randomwalk_profile_str("randomwalk");

    //
    // Closure functions.
    //

    typedef void (*convert_closure_fun)(
        CompositeClosure&           composite_closure,
        const Basis3f&              shading_basis,
        const void*                 osl_params,
        const Color3f&              weight,
        Arena&                      arena);

    convert_closure_fun g_closure_convert_funs[NumClosuresIDs];

    typedef int (*closure_get_modes)();

    closure_get_modes g_closure_get_modes_funs[NumClosuresIDs];

    int closure_no_modes()
    {
        return 0;
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

        static int modes()
        {
            return ScatteringMode::Diffuse | ScatteringMode::Glossy;
        }

        static void register_closure(OSLShadingSystem& shading_system)
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

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);

            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
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

            values->m_rd.set(Color3f(p->diffuse_reflectance), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_rd_multiplier = 1.0f;
            values->m_rg.set(Color3f(p->glossy_reflectance), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_rg_multiplier = 1.0f;
            values->m_nu = std::max(p->exponent_u, 0.01f);
            values->m_nv = std::max(p->exponent_v, 0.01f);
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

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);
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

        static int modes()
        {
            return ScatteringMode::Glossy;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FLOAT_PARAM(Params, exponent),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);

            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
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

            values->m_exponent = std::max(p->exponent, 0.001f);
            values->m_ior = std::max(p->ior, 0.001f);
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

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_STRING_PARAM(Params, tag),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);
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

        static int modes()
        {
            return ScatteringMode::Diffuse;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);

            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
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

        static int modes()
        {
            return ScatteringMode::Diffuse | ScatteringMode::Glossy;
        }

        static void register_closure(OSLShadingSystem& shading_system)
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

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);

            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
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

            values->m_base_color.set(Color3f(p->base_color), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_subsurface = saturate(p->subsurface);
            values->m_metallic = saturate(p->metallic);
            values->m_specular = std::max(p->specular, 0.0f);
            values->m_specular_tint = saturate(p->specular_tint);
            values->m_anisotropic = clamp(p->anisotropic, -1.0f, 1.0f);
            values->m_roughness = clamp(p->roughness, 0.0001f, 1.0f);
            values->m_sheen = std::max(p->sheen, 0.0f);
            values->m_sheen_tint = saturate(p->sheen_tint);
            values->m_clearcoat = std::max(p->clearcoat, 0.0f);
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

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);
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

            values->m_radiance.set(Color3f(weight / max_weight_component), g_std_lighting_conditions, Spectrum::Illuminance);
            values->m_radiance_multiplier = max_weight_component;
            values->m_exposure = 0.0f;
        }
    };

    struct GlassClosure
    {
        struct Params
        {
            OSL::Vec3       N;
            OSL::Vec3       T;
            OSL::Color3     surface_transmittance;
            OSL::Color3     reflection_tint;
            OSL::Color3     refraction_tint;
            float           roughness;
            float           anisotropy;
            float           ior;
            OSL::Color3     volume_transmittance;
            float           volume_transmittance_distance;
            float           energy_compensation;
        };

        static const char* name()
        {
            return "as_glass";
        }

        static ClosureID id()
        {
            return GlassID;
        }

        static int modes()
        {
            return ScatteringMode::Glossy | ScatteringMode::Specular;
        }

        static void prepare_closure(
            OSL::RendererServices*      render_services,
            int                         id,
            void*                       data)
        {
            // Initialize keyword parameter defaults.
            Params* params = new (data) Params();
            params->energy_compensation = 0.0f;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_VECTOR_PARAM(Params, T),
                CLOSURE_COLOR_PARAM(Params, surface_transmittance),
                CLOSURE_COLOR_PARAM(Params, reflection_tint),
                CLOSURE_COLOR_PARAM(Params, refraction_tint),
                CLOSURE_FLOAT_PARAM(Params, roughness),
                CLOSURE_FLOAT_PARAM(Params, anisotropy),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_COLOR_PARAM(Params, volume_transmittance),
                CLOSURE_FLOAT_PARAM(Params, volume_transmittance_distance),
                CLOSURE_FLOAT_KEYPARAM(Params, energy_compensation, "energy_compensation"),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, &prepare_closure, nullptr);
            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            GlassBSDFInputValues* values =
                composite_closure.add_closure<GlassBSDFInputValues>(
                    id(),
                    shading_basis,
                    weight,
                    p->N,
                    p->T,
                    arena);

            values->m_surface_transmittance.set(Color3f(p->surface_transmittance), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_surface_transmittance_multiplier = 1.0f;
            values->m_reflection_tint.set(Color3f(p->reflection_tint), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_refraction_tint.set(Color3f(p->refraction_tint), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_roughness = std::max(p->roughness, 0.0001f);
            values->m_anisotropy = clamp(p->anisotropy, -1.0f, 1.0f);
            values->m_ior = std::max(p->ior, 0.001f);
            values->m_volume_transmittance.set(Color3f(p->volume_transmittance), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_volume_transmittance_distance = p->volume_transmittance_distance;
            values->m_energy_compensation = saturate(p->energy_compensation);
            composite_closure.add_ior(weight, values->m_ior);
        }
    };

    struct GlossyClosure
    {
        struct Params
        {
            OSL::Vec3       N;
            OSL::Vec3       T;
            float           roughness;
            float           anisotropy;
            float           ior;
            float           energy_compensation;
            float           fresnel_weight;
        };

        static const char* name()
        {
            return "as_glossy";
        }

        static ClosureID id()
        {
            return GlossyID;
        }

        static int modes()
        {
            return ScatteringMode::Glossy | ScatteringMode::Specular;
        }

        static void prepare_closure(
            OSL::RendererServices*      render_services,
            int                         id,
            void*                       data)
        {
            // Initialize keyword parameter defaults.
            Params* params = new (data) Params();
            params->energy_compensation = 0.0f;
            params->fresnel_weight = 1.0f;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_VECTOR_PARAM(Params, T),
                CLOSURE_FLOAT_PARAM(Params, roughness),
                CLOSURE_FLOAT_PARAM(Params, anisotropy),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_FLOAT_KEYPARAM(Params, energy_compensation, "energy_compensation"),
                CLOSURE_FLOAT_KEYPARAM(Params, fresnel_weight, "fresnel_weight"),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, &prepare_closure, nullptr);
            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            const float roughness = saturate(p->roughness);
            const float fresnel_weight = saturate(p->fresnel_weight);

            const float ior = std::max(p->ior, 0.001f);

            GlossyBRDFInputValues* values =
                composite_closure.add_closure<GlossyBRDFInputValues>(
                    id(),
                    shading_basis,
                    weight,
                    p->N,
                    p->T,
                    arena);

            composite_closure.override_closure_scalar_weight(
                luminance(weight) * sample_weight(roughness, ior, fresnel_weight));

            values->m_reflectance.set(1.0f);
            values->m_reflectance_multiplier = 1.0f;
            values->m_roughness = roughness;
            values->m_anisotropy = clamp(p->anisotropy, -1.0f, 1.0f);
            values->m_ior = ior;
            values->m_fresnel_weight = fresnel_weight;
            values->m_energy_compensation = saturate(p->energy_compensation);
        }

        static float sample_weight(
            const float                 roughness,
            const float                 ior,
            const float                 fresnel_weight)
        {
            const float eavg = get_average_albedo(roughness);
            const float favg = lerp(
                1.0f,
                average_fresnel_reflectance_dielectric(ior),
                fresnel_weight);
            return eavg * favg;
        }
    };

    struct HairClosure
    {
        struct Params
        {
            OSL::Color3     reflectance;
            float           melanin;
            float           melanin_redness;
            float           eta;
            float           beta_M;
            float           beta_N;
            float           alpha;
        };

        static const char* name()
        {
            return "as_hair";
        }

        static ClosureID id()
        {
            return HairID;
        }

        static int modes()
        {
            return ScatteringMode::Glossy;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_COLOR_PARAM(Params, reflectance),
                CLOSURE_FLOAT_PARAM(Params, melanin),
                CLOSURE_FLOAT_PARAM(Params, melanin_redness),
                CLOSURE_FLOAT_PARAM(Params, eta),
                CLOSURE_FLOAT_PARAM(Params, beta_M),
                CLOSURE_FLOAT_PARAM(Params, beta_N),
                CLOSURE_FLOAT_PARAM(Params, alpha),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);
            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            HairBSDFInputValues* values =
                composite_closure.add_closure<HairBSDFInputValues>(
                    id(),
                    shading_basis,
                    weight,
                    Vector3f(0.0f),
                    arena);

            values->m_reflectance.set(Color3f(p->reflectance), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_melanin = saturate(p->melanin);
            values->m_melanin_redness = saturate(p->melanin_redness);
            values->m_eta = std::max(p->eta, 0.001f);
            values->m_beta_M = p->beta_M;
            values->m_beta_N = p->beta_N;
            values->m_alpha = p->alpha;
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

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);
        }
    };

    struct MatteClosure
    {
        struct Params
        {
            OSL::Color3 matte_color;
            float       matte_alpha;
        };

        static const char* name()
        {
            return "as_matte";
        }

        static ClosureID id()
        {
            return MatteID;
        }

        static void prepare_closure(
            OSL::RendererServices*      render_services,
            int                         id,
            void*                       data)
        {
            // Initialize keyword parameter defaults.
            Params* params = new (data) Params();
            params->matte_color = OSL::Color3(0.0f);
            params->matte_alpha = 0.0f;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_COLOR_PARAM(Params, matte_color),
                CLOSURE_FLOAT_PARAM(Params, matte_alpha),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);
        }
    };

    struct MetalClosure
    {
        struct Params
        {
            OSL::Vec3       N;
            OSL::Vec3       T;
            OSL::Color3     normal_reflectance;
            OSL::Color3     edge_tint;
            float           edge_tint_weight;
            float           roughness;
            float           anisotropy;
            float           energy_compensation;
        };

        static const char* name()
        {
            return "as_metal";
        }

        static ClosureID id()
        {
            return MetalID;
        }

        static int modes()
        {
            return ScatteringMode::Glossy | ScatteringMode::Specular;
        }

        static void prepare_closure(
            OSL::RendererServices*      render_services,
            int                         id,
            void*                       data)
        {
            // Initialize keyword parameter defaults.
            Params* params = new (data) Params();
            params->energy_compensation = 0.0f;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_VECTOR_PARAM(Params, T),
                CLOSURE_COLOR_PARAM(Params, normal_reflectance),
                CLOSURE_COLOR_PARAM(Params, edge_tint),
                CLOSURE_FLOAT_PARAM(Params, edge_tint_weight),
                CLOSURE_FLOAT_PARAM(Params, roughness),
                CLOSURE_FLOAT_PARAM(Params, anisotropy),
                CLOSURE_FLOAT_KEYPARAM(Params, energy_compensation, "energy_compensation"),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, &prepare_closure, nullptr);
            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            MetalBRDFInputValues* values =
                composite_closure.add_closure<MetalBRDFInputValues>(
                    id(),
                    shading_basis,
                    weight,
                    p->N,
                    p->T,
                    arena);

            values->m_normal_reflectance.set(Color3f(p->normal_reflectance), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_edge_tint.set(Color3f(p->edge_tint), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_edge_tint_weight = saturate(p->edge_tint_weight);
            values->m_reflectance_multiplier = 1.0f;
            values->m_roughness = std::max(p->roughness, 0.0f);
            values->m_anisotropy = clamp(p->anisotropy, -1.0f, 1.0f);
            values->m_energy_compensation = saturate(p->energy_compensation);
        }
    };

    struct OrenNayarClosure
    {
        struct Params
        {
            OSL::Color3 reflectance;
            OSL::Vec3   N;
            float       roughness;
        };

        static const char* name()
        {
            return "as_oren_nayar";
        }

        static ClosureID id()
        {
            return OrenNayarID;
        }

        static int modes()
        {
            return ScatteringMode::Diffuse;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_COLOR_PARAM(Params, reflectance),
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FLOAT_PARAM(Params, roughness),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);

            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
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

            const Color3f reflectance = Color3f(p->reflectance);
            values->m_reflectance.set(reflectance, g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_reflectance_multiplier = 1.0f;
            values->m_roughness = std::max(p->roughness, 0.0f);

            const float w = luminance(weight) * luminance(reflectance);
            composite_closure.override_closure_scalar_weight(w);
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

        static int modes()
        {
            return ScatteringMode::Glossy;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FLOAT_PARAM(Params, exponent),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);

            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
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

            values->m_rd.set(1.0f);
            values->m_rd_multiplier = 1.0f;
            values->m_rg.set(1.0f);
            values->m_rg_multiplier = 1.0f;
            values->m_nu = std::max(p->exponent, 0.01f);
            values->m_nv = std::max(p->exponent, 0.01f);
            values->m_fr_multiplier = 1.0f;
        }
    };

    struct PlasticClosure
    {
        struct Params
        {
            OSL::Vec3       N;
            OSL::Color3     specular_reflectance;
            float           specular_reflectance_multiplier;
            float           roughness;
            float           ior;
            OSL::Color3     diffuse_reflectance;
            float           diffuse_reflectance_multiplier;
            float           internal_scattering;
        };

        static const char* name()
        {
            return "as_plastic";
        }

        static ClosureID id()
        {
            return PlasticID;
        }

        static int modes()
        {
            return ScatteringMode::Diffuse | ScatteringMode::Glossy | ScatteringMode::Specular;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_COLOR_PARAM(Params, specular_reflectance),
                CLOSURE_FLOAT_PARAM(Params, specular_reflectance_multiplier),
                CLOSURE_FLOAT_PARAM(Params, roughness),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_COLOR_PARAM(Params, diffuse_reflectance),
                CLOSURE_FLOAT_PARAM(Params, diffuse_reflectance_multiplier),
                CLOSURE_FLOAT_PARAM(Params, internal_scattering),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);
            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            PlasticBRDFInputValues* values =
                composite_closure.add_closure<PlasticBRDFInputValues>(
                    id(),
                    shading_basis,
                    weight,
                    p->N,
                    arena);

            values->m_specular_reflectance.set(Color3f(p->specular_reflectance), g_std_lighting_conditions,
            Spectrum::Reflectance);
            values->m_specular_reflectance_multiplier = std::max(p->specular_reflectance_multiplier, 0.0f);
            values->m_roughness = clamp(p->roughness, 0.0001f, 1.0f);
            values->m_ior = std::max(p->ior, 0.001f);
            values->m_diffuse_reflectance.set(Color3f(p->diffuse_reflectance), g_std_lighting_conditions,
            Spectrum::Reflectance);
            values->m_diffuse_reflectance_multiplier = std::max(p->diffuse_reflectance_multiplier, 0.0f);
            values->m_internal_scattering = std::max(p->internal_scattering, 0.0f);
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

        static int modes()
        {
            return ScatteringMode::Specular;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);

            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            GlossyBRDFInputValues* values =
                composite_closure.add_closure<GlossyBRDFInputValues>(
                    GlossyID,
                    shading_basis,
                    weight,
                    p->N,
                    arena);

            values->m_reflectance.set(1.0f);
            values->m_reflectance_multiplier = 1.0f;
            values->m_roughness = 0.0f;
            values->m_anisotropy = 0.0f;
            values->m_ior = std::max(p->ior, 0.001f);
            values->m_energy_compensation = 0.0f;
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

        static int modes()
        {
            return ScatteringMode::Diffuse;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);

            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
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
            float           volume_anisotropy;
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
            params->volume_anisotropy = 0.0f;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_STRING_PARAM(Params, profile),
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_COLOR_PARAM(Params, reflectance),
                CLOSURE_COLOR_PARAM(Params, mean_free_path),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_FLOAT_KEYPARAM(Params, fresnel_weight, "fresnel_weight"),
                CLOSURE_FLOAT_KEYPARAM(Params, volume_anisotropy, "volume_anisotropy"),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, &prepare_closure, nullptr);
        }

        static void convert_closure(
            CompositeSubsurfaceClosure& composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            if (p->profile == g_standard_dipole_profile_str)
            {
                DipoleBSSRDFInputValues* values =
                    composite_closure.add_closure<DipoleBSSRDFInputValues>(
                        SubsurfaceStandardDipoleID,
                        shading_basis,
                        weight,
                        p->N,
                        arena);

                copy_parameters(p, values);
            }
            else if (p->profile == g_better_dipole_profile_str)
            {
                DipoleBSSRDFInputValues* values =
                    composite_closure.add_closure<DipoleBSSRDFInputValues>(
                        SubsurfaceBetterDipoleID,
                        shading_basis,
                        weight,
                        p->N,
                        arena);

                copy_parameters(p, values);
            }
            else if (p->profile == g_directional_dipole_profile_str)
            {
                DipoleBSSRDFInputValues* values =
                    composite_closure.add_closure<DipoleBSSRDFInputValues>(
                        SubsurfaceDirectionalDipoleID,
                        shading_basis,
                        weight,
                        p->N,
                        arena);

                copy_parameters(p, values);
            }
            else if (p->profile == g_normalized_diffusion_profile_str)
            {
                NormalizedDiffusionBSSRDFInputValues* values =
                    composite_closure.add_closure<NormalizedDiffusionBSSRDFInputValues>(
                        SubsurfaceNormalizedDiffusionID,
                        shading_basis,
                        weight,
                        p->N,
                        arena);

                copy_parameters(p, values);
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
            else if (p->profile == g_randomwalk_profile_str)
            {
                RandomwalkBSSRDFInputValues* values =
                    composite_closure.add_closure<RandomwalkBSSRDFInputValues>(
                        SubsurfaceRandomwalkID,
                        shading_basis,
                        weight,
                        p->N,
                        arena);

                copy_parameters(p, values);
                values->m_volume_anisotropy = clamp(p->volume_anisotropy, -0.999f, 0.999f);
            }
            else
            {
                std::string msg = "unknown subsurface profile: ";
                msg += p->profile.c_str();
                throw ExceptionOSLRuntimeError(msg.c_str());
            }
        }

        template <typename InputValues>
        static void copy_parameters(
            const Params*               p,
            InputValues*                values)
        {
            values->m_weight = 1.0f;
            values->m_reflectance.set(Color3f(p->reflectance), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_reflectance_multiplier = 1.0f;
            values->m_mfp.set(Color3f(p->mean_free_path), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_mfp_multiplier = 1.0f;
            values->m_ior = p->ior;
            values->m_fresnel_weight = saturate(p->fresnel_weight);
        }
    };

    struct RandomwalkGlassClosure
    {
        struct Params
        {
            OSL::Vec3       N;
            OSL::Color3     reflectance;
            OSL::Color3     mean_free_path;
            float           ior;
            float           surface_roughness;
            float           volume_anisotropy;
        };

        static const char* name()
        {
            return "as_randomwalk_glass";
        }

        static ClosureID id()
        {
            return RandomwalkGlassID;
        }

        static void prepare_closure(
            OSL::RendererServices*      render_services,
            int                         id,
            void*                       data)
        {
            // Initialize keyword parameter defaults.
            Params* params = new (data) Params();
            params->volume_anisotropy = 0.0f;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_COLOR_PARAM(Params, reflectance),
                CLOSURE_COLOR_PARAM(Params, mean_free_path),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_FLOAT_PARAM(Params, surface_roughness),
                CLOSURE_FLOAT_KEYPARAM(Params, volume_anisotropy, "volume_anisotropy"),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, &prepare_closure, nullptr);
        }

        static void convert_closure(
            CompositeSubsurfaceClosure& composite_closure,
            const Basis3f&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight,
            Arena&                      arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            RandomwalkBSSRDFInputValues* values =
                composite_closure.add_closure<RandomwalkBSSRDFInputValues>(
                    RandomwalkGlassID,
                    shading_basis,
                    weight,
                    p->N,
                    arena);

            copy_parameters(p, values);
        }

        static void copy_parameters(
            const Params*                   p,
            RandomwalkBSSRDFInputValues*    values)
        {
            values->m_weight = 1.0f;
            values->m_reflectance.set(Color3f(p->reflectance), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_reflectance_multiplier = 1.0f;
            values->m_mfp.set(Color3f(p->mean_free_path), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_mfp_multiplier = 1.0f;
            values->m_ior = p->ior;
            values->m_surface_roughness = clamp(p->surface_roughness, 0.0001f, 1.0f);
            values->m_fresnel_weight = 1.0f;
            values->m_volume_anisotropy = clamp(p->volume_anisotropy, -0.999f, 0.999f);
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

        static int modes()
        {
            return ScatteringMode::Diffuse;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);

            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&           composite_closure,
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

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);
        }
    };

    //
    // NPR closures.
    //

    struct NPRShadingClosure
    {
        struct Params
        {
        };

        static const char* name()
        {
            return "as_npr_shading";
        }

        static ClosureID id()
        {
            return NPRShadingID;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);
        }

        static void convert_closure(
            CompositeNPRClosure&    composite_closure,
            const void*             osl_params,
            const Color3f&          weight,
            Arena&                  arena)
        {
            composite_closure.add_closure<NPRShadingInputValues>(id(), weight, arena);
        }
    };

    struct NPRContourClosure
    {
        struct Params
        {
            OSL::Color3 contour_color;
            float       contour_opacity;
            float       contour_width;
            int         object_id;
            int         material_id;
            int         occlusion;
            float       occlusion_threshold;
            int         creases;
            float       creases_threshold;
            int         quality;
        };

        static const char* name()
        {
            return "as_npr_contour";
        }

        static ClosureID id()
        {
            return NPRContourID;
        }

        static void prepare_closure(
            OSL::RendererServices*      render_services,
            int                         id,
            void*                       data)
        {
            // Initialize keyword parameter defaults.
            Params* params = new (data) Params();
            params->quality = 1;
        }

        static void register_closure(OSLShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_COLOR_PARAM(Params, contour_color),
                CLOSURE_FLOAT_PARAM(Params, contour_opacity),
                CLOSURE_FLOAT_PARAM(Params, contour_width),
                CLOSURE_INT_PARAM(Params, object_id),
                CLOSURE_INT_PARAM(Params, material_id),

                CLOSURE_INT_PARAM(Params, occlusion),
                CLOSURE_FLOAT_PARAM(Params, occlusion_threshold),
                CLOSURE_INT_PARAM(Params, creases),
                CLOSURE_FLOAT_PARAM(Params, creases_threshold),

                CLOSURE_INT_KEYPARAM(Params, quality, "quality"),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, &prepare_closure, nullptr);
        }

        static void convert_closure(
            CompositeNPRClosure&    composite_closure,
            const void*             osl_params,
            const Color3f&          weight,
            Arena&                  arena)
        {
            const Params* p = static_cast<const Params*>(osl_params);

            unsigned int features = 0;
            if (p->object_id != 0) features |= static_cast<unsigned int>(NPRContourFeatures::ObjectInstanceID);
            if (p->material_id != 0) features |= static_cast<unsigned int>(NPRContourFeatures::MaterialID);
            if (p->occlusion != 0) features |= static_cast<unsigned int>(NPRContourFeatures::OcclusionEdges);
            if (p->creases != 0) features |= static_cast<unsigned int>(NPRContourFeatures::CreaseEdges);

            if (features == 0 || p->contour_opacity == 0.0f || p->contour_width == 0.0f)
                return;

            NPRContourInputValues* values =
                composite_closure.add_closure<NPRContourInputValues>(id(), weight, arena);

            values->m_color = Color3f(p->contour_color);
            values->m_opacity = saturate(p->contour_opacity);
            values->m_width = std::max(p->contour_width, 0.0f);

            values->m_occlusion_threshold = std::max(p->occlusion_threshold, 0.0f);
            values->m_cos_crease_threshold = std::cos(deg_to_rad(p->creases_threshold));

            values->m_features = features;
            values->m_quality = static_cast<std::size_t>(clamp(p->quality, 1, 4));
        }
    };


    //
    // Layered Closures.
    //

    struct LayeredClosureBaseParams
    {
        void* substrate;
    };

    const OSL::ClosureColor* get_nested_closure_color(const std::size_t closure_id, const void* params)
    {
        assert(closure_id >= FirstLayeredClosure);

        const LayeredClosureBaseParams *p =
            reinterpret_cast<const LayeredClosureBaseParams*>(params);

        return reinterpret_cast<const OSL::ClosureColor*>(p->substrate);
    }

    struct GlossyLayerClosure
    {
        typedef GlossyLayerBSDFInputValues InputValues;

        struct Params : public LayeredClosureBaseParams
        {
            OSL::Vec3   N;
            OSL::Vec3   T;
            OSL::Color3 reflectance;
            float       roughness;
            float       anisotropy;
            float       ior;
            OSL::Color3 transmittance;
            float       thickness;
        };

        static const char* name()
        {
            return "as_glossy_layer";
        }

        static ClosureID id()
        {
            return GlossyLayerID;
        }

        static int modes()
        {
            return ScatteringMode::Glossy;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_CLOSURE_PARAM(Params, substrate),
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_VECTOR_PARAM(Params, T),
                CLOSURE_COLOR_PARAM(Params, reflectance),
                CLOSURE_FLOAT_PARAM(Params, roughness),
                CLOSURE_FLOAT_PARAM(Params, anisotropy),
                CLOSURE_FLOAT_PARAM(Params, ior),
                CLOSURE_COLOR_PARAM(Params, transmittance),
                CLOSURE_FLOAT_PARAM(Params, thickness),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, nullptr, nullptr);
            g_closure_convert_funs[id()] = &convert_closure;
            g_closure_get_modes_funs[id()] = &modes;
        }

        static void convert_closure(
            CompositeClosure&   composite_closure,
            const Basis3f&      shading_basis,
            const void*         osl_params,
            const Color3f&      weight,
            Arena&              arena)
        {
            const Params* p = reinterpret_cast<const Params*>(osl_params);

            InputValues* values =
                composite_closure.add_closure<GlossyLayerBSDFInputValues>(
                    id(),
                    shading_basis,
                    weight,
                    p->N,
                    p->T,
                    arena);

            const float roughness = clamp(p->roughness, 0.01f, 1.0f);
            const float ior = max(p->ior, 0.001f);

            composite_closure.override_closure_scalar_weight(
                luminance(weight) * GlossyClosure::sample_weight(roughness, ior, 1.0f));

            values->m_substrate = p->substrate;
            values->m_reflectance.set(Color3f(p->reflectance), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_roughness = roughness;
            values->m_anisotropy = clamp(p->anisotropy, -1.0f, 1.0f);
            values->m_ior = ior;
            values->m_transmittance.set(Color3f(p->transmittance), g_std_lighting_conditions, Spectrum::Reflectance);
            values->m_transmittance = clamp_low(values->m_transmittance, 0.001f);
            values->m_thickness = std::max(p->thickness, 0.0f);
        }
    };
}


//
// ExceptionOSLRuntimeError class implementation.
//

ExceptionOSLRuntimeError::ExceptionOSLRuntimeError(const char* what)
    : foundation::Exception(what)
{
}


//
// CompositeClosure class implementation.
//

CompositeClosure::CompositeClosure()
  : m_closure_count(0)
  , m_ior_count(0)
{
    std::memset(m_layers, -1, MaxClosureEntries * MaxClosureLayers);
}

void CompositeClosure::compute_closure_shading_basis(
    const Vector3f& normal,
    const Basis3f&  original_shading_basis)
{
    const float normal_square_norm = square_norm(normal);
    if APPLESEED_LIKELY(normal_square_norm != 0.0f)
    {
        m_bases[m_closure_count] =
            Basis3f(
                normal / std::sqrt(normal_square_norm),
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
            m_bases[m_closure_count] =
                Basis3f(
                    normal / std::sqrt(normal_square_norm),
                    tangent / std::sqrt(tangent_square_norm));
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
    return
        do_add_closure<InputValues, false>(
            closure_type,
            original_shading_basis,
            weight,
            normal,
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
    return
        do_add_closure<InputValues, true>(
            closure_type,
            original_shading_basis,
            weight,
            normal,
            tangent,
            arena);
}

template <typename InputValues, bool HasTangent>
InputValues* CompositeClosure::do_add_closure(
    const ClosureID             closure_type,
    const Basis3f&              original_shading_basis,
    const Color3f&              weight,
    const Vector3f&             normal,
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

    m_weights[m_closure_count].set(weight, g_std_lighting_conditions, Spectrum::Reflectance);
    m_scalar_weights[m_closure_count] = w;

    if (HasTangent)
        compute_closure_shading_basis(normal, tangent, original_shading_basis);
    else compute_closure_shading_basis(normal, original_shading_basis);

    m_closure_types[m_closure_count] = closure_type;

    InputValues* values = arena.allocate<InputValues>();
    m_input_values[m_closure_count] = values;

    ++m_closure_count;

    return values;
}

void CompositeClosure::add_ior(
    const Color3f&              weight,
    const float                 ior)
{
    // We use the luminance of the weight as the IOR weight.
    const float w = luminance(weight);
    assert(w > 0.0f);

    m_iors[m_ior_count] = ior;
    m_ior_cdf[m_ior_count] = w;
    ++m_ior_count;
}

float CompositeClosure::choose_ior(const float w) const
{
    assert(m_ior_count > 0);

    if APPLESEED_LIKELY(m_ior_count == 1)
        return m_iors[0];

    const std::size_t index = sample_cdf_linear_search(m_ior_cdf, w);
    return m_iors[index];
}

void CompositeClosure::copy_layer_ids(const SmallClosureLayerIDStack& layer_stack)
{
    if (layer_stack.empty())
        return;

    std::int8_t* layers = m_layers + get_last_closure_index() * MaxClosureLayers;

    const int last_item = static_cast<int>(layer_stack.size()) -1;
    for (int i = last_item; i >= 0; --i)
        *layers++ = layer_stack[i];
}

void CompositeClosure::compute_pdfs(float pdfs[MaxClosureEntries])
{
    const std::size_t closure_count = get_closure_count();

    float total_weight = 0.0f;
    for (std::size_t i = 0; i < closure_count; ++i)
    {
        pdfs[i] = m_scalar_weights[i];
        total_weight += pdfs[i];
    }

    if (total_weight != 0.0f)
    {
        const float rcp_total_weight = 1.0f / total_weight;

        for (std::size_t i = 0; i < closure_count; ++i)
            pdfs[i] *= rcp_total_weight;
    }
}


//
// CompositeSurfaceClosure class implementation.
//

CompositeSurfaceClosure::CompositeSurfaceClosure(
    const Basis3f&              original_shading_basis,
    const OSL::ClosureColor*    ci,
    Arena&                      arena)
{
    SmallClosureLayerIDStack layer_stack;
    process_closure_tree(ci, original_shading_basis, Color3f(1.0f), layer_stack, arena);

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
        for (std::size_t i = 1; i < m_ior_count; ++i)
        {
            total_weight += m_ior_cdf[i];
            m_ior_cdf[i] = total_weight;
        }

        const float rcp_total_weight = 1.0f / total_weight;

        for (std::size_t i = 0; i < m_ior_count - 1; ++i)
            m_ior_cdf[i] *= rcp_total_weight;

        m_ior_cdf[m_ior_count - 1] = 1.0f;
    }
}

int CompositeSurfaceClosure::compute_pdfs(
    const int                   modes,
    float                       pdfs[MaxClosureEntries]) const
{
    memset(pdfs, 0, sizeof(float) * MaxClosureEntries);

    int num_closures = 0;
    float sum_weights = 0.0f;

    for (std::size_t i = 0, e = get_closure_count(); i < e; ++i)
    {
        const ClosureID cid = m_closure_types[i];
        const int closure_modes = g_closure_get_modes_funs[cid]();

        if (closure_modes & modes)
        {
            pdfs[i] = m_scalar_weights[i];
            sum_weights += m_scalar_weights[i];
            ++num_closures;
        }
        else
            pdfs[i] = 0.0f;
    }

    if (sum_weights != 0.0f)
    {
        const float rcp_sum_weights = 1.0f / sum_weights;
        for (std::size_t i = 0, e = get_closure_count(); i < e; ++i)
            pdfs[i] *= rcp_sum_weights;
    }

    return num_closures;
}

std::size_t CompositeSurfaceClosure::choose_closure(
    const float                 w,
    const std::size_t           num_closures,
    float                       pdfs[MaxClosureEntries]) const
{
    assert(num_closures > 0);
    assert(num_closures < MaxClosureEntries);

    return sample_pdf_linear_search(pdfs, num_closures, w);
}

void CompositeSurfaceClosure::process_closure_tree(
    const OSL::ClosureColor*    closure,
    const Basis3f&              original_shading_basis,
    const Color3f&              weight,
    SmallClosureLayerIDStack&   layer_stack,
    Arena&                      arena)
{
    if (closure == nullptr)
        return;

    switch (closure->id)
    {
      case OSL::ClosureColor::MUL:
        {
            const OSL::ClosureMul* c = reinterpret_cast<const OSL::ClosureMul*>(closure);
            const Color3f w = weight * Color3f(c->weight);
            process_closure_tree(c->closure, original_shading_basis, w, layer_stack, arena);
        }
        break;

      case OSL::ClosureColor::ADD:
        {
            const OSL::ClosureAdd* c = reinterpret_cast<const OSL::ClosureAdd*>(closure);
            process_closure_tree(c->closureA, original_shading_basis, weight, layer_stack, arena);
            process_closure_tree(c->closureB, original_shading_basis, weight, layer_stack, arena);
        }
        break;

      default:
        {
            const OSL::ClosureComponent* c = reinterpret_cast<const OSL::ClosureComponent*>(closure);
            const Color3f w = weight * Color3f(c->w);

            if (luminance(w) > 0.0f)
            {
                if (!g_closure_convert_funs[c->id])
                    return;

                g_closure_convert_funs[c->id](*this, original_shading_basis, c->data(), w, arena);
                copy_layer_ids(layer_stack);

                if (c->id >= FirstLayeredClosure)
                {
                    layer_stack.push(get_last_closure_index());

                    // Recurse into nested closures.
                    const OSL::ClosureColor* nested = get_nested_closure_color(c->id, c->data());
                    process_closure_tree(nested, original_shading_basis, w, layer_stack, arena);

                    layer_stack.pop();
                }
            }
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
    SmallClosureLayerIDStack layer_stack;
    process_closure_tree(ci, original_shading_basis, Color3f(1.0f), layer_stack, arena);
    compute_pdfs(m_pdfs);
}

std::size_t CompositeSubsurfaceClosure::choose_closure(const float w) const
{
    assert(get_closure_count() > 0);
    return sample_pdf_linear_search(m_pdfs, get_closure_count(), w);
}

void CompositeSubsurfaceClosure::process_closure_tree(
    const OSL::ClosureColor*    closure,
    const Basis3f&              original_shading_basis,
    const Color3f&              weight,
    SmallClosureLayerIDStack&   layer_stack,
    Arena&                      arena)
{
    if (closure == nullptr)
        return;

    switch (closure->id)
    {
      case OSL::ClosureColor::MUL:
        {
            const OSL::ClosureMul* c = reinterpret_cast<const OSL::ClosureMul*>(closure);
            process_closure_tree(c->closure, original_shading_basis, weight * Color3f(c->weight), layer_stack, arena);
        }
        break;

      case OSL::ClosureColor::ADD:
        {
            const OSL::ClosureAdd* c = reinterpret_cast<const OSL::ClosureAdd*>(closure);
            process_closure_tree(c->closureA, original_shading_basis, weight, layer_stack, arena);
            process_closure_tree(c->closureB, original_shading_basis, weight, layer_stack, arena);
        }
        break;

      default:
        {
            const OSL::ClosureComponent* c = reinterpret_cast<const OSL::ClosureComponent*>(closure);

            const Color3f w = weight * Color3f(c->w);
            if (luminance(w) > 0.0f)
            {
                if (c->id == SubsurfaceID)
                {
                    SubsurfaceClosure::convert_closure(
                        *this,
                        original_shading_basis,
                        c->data(),
                        w,
                        arena);
                    copy_layer_ids(layer_stack);
                }
                else if (c->id == RandomwalkGlassID)
                {
                    RandomwalkGlassClosure::convert_closure(
                        *this,
                        original_shading_basis,
                        c->data(),
                        w,
                        arena);
                    copy_layer_ids(layer_stack);
                }
                else if (c->id >= FirstLayeredClosure)
                {
                    g_closure_convert_funs[c->id](*this, original_shading_basis, c->data(), w, arena);
                    copy_layer_ids(layer_stack);

                    // Prevent this closure from being sampled.
                    override_closure_scalar_weight(0.0f);

                    layer_stack.push(get_last_closure_index());

                    // Recurse into nested closures.
                    const OSL::ClosureColor* nested = get_nested_closure_color(c->id, c->data());
                    process_closure_tree(nested, original_shading_basis, w, layer_stack, arena);

                    layer_stack.pop();
                }
            }
        }
        break;
    }
}


//
// CompositeEmissionClosure class implementation.
//

CompositeEmissionClosure::CompositeEmissionClosure(
    const Basis3f&              original_shading_basis,
    const OSL::ClosureColor*    ci,
    Arena&                      arena)
{
    SmallClosureLayerIDStack layer_stack;
    process_closure_tree(ci, original_shading_basis, Color3f(1.0f), layer_stack, arena);
    compute_pdfs(m_pdfs);
}

std::size_t CompositeEmissionClosure::choose_closure(const float w) const
{
    assert(get_closure_count() > 0);
    return sample_pdf_linear_search(m_pdfs, get_closure_count(), w);
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
    m_weights[m_closure_count].set(weight, g_std_lighting_conditions, Spectrum::Reflectance);
    m_pdfs[m_closure_count] = max_weight_component;

    InputValues* values = arena.allocate<InputValues>();
    m_input_values[m_closure_count] = values;

    ++m_closure_count;

    return values;
}

void CompositeEmissionClosure::process_closure_tree(
    const OSL::ClosureColor*    closure,
    const Basis3f&              original_shading_basis,
    const Color3f&              weight,
    SmallClosureLayerIDStack&   layer_stack,
    Arena&                      arena)
{
    if (closure == nullptr)
        return;

    switch (closure->id)
    {
      case OSL::ClosureColor::MUL:
        {
            const OSL::ClosureMul* c = reinterpret_cast<const OSL::ClosureMul*>(closure);
            process_closure_tree(c->closure, original_shading_basis, weight * Color3f(c->weight), layer_stack, arena);
        }
        break;

      case OSL::ClosureColor::ADD:
        {
            const OSL::ClosureAdd* c = reinterpret_cast<const OSL::ClosureAdd*>(closure);
            process_closure_tree(c->closureA, original_shading_basis, weight, layer_stack, arena);
            process_closure_tree(c->closureB, original_shading_basis, weight, layer_stack, arena);
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
                    copy_layer_ids(layer_stack);
                }
                else if (c->id >= FirstLayeredClosure)
                {
                    g_closure_convert_funs[c->id](*this, original_shading_basis, c->data(), w, arena);
                    copy_layer_ids(layer_stack);

                    // Prevent this closure from being sampled.
                    override_closure_scalar_weight(0.0f);

                    layer_stack.push(get_last_closure_index());

                    // Recurse into nested closures.
                    const OSL::ClosureColor* nested = get_nested_closure_color(c->id, c->data());
                    process_closure_tree(nested, original_shading_basis, w, layer_stack, arena);

                    layer_stack.pop();
                }
            }
        }
        break;
    }
}


//
// CompositeNPRClosure class implementation.
//

CompositeNPRClosure::CompositeNPRClosure(
    const OSL::ClosureColor*    ci,
    Arena&                      arena)
{
    process_closure_tree(ci, Color3f(1.0f), arena);
}

template <typename InputValues>
InputValues* CompositeNPRClosure::add_closure(
    const ClosureID             closure_type,
    const Color3f&              weight,
    Arena&                      arena)
{
    // Make sure we have enough space.
    if APPLESEED_UNLIKELY(get_closure_count() >= MaxClosureEntries)
    {
        throw ExceptionOSLRuntimeError(
            "maximum number of closures in osl shader group exceeded");
    }

    m_closure_types[m_closure_count] = closure_type;
    m_weights[m_closure_count].set(weight, g_std_lighting_conditions, Spectrum::Reflectance);

    InputValues* values = arena.allocate<InputValues>();
    m_input_values[m_closure_count] = values;

    ++m_closure_count;

    return values;
}

std::size_t CompositeNPRClosure::get_nth_contour_closure_index(const std::size_t i) const
{
    std::size_t n = 0;

    for (std::size_t j = 0 , e = get_closure_count(); j < e; ++j)
    {
        if (get_closure_type(j) == NPRContourID)
        {
            if (n == i)
                return j;

            ++n;
        }
    }

    return ~0;
}

void CompositeNPRClosure::process_closure_tree(
    const OSL::ClosureColor*    closure,
    const Color3f&              weight,
    Arena&                      arena)
{
    if (closure == nullptr)
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
            if (c->id == NPRShadingID)
            {
                const Color3f w = weight * Color3f(c->w);
                NPRShadingClosure::convert_closure(
                    *this,
                    c->data(),
                    w,
                    arena);
            }
            else if (c->id == NPRContourID)
            {
                const Color3f w = weight * Color3f(c->w);
                NPRContourClosure::convert_closure(
                    *this,
                    c->data(),
                    w,
                    arena);
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

bool process_matte_tree(
    const OSL::ClosureColor*    closure,
    Color3f&                    matte_color,
    float&                      matte_alpha)
{
    if (closure)
    {
        switch (closure->id)
        {
          case OSL::ClosureColor::MUL:
            {
                const OSL::ClosureMul* c = reinterpret_cast<const OSL::ClosureMul*>(closure);
                const bool is_matte = process_matte_tree(c->closure, matte_color, matte_alpha);
                matte_color = Color3f(c->weight) * matte_color;
                matte_alpha = saturate(luminance(Color3f((c->weight) * matte_alpha)));
                return is_matte;
            }

          case OSL::ClosureColor::ADD:
            {
                const OSL::ClosureAdd* c = reinterpret_cast<const OSL::ClosureAdd*>(closure);
                Color3f color_a(0.0f), color_b(0.0f);
                float alpha_a(0.0f), alpha_b(0.0f);
                const bool is_matte_a = process_matte_tree(c->closureA, color_a, alpha_a);
                const bool is_matte_b = process_matte_tree(c->closureB, color_b, alpha_b);
                matte_color = color_a + color_b;
                matte_alpha = alpha_a + alpha_b;
                return is_matte_a || is_matte_b;
            }

          default:
            {
                const OSL::ClosureComponent* c = reinterpret_cast<const OSL::ClosureComponent*>(closure);

                if (c->id == MatteID)
                {
                    const MatteClosure::Params* p = static_cast<const MatteClosure::Params*>(c->data());
                    matte_color = p->matte_color;
                    matte_alpha = p->matte_alpha;
                    return true;
                }
                else if (c->id == HoldoutID)
                {
                    matte_color.set(0.0f);
                    matte_alpha = 0.0f;
                    return true;
                }
            }
            break;
        }
    }

    return false;
}

Color3f process_background_tree(const OSL::ClosureColor* ci)
{
    return do_process_closure_id_tree(ci, BackgroundID);
}

namespace
{
    template <typename ClosureType>
    void register_closure(OSLShadingSystem& shading_system)
    {
        ClosureType::register_closure(shading_system);
        RENDERER_LOG_DEBUG("registered osl closure %s.", ClosureType::name());
    }
}

void register_closures(OSLShadingSystem& shading_system)
{
    for (std::size_t i = 0; i < NumClosuresIDs; ++i)
    {
        g_closure_convert_funs[i] = nullptr;
        g_closure_get_modes_funs[i] = &closure_no_modes;
    }

    register_closure<AshikhminShirleyClosure>(shading_system);
    register_closure<BackgroundClosure>(shading_system);
    register_closure<BlinnClosure>(shading_system);
    register_closure<DebugClosure>(shading_system);
    register_closure<DiffuseClosure>(shading_system);
    register_closure<DisneyClosure>(shading_system);
    register_closure<EmissionClosure>(shading_system);
    register_closure<GlassClosure>(shading_system);
    register_closure<GlossyClosure>(shading_system);
    register_closure<GlossyLayerClosure>(shading_system);
    register_closure<HairClosure>(shading_system);
    register_closure<HoldoutClosure>(shading_system);
    register_closure<MatteClosure>(shading_system);
    register_closure<MetalClosure>(shading_system);
    register_closure<NPRContourClosure>(shading_system);
    register_closure<NPRShadingClosure>(shading_system);
    register_closure<OrenNayarClosure>(shading_system);
    register_closure<PhongClosure>(shading_system);
    register_closure<PlasticClosure>(shading_system);
    register_closure<RandomwalkGlassClosure>(shading_system);
    register_closure<ReflectionClosure>(shading_system);
    register_closure<SheenClosure>(shading_system);
    register_closure<SubsurfaceClosure>(shading_system);
    register_closure<TranslucentClosure>(shading_system);
    register_closure<TransparentClosure>(shading_system);
}

}   // namespace renderer
