
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
#include "foundation/math/cdf.h"
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

using OSL::TypeDesc;

namespace renderer
{
namespace
{
    //
    // Closures.
    //

    typedef void(*convert_closure_fun)(
        CompositeSurfaceClosure&    composite_closure,
        const Basis3d&              shading_basis,
        const void*                 osl_params,
        const Color3f&              weight);

    convert_closure_fun g_closure_convert_funs[NumClosuresIDs];

    void convert_closure_nop(
        CompositeSurfaceClosure&    composite_closure,
        const Basis3d&              shading_basis,
        const void*                 osl_params,
        const Color3f&              weight)
    {
    }

    struct AshikhminShirleyClosure
    {
        typedef AshikhminBRDFInputValues InputValues;

        struct Params
        {
            OSL::Vec3   N;
            OSL::Vec3   T;
            OSL::Color3 rd;
            OSL::Color3 rg;
            float       nu;
            float       nv;
            float       fr;
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
                CLOSURE_COLOR_PARAM(Params, rd),
                CLOSURE_COLOR_PARAM(Params, rg),
                CLOSURE_FLOAT_PARAM(Params, nu),
                CLOSURE_FLOAT_PARAM(Params, nv),
                CLOSURE_FLOAT_PARAM(Params, fr),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3d&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight)
        {
            const Params* p = reinterpret_cast<const Params*>(osl_params);
            InputValues* values =
                composite_closure.add_closure<InputValues>(
                    static_cast<ClosureID>(id()),
                    shading_basis,
                    weight,
                    Vector3d(p->N),
                    Vector3d(p->T));

            values->m_rd = Color3f(p->rd);
            values->m_rd_multiplier = 1.0;
            values->m_rg = Color3f(p->rg);
            values->m_rg_multiplier = 1.0;
            values->m_nu = max(p->nu, 0.01f);
            values->m_nv = max(p->nv, 0.01f);
            values->m_fr_multiplier = p->fr;
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

    struct DisneyClosure
    {
        typedef DisneyBRDFInputValues InputValues;

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
            const Basis3d&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight)
        {
            const Params* p = reinterpret_cast<const Params*>(osl_params);
            InputValues* values =
                composite_closure.add_closure<InputValues>(
                    static_cast<ClosureID>(id()),
                    shading_basis,
                    weight,
                    Vector3d(p->N),
                    Vector3d(p->T));

            values->m_base_color = Color3f(p->base_color);
            values->m_subsurface = saturate(p->subsurface);
            values->m_metallic = saturate(p->metallic);
            values->m_specular = max(p->specular, 0.0f);
            values->m_specular_tint = saturate(p->specular_tint);
            values->m_anisotropic = saturate(p->anisotropic);
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

    OSL::ustring g_beckmann_mdf_str("beckmann");
    OSL::ustring g_blinn_mdf_str("blinn");
    OSL::ustring g_ggx_mdf_str("ggx");

    struct MicrofacetClosure
    {
        struct Params
        {
            OSL::ustring    dist;
            OSL::Vec3       N;
            OSL::Vec3       T;
            float           xalpha;
            float           yalpha;
            float           eta;
            int             refract;
        };

        static const char* name()
        {
            return "microfacet";
        }

        static ClosureID id()
        {
            return MicrofacetID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_STRING_PARAM(Params, dist),
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_VECTOR_PARAM(Params, T),
                CLOSURE_FLOAT_PARAM(Params, xalpha),
                CLOSURE_FLOAT_PARAM(Params, yalpha),
                CLOSURE_FLOAT_PARAM(Params, eta),
                CLOSURE_INT_PARAM(Params, refract),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3d&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight)
        {
            const Params* p = reinterpret_cast<const Params*>(osl_params);

            if (p->refract == 0)
            {
                OSLMicrofacetBRDFInputValues* values;

                if (p->dist == g_blinn_mdf_str)
                {
                    values =
                        composite_closure.add_closure<OSLMicrofacetBRDFInputValues>(
                            MicrofacetBlinnReflectionID,
                            shading_basis,
                            weight,
                            Vector3d(p->N),
                            Vector3d(p->T));
                }
                else if (p->dist == g_ggx_mdf_str)
                {
                    values =
                        composite_closure.add_closure<OSLMicrofacetBRDFInputValues>(
                            MicrofacetGGXReflectionID,
                            shading_basis,
                            weight,
                            Vector3d(p->N),
                            Vector3d(p->T));
                }
                else if(p->dist == g_beckmann_mdf_str)
                {
                    values =
                        composite_closure.add_closure<OSLMicrofacetBRDFInputValues>(
                            MicrofacetBeckmannReflectionID,
                            shading_basis,
                            weight,
                            Vector3d(p->N),
                            Vector3d(p->T));
                }
                else
                {
                    string msg("invalid microfacet distribution function: ");
                    msg += p->dist.c_str();
                    throw ExceptionOSLRuntimeError(msg.c_str());
                }

                values->m_ax = max(p->xalpha, 0.0001f);
                values->m_ay = max(p->yalpha, 0.0001f);
            }
            else if (p->refract == 1)
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

                OSLMicrofacetBTDFInputValues* values;

                if (p->dist == g_ggx_mdf_str)
                {
                    values =
                        composite_closure.add_closure<OSLMicrofacetBTDFInputValues>(
                            MicrofacetGGXRefractionID,
                            shading_basis,
                            weight,
                            Vector3d(p->N),
                            Vector3d(p->T));
                }
                else if(p->dist == g_beckmann_mdf_str)
                {
                    values =
                        composite_closure.add_closure<OSLMicrofacetBTDFInputValues>(
                            MicrofacetBeckmannRefractionID,
                            shading_basis,
                            weight,
                            Vector3d(p->N),
                            Vector3d(p->T));
                }
                else
                {
                    string msg("invalid microfacet distribution function: ");
                    msg += p->dist.c_str();
                    throw ExceptionOSLRuntimeError(msg.c_str());
                }

                values->m_ax = max(p->xalpha, 0.0001f);
                values->m_ay = max(p->yalpha, 0.0001f);
                values->m_from_ior = from_ior;
                values->m_to_ior = to_ior;
            }
            else
            {
                throw ExceptionOSLRuntimeError("invalid microfacet refraction mode.");
            }
        }
    };

    struct OrenNayarClosure
    {
        typedef OrenNayarBRDFInputValues InputValues;

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
            const Basis3d&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight)
        {
            const Params* p = reinterpret_cast<const Params*>(osl_params);
            InputValues* values =
                composite_closure.add_closure<InputValues>(
                    static_cast<ClosureID>(id()),
                    shading_basis,
                    weight,
                    Vector3d(p->N));

            values->m_reflectance.set(1.0f);
            values->m_reflectance_multiplier = 1.0;
            values->m_roughness = max(p->roughness, 0.0f);
        }
    };

    struct ReflectionClosure
    {
        typedef SpecularBRDFInputValues InputValues;

        struct Params
        {
            OSL::Vec3 N;
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
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3d&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight)
        {
            const Params* p = reinterpret_cast<const Params*>(osl_params);
            InputValues* values =
                composite_closure.add_closure<InputValues>(
                    static_cast<ClosureID>(id()),
                    shading_basis,
                    weight,
                    Vector3d(p->N));

            values->m_reflectance.set(1.0f);
            values->m_reflectance_multiplier = 1.0;
        }
    };

    struct RefractionClosure
    {
        typedef SpecularBTDFInputValues InputValues;

        struct Params
        {
            OSL::Vec3 N;
            float     eta;
        };

        static const char* name()
        {
            return "refraction";
        }

        static ClosureID id()
        {
            return RefractionID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_FLOAT_PARAM(Params, eta),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);

            g_closure_convert_funs[id()] = &convert_closure;
        }

        static void convert_closure(
            CompositeSurfaceClosure&    composite_closure,
            const Basis3d&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight)
        {
            const Params* p = reinterpret_cast<const Params*>(osl_params);

            // Assume one of the media is air.
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

            InputValues* values =
                composite_closure.add_closure<InputValues>(
                    static_cast<ClosureID>(id()),
                    shading_basis,
                    weight,
                    Vector3d(p->N));

            values->m_reflectance.set(0.0f);
            values->m_reflectance_multiplier = 0.0;
            values->m_transmittance.set(1.0f);
            values->m_transmittance_multiplier = 1.0;
            values->m_fresnel_multiplier = 0.0;
            values->m_from_ior = from_ior;
            values->m_to_ior = to_ior;
        }
    };

    struct SheenClosure
    {
        typedef SheenBRDFInputValues InputValues;

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
            const Basis3d&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight)
        {
            const Params* p = reinterpret_cast<const Params*>(osl_params);
            InputValues* values =
                composite_closure.add_closure<InputValues>(
                    static_cast<ClosureID>(id()),
                    shading_basis,
                    weight,
                    Vector3d(p->N));

            values->m_reflectance.set(1.0f);
            values->m_reflectance_multiplier = 1.0;
        }
    };

    OSL::ustring g_standard_dipole_profile_str("standard_dipole");
    OSL::ustring g_better_dipole_profile_str("better_dipole");
    OSL::ustring g_directional_dipole_profile_str("directional_dipole");
    OSL::ustring g_normalized_diffusion_profile_str("normalized_diffusion");

    struct SubsurfaceClosure
    {
        struct Params
        {
            OSL::ustring    profile;
            OSL::Vec3       N;
            OSL::Color3     rd;
            OSL::Color3     dmfp;
            float           eta;
            float           g;
        };

        static const char* name()
        {
            return "as_subsurface";
        }

        static ClosureID id()
        {
            return SubsurfaceID;
        }

        static void register_closure(OSL::ShadingSystem& shading_system)
        {
            const OSL::ClosureParam params[] =
            {
                CLOSURE_STRING_PARAM(Params, profile),
                CLOSURE_VECTOR_PARAM(Params, N),
                CLOSURE_COLOR_PARAM(Params, rd),
                CLOSURE_COLOR_PARAM(Params, dmfp),
                CLOSURE_FLOAT_PARAM(Params, eta),
                CLOSURE_FLOAT_PARAM(Params, g),
                CLOSURE_FINISH_PARAM(Params)
            };

            shading_system.register_closure(name(), id(), params, 0, 0);
        }

        static void convert_closure(
            CompositeSubsurfaceClosure&     composite_closure,
            const Basis3d&                  shading_basis,
            const void*                     osl_params,
            const Color3f&                  weight)
        {
            const Params* p = reinterpret_cast<const Params*>(osl_params);

            if (p->profile == g_normalized_diffusion_profile_str)
            {
#ifdef APPLESEED_WITH_NORMALIZED_DIFFUSION_BSSRDF
                NormalizedDiffusionBSSRDFInputValues* values =
                    composite_closure.add_closure<NormalizedDiffusionBSSRDFInputValues>(
                        SubsurfaceNormalizedDiffusionID,
                        shading_basis,
                        weight,
                        Vector3d(p->N));

                values->m_weight = 1.0;
                values->m_reflectance = Color3f(p->rd);
                values->m_reflectance_multiplier = 1.0;
                values->m_dmfp = luminance(Color3f(p->dmfp));
                values->m_dmfp_multiplier = 1.0;
                values->m_inside_ior = p->eta;
                values->m_outside_ior = 1.0;
#else
                throw ExceptionOSLRuntimeError("unknown subsurface profile: normalized_diffusion");
#endif
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
                            Vector3d(p->N));
                }
                else if (p->profile == g_standard_dipole_profile_str)
                {
                    values =
                        composite_closure.add_closure<DipoleBSSRDFInputValues>(
                            SubsurfaceStandardDipoleID,
                            shading_basis,
                            weight,
                            Vector3d(p->N));
                }
                else if (p->profile == g_directional_dipole_profile_str)
                {
                    values =
                        composite_closure.add_closure<DipoleBSSRDFInputValues>(
                            SubsurfaceDirectionalDipoleID,
                            shading_basis,
                            weight,
                            Vector3d(p->N));
                }
                else
                {
                    string msg = "unknown subsurface profile: ";
                    msg += p->profile.c_str();
                    throw ExceptionOSLRuntimeError(msg.c_str());
                }

                values->m_weight = 1.0;
                values->m_reflectance = Color3f(p->rd);
                values->m_reflectance_multiplier = 1.0;
                values->m_dmfp = luminance(Color3f(p->dmfp));
                values->m_dmfp_multiplier = 1.0;
                values->m_anisotropy = p->g;
                values->m_inside_ior = p->eta;
                values->m_outside_ior = 1.0;
            }
        }
    };

    struct TranslucentClosure
    {
        typedef DiffuseBTDFInputValues InputValues;

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
            const Basis3d&              shading_basis,
            const void*                 osl_params,
            const Color3f&              weight)
        {
            const Params* p = reinterpret_cast<const Params*>(osl_params);
            InputValues* values =
                composite_closure.add_closure<InputValues>(
                    static_cast<ClosureID>(id()),
                    shading_basis,
                    weight,
                    Vector3d(p->N));

            values->m_transmittance.set(1.0f);
            values->m_transmittance_multiplier = 1.0;
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
}


//
// CompositeClosure class implementation.
//

CompositeClosure::CompositeClosure()
  : m_num_closures(0)
  , m_num_bytes(0)
{
    assert(is_aligned(m_pool, InputValuesAlignment));
}

void CompositeClosure::compute_cdf()
{
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

size_t CompositeClosure::choose_closure(const double w) const
{
    return sample_cdf(m_cdf, m_cdf + get_num_closures(), w);
}

void CompositeClosure::compute_closure_shading_basis(
    const Vector3d& normal,
    const Basis3d&  original_shading_basis)
{
    // The normalization of the normal shouldn't be needed here,
    // because the spec says that normals passed to closures
    // must be normalized. But, since OSL uses floats, the normals are
    // only normalized to float precision and not double precision.
    m_bases[m_num_closures] =
        Basis3d(
            normalize(normal),
            original_shading_basis.get_tangent_u());
}

void CompositeClosure::compute_closure_shading_basis(
    const Vector3d& normal,
    const Vector3d& tangent,
    const Basis3d&  original_shading_basis)
{
    if (square_norm(tangent) != 0.0)
    {
        m_bases[m_num_closures] =
            Basis3d(
                normalize(normal),
                normalize(tangent));
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
    const Basis3d&              original_shading_basis,
    const Color3f&              weight,
    const Vector3d&             normal)
{
    return do_add_closure<InputValues>(
        closure_type,
        original_shading_basis,
        weight,
        normal,
        false,
        Vector3d(0.0));
}

template <typename InputValues>
InputValues* CompositeClosure::add_closure(
    const ClosureID             closure_type,
    const Basis3d&              original_shading_basis,
    const Color3f&              weight,
    const Vector3d&             normal,
    const Vector3d&             tangent)
{
    return do_add_closure<InputValues>(
        closure_type,
        original_shading_basis,
        weight,
        normal,
        true,
        tangent);
}

template <typename InputValues>
InputValues* CompositeClosure::do_add_closure(
    const ClosureID             closure_type,
    const Basis3d&              original_shading_basis,
    const Color3f&              weight,
    const Vector3d&             normal,
    bool                        has_tangent,
    const Vector3d&             tangent)
{
    // Check that InputValues is included in our type list.
    typedef typename boost::mpl::contains<InputValuesTypeList, InputValues>::type value_in_list;
    BOOST_STATIC_ASSERT(value_in_list::value);

    // Make sure we have enough space.
    if (get_num_closures() >= MaxClosureEntries)
    {
        throw ExceptionOSLRuntimeError(
            "maximum number of closures in OSL shader group exceeded.");
    }

    assert(m_num_bytes + sizeof(InputValues) <= MaxPoolSize);

    // We use the luminance of the weight as the BSDF weight.
    const double w = luminance(weight);
    assert(w > 0.0);

    m_pdf_weights[m_num_closures] = w;
    m_weights[m_num_closures] = weight;

    if (!has_tangent)
        compute_closure_shading_basis(normal, original_shading_basis);
    else
        compute_closure_shading_basis(normal, tangent, original_shading_basis);

    m_closure_types[m_num_closures] = closure_type;

    char* values_ptr = m_pool + m_num_bytes;
    assert(is_aligned(values_ptr, InputValuesAlignment));
    new (values_ptr) InputValues();
    m_input_values[m_num_closures] = values_ptr;
    m_num_bytes += align(sizeof(InputValues), InputValuesAlignment);
    ++m_num_closures;

    return reinterpret_cast<InputValues*>(values_ptr);
}


//
// CompositeSurfaceClosure class implementation.
//

BOOST_STATIC_ASSERT(sizeof(CompositeSurfaceClosure) <= InputEvaluator::DataSize);

CompositeSurfaceClosure::CompositeSurfaceClosure(
    const Basis3d&              original_shading_basis,
    const OSL::ClosureColor*    ci)
{
    process_closure_tree(ci, original_shading_basis, Color3f(1.0f));
    compute_cdf();
}

void CompositeSurfaceClosure::process_closure_tree(
    const OSL::ClosureColor*    closure,
    const Basis3d&              original_shading_basis,
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
            process_closure_tree(c->closure, original_shading_basis, w);
        }
        break;

      case OSL::ClosureColor::ADD:
        {
            const OSL::ClosureAdd* c = reinterpret_cast<const OSL::ClosureAdd*>(closure);
            process_closure_tree(c->closureA, original_shading_basis, weight);
            process_closure_tree(c->closureB, original_shading_basis, weight);
        }
        break;

      case OSL::ClosureColor::COMPONENT:
        {
            const OSL::ClosureComponent* c = reinterpret_cast<const OSL::ClosureComponent*>(closure);
            const Color3f w = weight * Color3f(c->w);

            if (luminance(w) > 0.0f)
                g_closure_convert_funs[c->id](*this, original_shading_basis, c->data(), w);
        }
        break;

      assert_otherwise;
    }
}


//
// CompositeSubsurfaceClosure class implementation.
//

CompositeSubsurfaceClosure::CompositeSubsurfaceClosure(
    const Basis3d&              original_shading_basis,
    const OSL::ClosureColor*    ci)
{
    process_closure_tree(ci, original_shading_basis, Color3f(1.0f));
    compute_cdf();
}

void CompositeSubsurfaceClosure::process_closure_tree(
    const OSL::ClosureColor*    closure,
    const Basis3d&              original_shading_basis,
    const foundation::Color3f&  weight)
{
    if (closure == 0)
        return;

    switch (closure->type)
    {
      case OSL::ClosureColor::MUL:
        {
            const OSL::ClosureMul* c = reinterpret_cast<const OSL::ClosureMul*>(closure);
            process_closure_tree(c->closure, original_shading_basis, weight * Color3f(c->weight));
        }
        break;

      case OSL::ClosureColor::ADD:
        {
            const OSL::ClosureAdd* c = reinterpret_cast<const OSL::ClosureAdd*>(closure);
            process_closure_tree(c->closureA, original_shading_basis, weight);
            process_closure_tree(c->closureB, original_shading_basis, weight);
        }
        break;

      case OSL::ClosureColor::COMPONENT:
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
                        w);
                }
            }
        }
        break;

      assert_otherwise;
    }
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
            process_closure_tree(c->closure, weight * Color3f(c->weight));
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

            if (c->id == EmissionID)
                m_total_weight += weight * Color3f(c->w);
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

namespace
{
    template <typename ClosureType>
    void register_closure(OSL::ShadingSystem& shading_system)
    {
        ClosureType::register_closure(shading_system);
        RENDERER_LOG_INFO("registered OSL closure %s.", ClosureType::name());
    }
}

void register_closures(OSL::ShadingSystem& shading_system)
{
    for (size_t i = 0; i < NumClosuresIDs; ++i)
        g_closure_convert_funs[i] = &convert_closure_nop;

    register_closure<AshikhminShirleyClosure>(shading_system);
    register_closure<BackgroundClosure>(shading_system);
    register_closure<DebugClosure>(shading_system);
    register_closure<DisneyClosure>(shading_system);
    register_closure<EmissionClosure>(shading_system);
    register_closure<HoldoutClosure>(shading_system);
    register_closure<MicrofacetClosure>(shading_system);
    register_closure<OrenNayarClosure>(shading_system);
    register_closure<ReflectionClosure>(shading_system);
    register_closure<RefractionClosure>(shading_system);
    register_closure<SheenClosure>(shading_system);
    register_closure<SubsurfaceClosure>(shading_system);
    register_closure<TranslucentClosure>(shading_system);
    register_closure<TransparentClosure>(shading_system);
}

}   // namespace renderer
