
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Esteban Tovagliari, The appleseedhq Organization
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
#include "glossylayerbsdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/energycompensation.h"
#include "renderer/modeling/bsdf/energycompensationtables.h"
#include "renderer/modeling/bsdf/fresnel.h"
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/vector.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <string>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;
namespace bfs = boost::filesystem;

namespace renderer
{

namespace
{
    //
    // Glossy Layer BSDF.
    //
    // References:
    //
    //   [1] Microfacet Models for Refraction through Rough Surfaces
    //       http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
    //
    //   [2] Physically-Based Shading at Disney
    //       https://disney-animation.s3.amazonaws.com/library/s2012_pbs_disney_brdf_notes_v2.pdf
    //
    //   [3] Revisiting Physically Based Shading at Imageworks
    //       http://blog.selfshadow.com/publications/s2017-shading-course/imageworks/s2017_pbs_imageworks_slides.pdf
    //

    const char* Model = "glossy_layer_bsdf";

    class GlossyLayerBSDFImpl
      : public BSDF
    {
      public:
        GlossyLayerBSDFImpl(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, Reflective, ScatteringMode::Glossy, params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        std::size_t compute_input_data_size() const override
        {
            return sizeof(InputValues);
        }

        void prepare_inputs(
            Arena&                      arena,
            const ShadingPoint&         shading_point,
            void*                       data) const override
        {
            InputValues* values = static_cast<InputValues*>(data);
            new (&values->m_precomputed) InputValues::Precomputed();

            values->m_roughness = std::max(values->m_roughness, shading_point.get_ray().m_min_roughness);
            values->m_precomputed.m_outside_ior = shading_point.get_ray().get_current_ior();
            normal_reflectance_dielectric(
                values->m_precomputed.m_F0,
                values->m_ior / values->m_precomputed.m_outside_ior);
        }

        void sample(
            SamplingContext&            sampling_context,
            const void*                 data,                       // input values
            const bool                  adjoint,                    // if true, use the adjoint scattering kernel
            const bool                  cosine_mult,                // if true, multiply by |cos(incoming, normal)|
            const LocalGeometry&        local_geometry,
            const foundation::Dual3f&   outgoing,                   // world space outgoing direction, unit-length
            const int                   modes,                      // allowed scattering modes
            BSDFSample&                 sample) const override
        {
            if (!ScatteringMode::has_glossy(modes))
                return;

            const InputValues* values = static_cast<const InputValues*>(data);

            float alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropy,
                alpha_x,
                alpha_y);

            const FresnelDielectricFun f(
                values->m_reflectance,
                1.0f,
                values->m_precomputed.m_outside_ior / values->m_ior,
                1.0f);

            MicrofacetBRDFHelper<GGXMDF>::sample(
                sampling_context,
                values->m_roughness,
                alpha_x,
                alpha_y,
                f,
                local_geometry,
                outgoing,
                sample);

            if (sample.get_mode() != ScatteringMode::None)
            {
                apply_energy_compensation_factor(
                    values,
                    outgoing.get_value(),
                    local_geometry.m_shading_basis.get_normal(),
                    sample.m_value.m_glossy);

                sample.m_min_roughness = values->m_roughness;
            }

            sample.m_value.m_beauty = sample.m_value.m_glossy;
        }

        float evaluate(
            const void*                 data,                       // input values
            const bool                  adjoint,                    // if true, use the adjoint scattering kernel
            const bool                  cosine_mult,                // if true, multiply by |cos(incoming, normal)|
            const LocalGeometry&        local_geometry,
            const foundation::Vector3f& outgoing,                   // world space outgoing direction, unit-length
            const foundation::Vector3f& incoming,                   // world space incoming direction, unit-length
            const int                   modes,                      // enabled scattering modes
            DirectShadingComponents&    value) const override
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);

            float alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropy,
                alpha_x,
                alpha_y);

            const FresnelDielectricFun f(
                values->m_reflectance,
                1.0f,
                values->m_precomputed.m_outside_ior / values->m_ior,
                1.0f);

            const float pdf =
                MicrofacetBRDFHelper<GGXMDF>::evaluate(
                    alpha_x,
                    alpha_y,
                    f,
                    local_geometry,
                    outgoing,
                    incoming,
                    value.m_glossy);

            apply_energy_compensation_factor(
                values,
                outgoing,
                local_geometry.m_shading_basis.get_normal(),
                value.m_glossy);

            value.m_beauty = value.m_glossy;

            assert(pdf >= 0.0f);
            return pdf;
        }

        float evaluate_pdf(
            const void*                 data,                       // input values
            const bool                  adjoint,                    // if true, use the adjoint scattering kernel
            const LocalGeometry&        local_geometry,
            const foundation::Vector3f& outgoing,                   // world space outgoing direction, unit-length
            const foundation::Vector3f& incoming,                   // world space incoming direction, unit-length
            const int                   modes) const override       // enabled scattering modes
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);

            float alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropy,
                alpha_x,
                alpha_y);

            const float pdf =
                MicrofacetBRDFHelper<GGXMDF>::pdf(
                    alpha_x,
                    alpha_y,
                    local_geometry,
                    outgoing,
                    incoming);

            assert(pdf >= 0.0f);
            return pdf;
        }

        void attenuate_substrate(
            const void*                 data,
            const foundation::Basis3f&  shading_basis,
            const foundation::Vector3f& outgoing,
            const foundation::Vector3f& incoming,
            Spectrum&                   value) const override
        {
            do_attenuate_substrate(data, shading_basis, outgoing, incoming, value);
        }

        void attenuate_substrate(
            const void*                 data,
            const foundation::Basis3f&  shading_basis,
            const foundation::Vector3f& outgoing,
            const foundation::Vector3f& incoming,
            DirectShadingComponents&    value) const override
        {
            do_attenuate_substrate(data, shading_basis, outgoing, incoming, value);
        }

        void attenuate_emission(
            const void*                 data,
            const Basis3f&              shading_basis,
            const Vector3f&             outgoing,
            Spectrum&                   value) const override
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3f& normal = shading_basis.get_normal();
            const float cos_on = dot(normal, outgoing);

            const float eta = 1.0f / values->m_ior;
            const float Eo = get_dielectric_layer_directional_albedo(
                eta,
                values->m_roughness,
                std::abs(cos_on));

            value *= 1.0f - Eo;

            if (values->m_thickness != 0.0f)
            {
                const float Lo = safe_rcp(std::abs(cos_on), 1e-6f);
                value *= pow(values->m_transmittance, values->m_thickness * Lo);
            }
        }

      private:
        typedef GlossyLayerBSDFInputValues InputValues;

        static void apply_energy_compensation_factor(
            const InputValues*          values,
            const Vector3f&             outgoing,
            const Vector3f&             n,
            Spectrum&                   value)
        {
            const float Ess = get_directional_albedo(
                std::abs(dot(outgoing, n)),
                values->m_roughness);

            if (Ess == 0.0f)
                return;

            const float fms = values->m_precomputed.m_F0 * (1.0f - Ess) / Ess;
            value *= 1.0f + fms;
        }

        template <typename SpectrumType>
        void do_attenuate_substrate(
            const void*                 data,
            const foundation::Basis3f&  shading_basis,
            const foundation::Vector3f& outgoing,
            const foundation::Vector3f& incoming,
            SpectrumType&               value) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3f& normal = shading_basis.get_normal();
            const float cos_on = dot(normal, outgoing);
            const float cos_in = dot(normal, incoming);

            const float eta = 1.0f / values->m_ior;
            const float Eo = get_dielectric_layer_directional_albedo(
                eta,
                values->m_roughness,
                std::abs(cos_on));
            const float Ei = get_dielectric_layer_directional_albedo(
                eta,
                values->m_roughness,
                std::abs(cos_in));

            value *= std::min(1.0f - Ei, 1.0f - Eo);

            if (values->m_thickness != 0.0f)
            {
                const float Lo = safe_rcp(std::abs(cos_on), 1e-6f);
                const float Li = safe_rcp(std::abs(cos_in), 1e-6f);
                float L = Li + Lo;

                // For transmission, use the average length.
                if (cos_in * cos_on <= 0.0f)
                    L *= 0.5f;

                value *= pow(values->m_transmittance, values->m_thickness * L);
            }
        }
    };

    typedef BSDFWrapper<GlossyLayerBSDFImpl> GlossyLayerBSDF;

    const float MinIOR = 1.01f;
    const float MaxIOR = 3.0f;

    class GlossyLayerAlbedoTable
      : public AlbedoTable3D
    {
      public:
        GlossyLayerAlbedoTable(const float* table, const float min_eta, const float max_eta)
          : AlbedoTable3D(table, min_eta, max_eta)
        {
        }

        GlossyLayerAlbedoTable(const float min_eta, const float max_eta)
          : AlbedoTable3D(min_eta, max_eta)
        {
            for (std::size_t z = 0; z < TableSize; ++z)
            {
                const float eta = lerp(m_min_eta, m_max_eta, static_cast<float>(z) / (TableSize - 1));

                for (std::size_t y = 0; y < TableSize; ++y)
                {
                    const float roughness = static_cast<float>(y) / (TableSize - 1);
                    const float alpha = max(square(roughness), 0.001f);

                    for (std::size_t x = 0; x < TableSize; ++x)
                    {
                        const float cos_theta = static_cast<float>(x) / (TableSize - 1);
                        dir_table(x, y, z) = compute_directional_albedo(eta, alpha, cos_theta);
                    }

                    avg_table(y, z) = average_albedo(TableSize, &dir_table(0, y, z));
                }
            }
        }

      private:
        // Compute the albedo for a given outgoing direction.
        // See Physically Based Rendering, first edition, pp. 689-690.
        float compute_directional_albedo(
            const float eta,
            const float alpha,
            const float cos_theta) const
        {
            const std::size_t SampleCount = 512;

            // Special cases.
            if (cos_theta == 0.0f || alpha == 0.0f)
                return 1.0f;

            // Build the outgoing vector.
            const float sin_theta = sqrt(1.0f - square(cos_theta));
            const Vector3f wo(sin_theta, cos_theta, 0.0f);

            float R = 0.0f;

            for (std::size_t i = 0; i < SampleCount; ++i)
            {
                // Generate a uniform sample in [0,1)^3.
                const std::size_t Bases[] = { 2 };
                const Vector2f s = hammersley_sequence<float, 2>(Bases, SampleCount, i);
                R += sample(s, eta, alpha, wo);
            }

            return std::min(R / static_cast<float>(SampleCount), 1.0f);
        }

        float sample(
            const Vector2f&     s,
            const float         eta,
            const float         alpha,
            const Vector3f&     wo) const
        {
            Vector3f m = GGXMDF::sample(wo, s, alpha);

            const float cos_oh = std::abs(dot(wo, m));
            const float cos_on = std::abs(wo.y);

            if (cos_on == 0.0f || cos_oh == 0.0f)
                return 0.0f;

            const Vector3f n(0.0f, 1.0f, 0.0f);
            Vector3f wi = reflect(wo, m);

            if (BSDF::force_above_surface(wi, n))
                m = normalize(wo + wi);

            const float cos_in = std::abs(wi.y);

            if (cos_in == 0.0f)
                return 0.0f;

            float F;
            fresnel_reflectance_dielectric(F, eta, saturate(abs(dot(wo, m))));

            const float G = GGXMDF::G(wi, wo, m, alpha);
            const float G1 = GGXMDF::G1(wo, m, alpha);
            return F * G / G1;
        }
    };

    struct GlossyLayerAlbedoTables
      : public NonCopyable
    {
#ifdef COMPUTE_ALBEDO_TABLES
        GlossyLayerAlbedoTables()
          : m_ggx(1.0f / MaxIOR, 1.0f / MinIOR)
        {
        }
#else
        GlossyLayerAlbedoTables()
          : m_ggx(g_dielectric_layer_albedo_table, 1.0f / MaxIOR, 1.0f / MinIOR)
        {
        }
#endif

        GlossyLayerAlbedoTable m_ggx;
    };

    GlossyLayerAlbedoTables g_dir_albedo_tables;
}


//
// GlossyLayerBSDFFactory class implementation.
//

auto_release_ptr<BSDF> GlossyLayerBSDFFactory::create(const char* name, const ParamArray& params)
{
    return auto_release_ptr<BSDF>(new GlossyLayerBSDF(name, params));
}


//
// Albedo tables.
//

float get_dielectric_layer_directional_albedo(const float eta, const float roughness, const float cos_theta)
{
    return g_dir_albedo_tables.m_ggx.get_directional_albedo(eta, roughness, cos_theta);
}

float get_dielectric_layer_average_albedo(const float eta, const float roughness)
{
    return g_dir_albedo_tables.m_ggx.get_average_albedo(eta, roughness);
}

void write_dielectric_layer_directional_albedo_tables(const char* directory)
{
    const bfs::path dir(directory);

    const GlossyLayerAlbedoTable ggx_table(1.0f / MaxIOR, 1.0f / MinIOR);

    ggx_table.write_table_to_image(
        dir / "dielectric_layer_albedo_table.exr");
    ggx_table.write_table_to_cpp_array(
        dir / "dielectric_layer_albedo_table.cpp",
        "g_dielectric_layer_albedo_table");
}

}   // namespace renderer
