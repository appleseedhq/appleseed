
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "microfacethelper.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/energycompensation.h"
#include "renderer/modeling/bsdf/energycompensationtables.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/qmc.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cmath>

using namespace foundation;
namespace bf = boost::filesystem;

namespace renderer
{

namespace
{
    //
    // References:
    //
    //   [1] A Microfacet Based Coupled Specular-Matte BRDF Model with Importance Sampling
    //       http://sirkan.iit.bme.hu/~szirmay/scook.pdf
    //
    //   [2] Revisiting Physically Based Shading at Imageworks
    //       http://blog.selfshadow.com/publications/s2017-shading-course/imageworks/s2017_pbs_imageworks_slides.pdf
    //

    class MDFAlbedoTable
      : public AlbedoTable2D
    {
      public:
        explicit MDFAlbedoTable(const float* table)
          : AlbedoTable2D(table)
        {
        }

        template <typename MDF>
        explicit MDFAlbedoTable(const MDF& mdf)
        {
            const size_t SampleCount = 512;
            float* p = m_albedo_table;

            // Directional albedo.
            for (size_t i = 0; i < TableSize; ++i)
            {
                const float roughness = static_cast<float>(i) / (TableSize - 1);
                directional_albedo<MDF>(roughness, TableSize, SampleCount, p);
                p += TableSize;
            }

            // Average albedo.
            p = m_albedo_table;
            for (size_t i = 0; i < TableSize; ++i)
            {
                m_avg_table[i] = average_albedo(TableSize, p);
                p += TableSize;
            }
        }

      private:
        // Compute the albedo for a given outgoing direction.
        // See Physically Based Rendering, first edition, pp. 689-690.
        template <typename MDF>
        static void directional_albedo(
            const float     roughness,
            const size_t    table_size,
            const size_t    sample_count,
            float*          values)
        {
            const float alpha = square(roughness);

            for (size_t i = 0; i < table_size; ++i)
            {
                const float cos_theta = static_cast<float>(i) / (table_size - 1);
                values[i] = directional_albedo<MDF>(cos_theta, alpha, sample_count);
            }
        }

        template <typename MDF>
        static float directional_albedo(
            const float     cos_theta,
            const float     alpha,
            const size_t    sample_count)
        {
            // Special cases.
            if (cos_theta == 0.0f || alpha == 0.0f)
                return 1.0f;

            // Build the outgoing vector.
            const float sin_theta = std::sqrt(1.0f - square(cos_theta));
            const Vector3f wo(sin_theta, cos_theta, 0.0f);

            float R = 0.0f;

            for (size_t i = 0; i < sample_count; ++i)
            {
                // Generate a uniform sample in [0,1)^3.
                const size_t Bases[] = { 2 };
                const Vector2f s = hammersley_sequence<float, 2>(Bases, sample_count, i);
                R += sample<GGXMDF>(s, wo, alpha);
            }

            return std::min(R / static_cast<float>(sample_count), 1.0f);
        }

        template <typename MDF>
        static float sample(
            const Vector2f& s,
            const Vector3f& wo,
            const float     alpha)
        {
            Vector3f m = MDF::sample(wo, s, alpha);

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

            const float G = MDF::G(wi, wo, m, alpha);
            const float G1 = MDF::G1(wo, m, alpha);
            return G / G1;
        }
    };

    struct AlbedoTables
      : public NonCopyable
    {
        MDFAlbedoTable m_ggx;

#ifdef COMPUTE_ALBEDO_TABLES
        AlbedoTables()
          : m_ggx(GGXMDF())
        {
        }
#else
        AlbedoTables()
          : m_ggx(g_glossy_ggx_albedo_table)
        {
        }
#endif
    };

    AlbedoTables g_dir_albedo_tables;
}

float get_directional_albedo(
    const float     cos_theta,
    const float     roughness)
{
    return g_dir_albedo_tables.m_ggx.get_directional_albedo(
        cos_theta,
        roughness);
}

float get_average_albedo(const float roughness)
{
    return g_dir_albedo_tables.m_ggx.get_average_albedo(roughness);
}

void write_microfacet_directional_albedo_tables(
    const char*     directory)
{
    const bf::path dir(directory);

    const GGXMDF ggx = {};
    const MDFAlbedoTable ggx_table(ggx);
    ggx_table.write_table_to_image(dir / "glossy_ggx_albedo_table.exr");
    ggx_table.write_table_to_cpp_array(
        dir / "glossy_ggx_albedo_table.cpp",
        "g_glossy_ggx_albedo_table");
}

}   // namespace renderer
