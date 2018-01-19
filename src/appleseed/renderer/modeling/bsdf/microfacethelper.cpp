
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/exrimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/qmc.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cmath>

using namespace foundation;
using namespace std;

namespace bfs = boost::filesystem;

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

    // Compute the albedo for a given outgoing direction.
    // See Physically Based Rendering, first edition, pp. 689-690.
    template <typename MDF>
    float directional_albedo(
        const float     cos_theta,
        const float     alpha,
        const size_t    sample_count)
    {
        // Special cases.
        if (cos_theta == 0.0f)
            return 1.0f;

        if (alpha == 0.0f)
            return 1.0f;

        // Build the outgoing vector.
        const float sin_theta = std::sqrt(1.0f - square(cos_theta));
        const Vector3f wo(sin_theta, cos_theta, 0.0f);

        float R = 0.0f;
        const MDF mdf;

        for (size_t i = 0; i < sample_count; ++i)
        {
            // Generate a uniform sample in [0,1)^2.
            static const size_t Bases[] = { 2 };
            const Vector2f s = hammersley_sequence<float, 2>(Bases, sample_count, i);

            // Sample the MDF.
            Vector3f h = mdf.sample(s, alpha);

            if (h.y == 0.0f)
                continue;

            const float cos_oh = dot(wo, h);
            if (cos_oh <= 0.0f)
                continue;

            const Vector3f wi = reflect(wo, h);

            const float G = mdf.G(wi, wo, h, alpha, alpha, 1.0f);

            // (D * G / (4.0f * cos_theta * cos_in)) * cos_in
            // ----------------------------------------------
            //       (D * cos_nh / (4.0f * cos_oh));
            //
            // Simplified:

            R += G * cos_oh / (h.y * cos_theta);
        }

        return min(R / static_cast<float>(sample_count), 1.0f);
    }

    template <typename MDF>
    void directional_albedo(
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

    float average_albedo(
        const size_t    table_size,
        const float*    directional_albedo)
    {
        float avg = 0.0f;

        for (size_t i = 0; i < table_size; ++i)
        {
            const float cos_theta = static_cast<float>(i) / (table_size - 1);
            avg += directional_albedo[i] * cos_theta;
        }

        return (2.0f * avg) / table_size;
    }

    class AlbedoTable
      : public NonCopyable
    {
      public:
        template <typename MDF>
        explicit AlbedoTable(const MDF& mdf)
          : TableSize(32)
          , TableHeight(33)
          , SampleCount(1024)
        {
            m_albedo_table = new float[TableSize * TableHeight];
            m_avg_table = m_albedo_table + TableSize * TableSize;

            float* p = m_albedo_table;
            for (size_t i = 0; i < TableSize; ++i)
            {
                const float roughness = static_cast<float>(i) / (TableSize - 1);
                directional_albedo<MDF>(roughness, TableSize, SampleCount, p);
                p += TableSize;
            }

            compute_average_albedo_table();
        }

        ~AlbedoTable()
        {
            delete[] m_albedo_table;
        }

        void compute_average_albedo_table()
        {
            float* p = m_albedo_table;
            for (size_t i = 0; i < TableSize; ++i)
            {
                m_avg_table[i] = average_albedo(TableSize, p);
                p += TableSize;
            }
        }

        float get_directional_albedo(const float cos_theta, const float roughness) const
        {
            // Compute the bilinear weights.
            const float x = map_to_index(cos_theta);
            const float y = map_to_index(roughness);

            const size_t i = static_cast<size_t>(floor(x));
            const size_t j = static_cast<size_t>(floor(y));

            const float s = x - i;
            const float t = y - j;

            // Fetch the values.
            const float a = lookup_table(i    , j   );
            const float b = lookup_table(i + 1, j   );
            const float c = lookup_table(i    , j + 1);
            const float d = lookup_table(i + 1, j + 1);

            // Bilinear interpolation.
            return lerp(lerp(a, b, t), lerp(c, d, t), s);
        }

        float get_average_albedo(const float roughness) const
        {
            // Compute the interpolation weight.
            const float x = map_to_index(roughness);

            const size_t i = static_cast<size_t>(floor(x));
            const size_t j = min(i + 1, TableSize - 1);

            const float t = x - i;

            // Fetch the values.
            const float a = m_avg_table[i];
            const float b = m_avg_table[j];

            // Interpolate.
            return lerp(a, b, t);
        }

        void write_table_to_image(const bfs::path& filename)
        {
            Image image(TableSize, TableHeight, TableSize, TableHeight, 3, PixelFormatFloat);

            // Directional albedo.
            const float* p = m_albedo_table;
            for (size_t j = 0; j < TableSize; ++j)
            {
                for (size_t i = 0; i < TableSize; ++i)
                {
                    // Write 1 - E(u) to match the images in [2].
                    image.set_pixel(i, j, Color3f(1.0f - p[i]));
                }

                p += TableSize;
            }

            // Average albedo.
            for (size_t i = 0; i < TableSize; ++i)
                image.set_pixel(i, TableSize, Color3f(p[i]));

            EXRImageFileWriter writer;
            writer.write(filename.string().c_str(), image);
        }

    private:
        const size_t    TableSize;
        const size_t    TableHeight;
        const size_t    SampleCount;

        float*          m_albedo_table;
        float*          m_avg_table;

        float map_to_index(const float x) const
        {
            return saturate(x) * static_cast<float>(TableSize - 1);
        }

        float lookup_table(const size_t i , const size_t j) const
        {
            const size_t ii = min(i, TableSize - 1);
            const size_t jj = min(j, TableSize - 1);
            const float* p = m_albedo_table + (jj * TableSize) + ii;
            return *p;
        }
    };

    struct AlbedoTables
      : public NonCopyable
    {
        AlbedoTable m_ggx;
        AlbedoTable m_beckmann;

        AlbedoTables()
          : m_ggx(GGXMDF())
          , m_beckmann(BeckmannMDF())
        {
        }
    };

    AlbedoTables g_dir_albedo_tables;

    void compute_energy_compensation_term(
        const AlbedoTable&  table,
        const float         roughness,
        const float         cos_in,
        const float         cos_on,
        float&              fms,
        float&              eavg)
    {
        if (cos_in == 0.0f || cos_on == 0.0f || roughness == 0.0f)
        {
            fms = 0.0f;
            eavg = 1.0f;
            return;
        }

        eavg = table.get_average_albedo(roughness);
        if (eavg == 1.0f)
        {
            fms = 0.0f;
            return;
        }

        const float eo = table.get_directional_albedo(abs(cos_on), roughness);
        const float ei = table.get_directional_albedo(abs(cos_in), roughness);
        fms = ((1.0f - eo) * (1.0f - ei)) / (Pi<float>() * (1.0f - eavg));
    }

}

void microfacet_energy_compensation_term(
    const GGXMDF&       mdf,
    const float         roughness,
    const float         cos_in,
    const float         cos_on,
    float&              fms,
    float&              eavg)
{
    compute_energy_compensation_term(
        g_dir_albedo_tables.m_ggx,
        roughness,
        cos_in,
        cos_on,
        fms,
        eavg);
}

void microfacet_energy_compensation_term(
    const BeckmannMDF&  mdf,
    const float         roughness,
    const float         cos_in,
    const float         cos_on,
    float&              fms,
    float&              eavg)
{
    compute_energy_compensation_term(
        g_dir_albedo_tables.m_beckmann,
        roughness,
        cos_in,
        cos_on,
        fms,
        eavg);
}

void write_microfacet_directional_albedo_tables_to_exr(
    const char*         directory)
{
    const bfs::path dir(directory);

    g_dir_albedo_tables.m_ggx.write_table_to_image(
        dir / "ggx_dir_albedo_table.exr");

    g_dir_albedo_tables.m_beckmann.write_table_to_image(
        dir / "beckmann_dir_albedo_table.exr");
}

}   // namespace renderer
