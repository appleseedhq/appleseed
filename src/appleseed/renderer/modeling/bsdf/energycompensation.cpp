
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "energycompensation.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/scalar.h"

// Boost headers.
#include "boost/filesystem/fstream.hpp"

// Standard headers.
#include <algorithm>
#include <iomanip>

using namespace foundation;

namespace bf = boost::filesystem;

namespace renderer
{

namespace
{
    void write_cpp_array(
        const bf::path&  filename,
        const char*      array_name,
        const size_t     dir_array_size,
        const size_t     avg_array_size,
        const size_t     num_columns,
        const float*     dir_table,
        const float*     avg_table)
    {
        const size_t array_size = dir_array_size + avg_array_size;
        assert(array_size % num_columns == 0);

        bf::ofstream of(filename);
        of << std::setprecision(6) << std::fixed;

        of << "extern const float " << array_name << "[" << array_size << "] = " << std::endl;
        of << "{" << std::endl;
        of << "    // Directional albedo." << std::endl;

        size_t i = 0;
        const float* p = dir_table;

        while (i < dir_array_size)
        {
            of << "    ";

            for (size_t j = 0; j < num_columns; ++j)
            {
                of << *p++ << "f" << ", ";
                ++i;
            }

            of << std::endl;
        }

        of << "    // Average albedo." << std::endl;

        i = 0;
        p = avg_table;
        const size_t last = avg_array_size - 1;

        while (i < avg_array_size)
        {
            of << "    ";

            for (size_t j = 0; j < num_columns; ++j)
            {
                of << *p++ << "f";
                if (i != last) of << ", ";

                ++i;
            }

            of << std::endl;
        }

        of << "};" << std::endl;
    }
}


//
// AlbedoTable2D class implementation.
//

AlbedoTable2D::AlbedoTable2D()
  : TableSize(32)
  , TableHeight(TableSize + 1)
{
    m_data = new float[array_size()];
    m_albedo_table = m_data;
    m_avg_table = m_albedo_table + TableSize * TableSize;
}

AlbedoTable2D::AlbedoTable2D(const float* table)
  : TableSize(32)
  , TableHeight(TableSize + 1)
  , m_data(nullptr)
{
    m_albedo_table = const_cast<float*>(table);
    m_avg_table = m_albedo_table + TableSize * TableSize;
}

AlbedoTable2D::~AlbedoTable2D()
{
    delete[] m_data;
}

size_t AlbedoTable2D::array_size() const
{
    return TableSize * TableHeight;
}

float AlbedoTable2D::get_directional_albedo(const float cos_theta, const float roughness) const
{
    assert(cos_theta >= 0.0f);
    assert(cos_theta <= 1.0f);
    assert(roughness >= 0.0f);
    assert(roughness <= 1.0f);

    // Compute the bilinear weights.
    const float x = cos_theta * (TableSize - 1);
    const float y = roughness * (TableSize - 1);

    size_t i, j;
    const float s = floor_frac(x, i);
    const float t = floor_frac(y, j);

    const size_t i1 = std::min(i + 1, TableSize - 1);
    const size_t j1 = std::min(j + 1, TableSize - 1);

    // Fetch the values.
    const float a = dir_table(i , j );
    const float b = dir_table(i1, j );
    const float c = dir_table(i , j1);
    const float d = dir_table(i1, j1);

    // Bilinear interpolation.
    return lerp(lerp(a, b, s), lerp(c, d, s), t);
}

float AlbedoTable2D::get_average_albedo(const float roughness) const
{
    assert(roughness >= 0.0f);
    assert(roughness <= 1.0f);

    // Compute the interpolation weight.
    const float x = roughness * (TableSize - 1);

    size_t i;
    const float t = floor_frac(x, i);
    const size_t i1 = std::min(i + 1, TableSize - 1);

    // Fetch the values.
    const float a = avg_table(i );
    const float b = avg_table(i1);

    // Interpolate.
    return lerp(a, b, t);
}

float AlbedoTable2D::dir_table(const size_t x , const size_t y) const
{
    assert(x < TableSize);
    assert(y < TableSize);

    return m_albedo_table[y * TableSize + x];
}

float AlbedoTable2D::avg_table(const size_t x) const
{
    assert(x < TableSize);

    return m_avg_table[x];
}

void AlbedoTable2D::write_table_to_image(const bf::path& filename) const
{
    Image image(TableSize, TableHeight, TableSize, TableHeight, 3, PixelFormatFloat);

    // Directional albedo.
    const float* p = m_albedo_table;
    for (size_t j = 0; j < TableSize; ++j)
    {
        for (size_t i = 0; i < TableSize; ++i)
        {
            // Write 1 - E(u) to match the images in
            // "Revisiting Physically Based Shading at Imageworks".
            image.set_pixel(i, j, Color3f(1.0f - p[i]));
        }

        p += TableSize;
    }

    // Average albedo.
    for (size_t i = 0; i < TableSize; ++i)
        image.set_pixel(i, TableSize, Color3f(p[i]));

    const std::string file = filename.string();

    GenericImageFileWriter writer(file.c_str());
    writer.append_image(&image);
    writer.write();
}

void AlbedoTable2D::write_table_to_cpp_array(
    const bf::path&  filename,
    const char*      array_name) const
{
    write_cpp_array(
        filename,
        array_name,
        TableSize * TableSize,
        TableSize,
        4,
        m_albedo_table,
        m_avg_table);
}


//
// AlbedoTable3D class implementation.
//

AlbedoTable3D::AlbedoTable3D(const float min_eta, const float max_eta)
  : TableSize(16)
  , m_min_eta(min_eta)
  , m_max_eta(max_eta)
{
    m_data = new float[array_size()];
    m_albedo_table = m_data;

    init();
}

AlbedoTable3D::AlbedoTable3D(const float* table, const float min_eta, const float max_eta)
  : TableSize(16)
  , m_data(nullptr)
  , m_min_eta(min_eta)
  , m_max_eta(max_eta)
{
    m_albedo_table = const_cast<float*>(table);

    init();
}

AlbedoTable3D::~AlbedoTable3D()
{
    delete[] m_data;
}

void AlbedoTable3D::init()
{
    const size_t avg_table_size = TableSize * TableSize;
    const size_t dir_table_size = TableSize * avg_table_size;
    m_avg_table = m_albedo_table + dir_table_size;
}

size_t AlbedoTable3D::array_size() const
{
    const size_t avg_table_size = TableSize * TableSize;
    const size_t dir_table_size = TableSize * avg_table_size;
    return dir_table_size + avg_table_size;
}

float AlbedoTable3D::get_directional_albedo(const float eta, const float roughness, const float cos_theta) const
{
    assert(eta >= m_min_eta);
    assert(eta <= m_max_eta);
    assert(roughness >= 0.0f);
    assert(roughness <= 1.0f);
    assert(cos_theta >= 0.0f);
    assert(cos_theta <= 1.0f);

    // Compute the trilinear weights.
    const float x = cos_theta * (TableSize - 1);
    const float y = roughness * (TableSize - 1);
    const float z = (TableSize - 1) * saturate((eta - m_min_eta) / (m_max_eta - m_min_eta));

    size_t ix, iy, iz;
    const float s = floor_frac(x, ix);
    const float t = floor_frac(y, iy);
    const float u = floor_frac(z, iz);

    const size_t ix1 = std::min(ix + 1, TableSize - 1);
    const size_t iy1 = std::min(iy + 1, TableSize - 1);
    const size_t iz1 = std::min(iz + 1, TableSize - 1);

    // Fetch the values.
    const float a = dir_table(ix , iy , iz);
    const float b = dir_table(ix1, iy , iz);
    const float c = dir_table(ix , iy1, iz);
    const float d = dir_table(ix1, iy1, iz);

    const float e = dir_table(ix , iy , iz1);
    const float f = dir_table(ix1, iy , iz1);
    const float g = dir_table(ix , iy1, iz1);
    const float h = dir_table(ix1, iy1, iz1);

    // Trilinear interpolation.
    const float q = lerp(lerp(a, b, s), lerp(c, d, s), t);
    const float r = lerp(lerp(e, f, s), lerp(g, h, s), t);
    return lerp(q, r, u);
}

float AlbedoTable3D::get_average_albedo(const float eta, const float roughness) const
{
    assert(eta >= m_min_eta);
    assert(eta <= m_max_eta);
    assert(roughness >= 0.0f);
    assert(roughness <= 1.0f);

    // Compute the bilinear weights.
    const float x = roughness * (TableSize - 1);
    const float y = (TableSize - 1) * saturate((eta - m_min_eta) / (m_max_eta - m_min_eta));

    size_t ix, iy;
    const float s = floor_frac(x, ix);
    const float t = floor_frac(y, iy);

    const size_t ix1 = std::min(ix + 1, TableSize - 1);
    const size_t iy1 = std::min(iy + 1, TableSize - 1);

    // Fetch the values.
    const float a = avg_table(ix , iy );
    const float b = avg_table(ix1, iy );
    const float c = avg_table(ix , iy1);
    const float d = avg_table(ix1, iy1);

    // Bilinear interpolation.
    return lerp(lerp(a, b, s), lerp(c, d, s), t);
}

float AlbedoTable3D::dir_table(const size_t x, const size_t y, const size_t z) const
{
    assert(x < TableSize);
    assert(y < TableSize);
    assert(z < TableSize);

    return m_albedo_table[(z * TableSize * TableSize) + (y * TableSize) + x];
}

float& AlbedoTable3D::dir_table(const size_t x, const size_t y, const size_t z)
{
    assert(x < TableSize);
    assert(y < TableSize);
    assert(z < TableSize);

    return m_albedo_table[(z * TableSize * TableSize) + (y * TableSize) + x];
}

float AlbedoTable3D::avg_table(const size_t x, const size_t y) const
{
    assert(x < TableSize);
    assert(y < TableSize);

    return m_avg_table[(y * TableSize) + x];
}

float& AlbedoTable3D::avg_table(const size_t x, const size_t y)
{
    assert(x < TableSize);
    assert(y < TableSize);

    return m_avg_table[(y * TableSize) + x];
}

void AlbedoTable3D::write_table_to_image(const bf::path& filename) const
{
    Image image(
        TableSize * (TableSize + 1),
        TableSize + 1,
        TableSize,
        TableSize + 1,
        3,
        PixelFormatFloat);

    for (size_t z = 0; z < TableSize; ++z)
    {
        Tile& tile = image.tile(z, 0);

        // Directional albedo.
        for (size_t y = 0; y < TableSize; ++y)
        {
            for (size_t x = 0; x < TableSize; ++x)
            {
                // Write 1 - E(u) to match the images in
                // "Revisiting Physically Based Shading at Imageworks".
                const Color3f c(1.0f - dir_table(x, y, z));
                tile.set_pixel(x, y, c);
            }
        }

        // Write the average albedo to the last scanline
        // to make it easier to compare with the 2d albedo tables.
        for (size_t x = 0; x < TableSize; ++x)
        {
            const Color3f c(avg_table(x, z));
            tile.set_pixel(x, TableSize, c);
        }
    }

    // Average albedo.
    Tile& tile = image.tile(TableSize, 0);
    for (size_t y = 0; y < TableSize; ++y)
    {
        for (size_t x = 0; x < TableSize; ++x)
        {
            const Color3f c(avg_table(x, y));
            tile.set_pixel(x, y, c);
        }
    }

    const std::string file = filename.string();

    GenericImageFileWriter writer(file.c_str());
    writer.append_image(&image);
    writer.write();
}

void AlbedoTable3D::write_table_to_cpp_array(
    const bf::path&  filename,
    const char*      array_name) const
{
    const size_t avg_table_size = TableSize * TableSize;
    const size_t dir_table_size = TableSize * avg_table_size;

    write_cpp_array(
        filename,
        array_name,
        dir_table_size,
        avg_table_size,
        4,
        m_albedo_table,
        m_avg_table);
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

    avg /= table_size;
    return std::min(2.0f * avg, 1.0f);
}

}   // namespace renderer
