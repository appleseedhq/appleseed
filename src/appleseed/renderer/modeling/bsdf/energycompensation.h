
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

#pragma once

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Boost headers.
#include "boost/filesystem/path.hpp"


namespace renderer
{

//
// Base class for 2D albedo tables.
//

class AlbedoTable2D
  : public foundation::NonCopyable
{
  public:
    // Return the directional albedo.
    float get_directional_albedo(const float cos_theta, const float roughness) const;

    // Return the average albedo.
    float get_average_albedo(const float roughness) const;

    // Write the table to an OpenEXR image.
    void write_table_to_image(const boost::filesystem::path& filename) const;

    // Write the table to a cpp array.
    void write_table_to_cpp_array(
        const boost::filesystem::path&  filename,
        const char*                     array_name) const;

  protected:
    AlbedoTable2D();

    explicit AlbedoTable2D(const float* table);

    ~AlbedoTable2D();

    size_t array_size() const;

    float dir_table(const size_t x , const size_t y) const;
    float avg_table(const size_t x) const;

    const size_t    TableSize;
    const size_t    TableHeight;

    float*          m_albedo_table;
    float*          m_avg_table;
    float*          m_data;
};


//
// Base class for 3D albedo tables.
//

class AlbedoTable3D
  : public foundation::NonCopyable
{
  public:
    // Return the directional albedo.
    float get_directional_albedo(
        const float eta,
        const float roughness,
        const float cos_theta) const;

    // Return the average albedo.
    float get_average_albedo(const float eta, const float roughness) const;

    // Write the table to an OpenEXR image.
    void write_table_to_image(const boost::filesystem::path& filename) const;

    // Write the table to a cpp array.
    void write_table_to_cpp_array(
        const boost::filesystem::path& filename,
        const char*                    array_name) const;

  protected:
    AlbedoTable3D(const float min_eta, const float max_eta);

    AlbedoTable3D(const float* table, const float min_eta, const float max_eta);

    ~AlbedoTable3D();

    void init();

    size_t array_size() const;

    float dir_table(const size_t x, const size_t y, const size_t z) const;
    float& dir_table(const size_t x, const size_t y, const size_t z);

    float avg_table(const size_t x, const size_t y) const;
    float& avg_table(const size_t x, const size_t y);

    const size_t    TableSize;

    float*          m_albedo_table;
    float*          m_avg_table;
    float*          m_data;

    const float     m_min_eta;
    const float     m_max_eta;
};

float average_albedo(
    const size_t    table_size,
    const float*    directional_albedo);

}   // namespace renderer
