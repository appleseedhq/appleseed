
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "volume.h"

// appleseed.foundation headers.
#include "foundation/platform/types.h"
#include "foundation/utility/cc.h"

// Standard headers.
#include <cassert>
#include <cstdio>
#include <utility>

using namespace foundation;

namespace renderer
{

//
// FluidChannels class implementation.
//

FluidChannels::FluidChannels()
  : m_color_index(NotPresent)
  , m_density_index(NotPresent)
  , m_temperature_index(NotPresent)
  , m_fuel_index(NotPresent)
  , m_falloff_index(NotPresent)
  , m_pressure_index(NotPresent)
  , m_coordinates_index(NotPresent)
  , m_velocity_index(NotPresent)
{
}


//
// Voxel grid I/O.
//

namespace
{
    struct FluidFileHeader
    {
        long            m_id;
        unsigned int    m_xres;
        unsigned int    m_yres;
        unsigned int    m_zres;
        unsigned int    m_has_color;
        unsigned int    m_has_density;
        unsigned int    m_has_temperature;
        unsigned int    m_has_fuel;
        unsigned int    m_has_falloff;
        unsigned int    m_has_pressure;
        unsigned int    m_has_coordinates;
        unsigned int    m_has_velocity;
    };

    size_t read_channels(
        FILE*           file,
        VoxelGrid*      grid,
        const size_t    channel_index,
        const size_t    channel_count)
    {
        assert(file);
        assert(grid);
        assert(channel_count > 0);

        size_t read = 0;

        for (size_t z = 0; z < grid->get_zres(); ++z)
        {
            for (size_t y = 0; y < grid->get_yres(); ++y)
            {
                for (size_t x = 0; x < grid->get_xres(); ++x)
                {
                    float* voxel = grid->voxel(x, y, z);
                    read +=
                        fread(
                            &voxel[channel_index],
                            sizeof(float),
                            channel_count,
                            file);
                }
            }
        }

        return read;
    }
}

std::unique_ptr<VoxelGrid> read_fluid_file(
    const char*         filename,
    FluidChannels&      channels)
{
    assert(filename);

    FILE* file = fopen(filename, "rb");

    if (file == nullptr)
        return std::unique_ptr<VoxelGrid>(nullptr);

    // Read the file header.
    FluidFileHeader header;
    if (fread(&header, sizeof(FluidFileHeader), 1, file) < 1)
    {
        fclose(file);
        return std::unique_ptr<VoxelGrid>(nullptr);
    }

    // Check the validity of the file header.
    if (header.m_id != CC32('F', 'L', 'D', '3'))
    {
        fclose(file);
        return std::unique_ptr<VoxelGrid>(nullptr);
    }

    const size_t voxel_count = header.m_xres * header.m_yres * header.m_zres;

    // Compute the number of channels, and set channel indices.
    size_t channel_count = 0;
    if (header.m_has_color)
    {
        channels.m_color_index = channel_count;
        channel_count += 3;
    }
    if (header.m_has_density)
    {
        channels.m_density_index = channel_count;
        channel_count += 1;
    }
    if (header.m_has_temperature)
    {
        channels.m_temperature_index = channel_count;
        channel_count += 1;
    }
    if (header.m_has_fuel)
    {
        channels.m_fuel_index = channel_count;
        channel_count += 1;
    }
    if (header.m_has_falloff)
    {
        channels.m_falloff_index = channel_count;
        channel_count += 1;
    }
    if (header.m_has_pressure)
    {
        channels.m_pressure_index = channel_count;
        channel_count += 1;
    }
    if (header.m_has_coordinates)
    {
        channels.m_coordinates_index = channel_count;
        channel_count += 3;
    }
    if (header.m_has_velocity)
    {
        channels.m_velocity_index = channel_count;
        channel_count += 3;
    }

    std::unique_ptr<VoxelGrid> grid(
        new VoxelGrid(
            header.m_xres,
            header.m_yres,
            header.m_zres,
            channel_count));

    size_t channel_index = 0;
    size_t read = 0;
    size_t needed = 0;

    // Read fluid color.
    if (header.m_has_color)
    {
        read += read_channels(file, grid.get(), channel_index, 3);
        needed += voxel_count * 3;
        channel_index += 3;
    }

    // Read fluid density.
    if (header.m_has_density)
    {
        read += read_channels(file, grid.get(), channel_index, 1);
        needed += voxel_count;
        channel_index += 1;
    }

    // Read fluid temperature.
    if (header.m_has_temperature)
    {
        read += read_channels(file, grid.get(), channel_index, 1);
        needed += voxel_count;
        channel_index += 1;
    }

    // Read fluid fuel.
    if (header.m_has_fuel)
    {
        read += read_channels(file, grid.get(), channel_index, 1);
        needed += voxel_count;
        channel_index += 1;
    }

    // Read fluid falloff.
    if (header.m_has_falloff)
    {
        read += read_channels(file, grid.get(), channel_index, 1);
        needed += voxel_count;
        channel_index += 1;
    }

    // Read fluid pressure.
    if (header.m_has_pressure)
    {
        read += read_channels(file, grid.get(), channel_index, 1);
        needed += voxel_count;
        channel_index += 1;
    }

    // Read fluid coordinates.
    if (header.m_has_coordinates)
    {
        read += read_channels(file, grid.get(), channel_index, 3);
        needed += voxel_count * 3;
        channel_index += 3;
    }

    // Read fluid velocity.
    if (header.m_has_velocity)
    {
        // X.
        read += read_channels(file, grid.get(), channel_index, 1);
        needed += voxel_count;
        channel_index += 1;

        // Y.
        read += read_channels(file, grid.get(), channel_index, 1);
        needed += voxel_count;
        channel_index += 1;

        // Z.
        read += read_channels(file, grid.get(), channel_index, 1);
        needed += voxel_count;
        channel_index += 1;
    }

    assert(channel_index == channel_count);

    fclose(file);

    return read == needed ? move(grid) : std::unique_ptr<VoxelGrid>(nullptr);
}

void write_voxel_grid(
    const char*         filename,
    const VoxelGrid&    grid)
{
    FILE* file = fopen(filename, "wt");

    if (file == nullptr)
        return;

    const size_t xres = grid.get_xres();
    const size_t yres = grid.get_yres();
    const size_t zres = grid.get_zres();
    const size_t channel_count = grid.get_channel_count();

    for (size_t z = 0; z < zres; ++z)
    {
        fprintf(file, "z " FMT_SIZE_T "\n\n", z);

        for (size_t y = 0; y < yres; ++y)
        {
            for (size_t x = 0; x < xres; ++x)
            {
                if (x > 0)
                    fprintf(file, "  ");

                const float* voxel = grid.voxel(x, y, z);

                for (size_t i = 0; i < channel_count; ++i)
                {
                    if (i > 0)
                        fprintf(file, ",");

                    fprintf(file, "%f", voxel[i]);
                }
            }

            fprintf(file, "\n");
        }

        fprintf(file, "\n");
    }

    fclose(file);
}

}   // namespace renderer
