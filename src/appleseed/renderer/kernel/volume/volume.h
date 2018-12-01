
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

#pragma once

// appleseed.foundation headers.
#include "foundation/math/voxelgrid.h"

// Standard headers.
#include <cstddef>
#include <memory>

namespace renderer
{

//
// The voxel grid used for rendering.
//

typedef foundation::VoxelGrid3<float, double> VoxelGrid;


//
// A structure to keep track of the channels in a voxel grid.
//

struct FluidChannels
{
    static const size_t NotPresent = ~size_t(0);

    size_t  m_color_index;
    size_t  m_density_index;
    size_t  m_temperature_index;
    size_t  m_fuel_index;
    size_t  m_falloff_index;
    size_t  m_pressure_index;
    size_t  m_coordinates_index;
    size_t  m_velocity_index;

    // Constructor, initializes all channel indices to NotPresent.
    FluidChannels();
};


//
// Voxel grid I/O.
//

// Read a fluid file created by 3Delight for Maya.
std::unique_ptr<VoxelGrid> read_fluid_file(
    const char*         filename,
    FluidChannels&      channels);

// Write a voxel grid to disk in a human-readable format.
void write_voxel_grid(
    const char*         filename,
    const VoxelGrid&    grid);

}   // namespace renderer
