
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "occupancygrid.h"

using namespace foundation;

namespace renderer
{

//
// OccupancyGrid class implementation.
//

OccupancyGrid::OccupancyGrid(
    const VoxelGrid&    voxel_grid,
    const size_t        density_channel_index,
    const float         occupancy_threshold)
  : m_grid(
        voxel_grid.get_xres(),
        voxel_grid.get_yres(),
        voxel_grid.get_zres(),
        1)
{
    initialize(
        voxel_grid,
        density_channel_index,
        occupancy_threshold);
}

void OccupancyGrid::initialize(
    const VoxelGrid&    voxel_grid,
    const size_t        density_channel_index,
    const float         occupancy_threshold)
{
    for (size_t z = 0; z < m_grid.get_zres(); ++z)
    {
        for (size_t y = 0; y < m_grid.get_yres(); ++y)
        {
            for (size_t x = 0; x < m_grid.get_xres(); ++x)
            {
                const float density_sum =
                    get_density_sum(
                        voxel_grid,
                        density_channel_index,
                        x,
                        y,
                        z);

                m_grid.voxel(x, y, z)[0] = density_sum > occupancy_threshold ? 1 : 0;
            }
        }
    }
}

float OccupancyGrid::get_density_sum(
    const VoxelGrid&    voxel_grid,
    const size_t        density_channel_index,
    const size_t        x,
    const size_t        y,
    const size_t        z) const
{
    float density_sum = 0.0f;

    for (int dx = -1; dx <= +1; ++dx)
    {
        for (int dy = -1; dy <= +1; ++dy)
        {
            for (int dz = -1; dz <= +1; ++dz)
            {
                const int ix = static_cast<int>(x) + dx;
                const int iy = static_cast<int>(y) + dy;
                const int iz = static_cast<int>(z) + dz;

                if (ix < 0 ||
                    iy < 0 ||
                    iz < 0 ||
                    ix >= static_cast<int>(voxel_grid.get_xres()) ||
                    iy >= static_cast<int>(voxel_grid.get_yres()) ||
                    iz >= static_cast<int>(voxel_grid.get_zres()))
                    continue;

                const float* voxel = voxel_grid.voxel(ix, iy, iz);
                assert(voxel[density_channel_index] >= 0.0f);

                density_sum += voxel[density_channel_index];
            }
        }
    }

    return density_sum;
}

}   // namespace renderer
