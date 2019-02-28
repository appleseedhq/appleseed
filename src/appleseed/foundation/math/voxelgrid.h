
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
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

namespace foundation
{

//
// A regular 3D grid of voxel.
//

template <typename ValueType, typename CoordType>
class VoxelGrid3
  : public NonCopyable
{
  public:
    // Types.
    typedef Vector<CoordType, 3> PointType;

    // Constructor.
    VoxelGrid3(
        const size_t        nx,
        const size_t        ny,
        const size_t        nz,
        const size_t        channel_count);

    // Get the grid properties.
    size_t get_xres() const;
    size_t get_yres() const;
    size_t get_zres() const;
    size_t get_channel_count() const;

    // Direct access to a given voxel.
    ValueType* voxel(
        const size_t        x,
        const size_t        y,
        const size_t        z);
    const ValueType* voxel(
        const size_t        x,
        const size_t        y,
        const size_t        z) const;

    // Perform an unfiltered lookup of the voxel grid.
    // 'point' must be expressed in the unit cube [0,1]^3.
    void nearest_lookup(
        const PointType&    point,
        ValueType*          values) const;

    // Perform a trilinearly interpolated lookup of the voxel grid.
    // 'point' must be expressed in the unit cube [0,1]^3.
    void linear_lookup(
        const PointType&    point,
        ValueType*          values) const;

    // Perform a triquadratically interpolated lookup of the voxel grid.
    // 'point' must be expressed in the unit cube [0,1]^3.
    void quadratic_lookup(
        const PointType&    point,
        ValueType*          values) const;

  private:
    const size_t            m_nx;
    const size_t            m_ny;
    const size_t            m_nz;
    const CoordType         m_scalar_nx;
    const CoordType         m_scalar_ny;
    const CoordType         m_scalar_nz;
    const CoordType         m_max_x;
    const CoordType         m_max_y;
    const CoordType         m_max_z;
    const size_t            m_channel_count;
    const size_t            m_row_size;
    const size_t            m_slice_size;
    std::vector<ValueType>  m_values;
};


//
// VoxelGrid3 class implementation.
//

template <typename ValueType, typename CoordType>
VoxelGrid3<ValueType, CoordType>::VoxelGrid3(
    const size_t            nx,
    const size_t            ny,
    const size_t            nz,
    const size_t            channel_count)
  : m_nx(nx)
  , m_ny(ny)
  , m_nz(nz)
  , m_scalar_nx(static_cast<CoordType>(nx))
  , m_scalar_ny(static_cast<CoordType>(ny))
  , m_scalar_nz(static_cast<CoordType>(nz))
  , m_max_x(static_cast<CoordType>(nx - 1))
  , m_max_y(static_cast<CoordType>(ny - 1))
  , m_max_z(static_cast<CoordType>(nz - 1))
  , m_channel_count(channel_count)
  , m_row_size(channel_count * nx)          // number of values in one row of voxels
  , m_slice_size(channel_count * nx * ny)   // number of values in one slice of voxels
  , m_values(nx * ny * nz * channel_count, ValueType(0.0))
{
    assert(m_nx > 0);
    assert(m_ny > 0);
    assert(m_nz > 0);
    assert(m_channel_count > 0);
}

template <typename ValueType, typename CoordType>
APPLESEED_FORCE_INLINE size_t VoxelGrid3<ValueType, CoordType>::get_xres() const
{
    return m_nx;
}

template <typename ValueType, typename CoordType>
APPLESEED_FORCE_INLINE size_t VoxelGrid3<ValueType, CoordType>::get_yres() const
{
    return m_ny;
}

template <typename ValueType, typename CoordType>
APPLESEED_FORCE_INLINE size_t VoxelGrid3<ValueType, CoordType>::get_zres() const
{
    return m_nz;
}

template <typename ValueType, typename CoordType>
APPLESEED_FORCE_INLINE size_t VoxelGrid3<ValueType, CoordType>::get_channel_count() const
{
    return m_channel_count;
}

template <typename ValueType, typename CoordType>
APPLESEED_FORCE_INLINE ValueType* VoxelGrid3<ValueType, CoordType>::voxel(
    const size_t            x,
    const size_t            y,
    const size_t            z)
{
    assert(x < m_nx);
    assert(y < m_ny);
    assert(z < m_nz);
    return &m_values[((z * m_ny + y) * m_nx + x) * m_channel_count];
}

template <typename ValueType, typename CoordType>
APPLESEED_FORCE_INLINE const ValueType* VoxelGrid3<ValueType, CoordType>::voxel(
    const size_t            x,
    const size_t            y,
    const size_t            z) const
{
    assert(x < m_nx);
    assert(y < m_ny);
    assert(z < m_nz);
    return &m_values[((z * m_ny + y) * m_nx + x) * m_channel_count];
}

template <typename ValueType, typename CoordType>
void VoxelGrid3<ValueType, CoordType>::nearest_lookup(
    const PointType&                point,
    ValueType* APPLESEED_RESTRICT   values) const
{
    // Compute the coordinates of the voxel containing the lookup point.
    const CoordType x = clamp(point.x * m_scalar_nx, CoordType(0.0), m_max_x);
    const CoordType y = clamp(point.y * m_scalar_ny, CoordType(0.0), m_max_y);
    const CoordType z = clamp(point.z * m_scalar_nz, CoordType(0.0), m_max_z);
    const size_t ix = truncate<size_t>(x);
    const size_t iy = truncate<size_t>(y);
    const size_t iz = truncate<size_t>(z);

    // Return the values of that voxel.
    const ValueType* APPLESEED_RESTRICT source = voxel(ix, iy, iz);
    for (size_t i = 0; i < m_channel_count; ++i)
        *values++ = *source++;
}

template <typename ValueType, typename CoordType>
void VoxelGrid3<ValueType, CoordType>::linear_lookup(
    const PointType&                point,
    ValueType* APPLESEED_RESTRICT   values) const
{
    // Compute the coordinates of the voxel containing the lookup point.
    const CoordType x = saturate(point.x) * m_max_x;
    const CoordType y = saturate(point.y) * m_max_y;
    const CoordType z = saturate(point.z) * m_max_z;
    const size_t ix = truncate<size_t>(x);
    const size_t iy = truncate<size_t>(y);
    const size_t iz = truncate<size_t>(z);

    // Compute interpolation weights.
    const ValueType x1 = static_cast<ValueType>(x - ix);
    const ValueType y1 = static_cast<ValueType>(y - iy);
    const ValueType z1 = static_cast<ValueType>(z - iz);
    const ValueType x0 = ValueType(1.0) - x1;
    const ValueType y0 = ValueType(1.0) - y1;
    const ValueType z0 = ValueType(1.0) - z1;
    const ValueType y0z0 = y0 * z0;
    const ValueType y1z0 = y1 * z0;
    const ValueType y0z1 = y0 * z1;
    const ValueType y1z1 = y1 * z1;
    const ValueType w000 = x0 * y0z0;
    const ValueType w100 = x1 * y0z0;
    const ValueType w010 = x0 * y1z0;
    const ValueType w110 = x1 * y1z0;
    const ValueType w001 = x0 * y0z1;
    const ValueType w101 = x1 * y0z1;
    const ValueType w011 = x0 * y1z1;
    const ValueType w111 = x1 * y1z1;

    // Compute source pointers.
    const size_t dx = ix == m_nx - 1 ? 0 : m_channel_count;
    const size_t dy = iy == m_ny - 1 ? 0 : m_row_size;
    const size_t dz = iz == m_nz - 1 ? 0 : m_slice_size;
    const ValueType* APPLESEED_RESTRICT src = voxel(ix, iy, iz);
    const ValueType* APPLESEED_RESTRICT src000 = src;
    const ValueType* APPLESEED_RESTRICT src100 = src + dx;
    const ValueType* APPLESEED_RESTRICT src010 = src + dy;
    const ValueType* APPLESEED_RESTRICT src001 = src + dz;
    const ValueType* APPLESEED_RESTRICT src110 = src100 + dy;
    const ValueType* APPLESEED_RESTRICT src101 = src100 + dz;
    const ValueType* APPLESEED_RESTRICT src011 = src010 + dz;
    const ValueType* APPLESEED_RESTRICT src111 = src110 + dz;

    // Blend.
    for (size_t i = 0; i < m_channel_count; ++i)
    {
       *values++ =
           *src000++ * w000 +
           *src100++ * w100 +
           *src010++ * w010 +
           *src110++ * w110 +
           *src001++ * w001 +
           *src101++ * w101 +
           *src011++ * w011 +
           *src111++ * w111;
    }
}

template <typename ValueType, typename CoordType>
void VoxelGrid3<ValueType, CoordType>::quadratic_lookup(
    const PointType&                point,
    ValueType* APPLESEED_RESTRICT   values) const
{
    //
    // The quadratic interpolation polynomial used here is defined as follow:
    //
    // We start with the general form of a degree 2 polynomial:
    //
    //   P(t) = a.t^2 + b.t + c         P'(t) = 2.a.t + b
    //
    // The parameter t extends from t = -1/2 (halfway between v0 and v1)
    // to t = +1/2 (halfway between v1 and v2). To simplify the expression
    // of the polynomial, we let s = t + 1/2 and define the polynomial over s.
    //
    // We compute a, b and c so that the following conditions are met:
    //
    //   C0 continuity at s = 0.0:      P (0.0) = (v0 + v1) / 2
    //   C0 continuity at s = 1.0:      P (1.0) = (v1 + v2) / 2
    //   C1 continuity at s = 0.0:      P'(0.0) = v1 - v0
    //   C1 continuity at s = 1.0:      P'(1.0) = v2 - v1
    //
    // One of the condition is redundant, as only three conditions are
    // required to completely define the polynomial.
    //
    // Given the above conditions, we find the following coefficients for P(s):
    //
    //   a = 1/2 * (v0 - 2.v1 + v2)
    //   b = v1 - v0
    //   c = 1/2 * (v0 + v1)
    //
    // And the weights for v0, v1 and v2:
    //
    //   w0 = 1/2 * (t^2 - 2.t + 1)
    //   w1 = -t^2 + t + 1/2
    //   w2 = 1/2 * t^2
    //

    // Compute the coordinates of the voxel containing the lookup point.
    const CoordType x = saturate(point.x) * m_max_x;
    const CoordType y = saturate(point.y) * m_max_y;
    const CoordType z = saturate(point.z) * m_max_z;
    const size_t ix = truncate<size_t>(x + CoordType(0.5));
    const size_t iy = truncate<size_t>(y + CoordType(0.5));
    const size_t iz = truncate<size_t>(z + CoordType(0.5));

    // Compute interpolation weights.
    const ValueType tx = static_cast<ValueType>(x - ix) + ValueType(0.5);
    const ValueType ty = static_cast<ValueType>(y - iy) + ValueType(0.5);
    const ValueType tz = static_cast<ValueType>(z - iz) + ValueType(0.5);
    const ValueType tx2 = tx * tx;
    const ValueType ty2 = ty * ty;
    const ValueType tz2 = tz * tz;
    const ValueType wx2 = ValueType(0.5) * tx2;
    const ValueType wx1 = tx - tx2 + ValueType(0.5);
    const ValueType wx0 = wx2 - tx + ValueType(0.5);
    const ValueType wy2 = ValueType(0.5) * ty2;
    const ValueType wy1 = ty - ty2 + ValueType(0.5);
    const ValueType wy0 = wy2 - ty + ValueType(0.5);
    const ValueType wz2 = ValueType(0.5) * tz2;
    const ValueType wz1 = tz - tz2 + ValueType(0.5);
    const ValueType wz0 = wz2 - tz + ValueType(0.5);

    // Compute source pointers.
    const size_t dxn = ix == 0 ? 0 : m_channel_count;
    const size_t dyn = iy == 0 ? 0 : m_row_size;
    const size_t dzn = iz == 0 ? 0 : m_slice_size;
    const size_t dxp = ix == m_nx - 1 ? 0 : m_channel_count;
    const size_t dyp = iy == m_ny - 1 ? 0 : m_row_size;
    const size_t dzp = iz == m_nz - 1 ? 0 : m_slice_size;
    const ValueType* APPLESEED_RESTRICT src = voxel(ix, iy, iz);
    const ValueType* APPLESEED_RESTRICT src111 = src;
    const ValueType* APPLESEED_RESTRICT src011 = src111 - dxn;
    const ValueType* APPLESEED_RESTRICT src211 = src111 + dxp;
    const ValueType* APPLESEED_RESTRICT src101 = src111 - dyn;
    const ValueType* APPLESEED_RESTRICT src001 = src011 - dyn;
    const ValueType* APPLESEED_RESTRICT src201 = src211 - dyn;
    const ValueType* APPLESEED_RESTRICT src121 = src111 + dyp;
    const ValueType* APPLESEED_RESTRICT src021 = src011 + dyp;
    const ValueType* APPLESEED_RESTRICT src221 = src211 + dyp;
    const ValueType* APPLESEED_RESTRICT src110 = src111 - dzn;
    const ValueType* APPLESEED_RESTRICT src010 = src011 - dzn;
    const ValueType* APPLESEED_RESTRICT src210 = src211 - dzn;
    const ValueType* APPLESEED_RESTRICT src100 = src101 - dzn;
    const ValueType* APPLESEED_RESTRICT src000 = src001 - dzn;
    const ValueType* APPLESEED_RESTRICT src200 = src201 - dzn;
    const ValueType* APPLESEED_RESTRICT src120 = src121 - dzn;
    const ValueType* APPLESEED_RESTRICT src020 = src021 - dzn;
    const ValueType* APPLESEED_RESTRICT src220 = src221 - dzn;
    const ValueType* APPLESEED_RESTRICT src112 = src111 + dzp;
    const ValueType* APPLESEED_RESTRICT src012 = src011 + dzp;
    const ValueType* APPLESEED_RESTRICT src212 = src211 + dzp;
    const ValueType* APPLESEED_RESTRICT src102 = src101 + dzp;
    const ValueType* APPLESEED_RESTRICT src002 = src001 + dzp;
    const ValueType* APPLESEED_RESTRICT src202 = src201 + dzp;
    const ValueType* APPLESEED_RESTRICT src122 = src121 + dzp;
    const ValueType* APPLESEED_RESTRICT src022 = src021 + dzp;
    const ValueType* APPLESEED_RESTRICT src222 = src221 + dzp;

    // Blend.
    for (size_t i = 0; i < m_channel_count; ++i)
    {
        const ValueType p00 = *src000++ * wx0 + *src100++ * wx1 + *src200++ * wx2;
        const ValueType p01 = *src001++ * wx0 + *src101++ * wx1 + *src201++ * wx2;
        const ValueType p02 = *src002++ * wx0 + *src102++ * wx1 + *src202++ * wx2;

        const ValueType p10 = *src010++ * wx0 + *src110++ * wx1 + *src210++ * wx2;
        const ValueType p11 = *src011++ * wx0 + *src111++ * wx1 + *src211++ * wx2;
        const ValueType p12 = *src012++ * wx0 + *src112++ * wx1 + *src212++ * wx2;

        const ValueType p20 = *src020++ * wx0 + *src120++ * wx1 + *src220++ * wx2;
        const ValueType p21 = *src021++ * wx0 + *src121++ * wx1 + *src221++ * wx2;
        const ValueType p22 = *src022++ * wx0 + *src122++ * wx1 + *src222++ * wx2;

        const ValueType q0 = p00 * wy0 + p10 * wy1 + p20 * wy2;
        const ValueType q1 = p01 * wy0 + p11 * wy1 + p21 * wy2;
        const ValueType q2 = p02 * wy0 + p12 * wy1 + p22 * wy2;

        *values++ = q0 * wz0 + q1 * wz1 + q2 * wz2;
    }
}

}   // namespace foundation
