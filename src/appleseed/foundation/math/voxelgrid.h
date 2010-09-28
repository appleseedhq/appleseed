
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_MATH_VOXELGRID_H
#define APPLESEED_FOUNDATION_MATH_VOXELGRID_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/typetraits.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstring>
#include <vector>

namespace foundation
{

//
// A regular 3D grid of voxel.
//

template <typename T>
class VoxelGrid3
  : public NonCopyable
{
  public:
    // Types.
    typedef T ValueType;
    typedef Vector<T, 3> Vector3Type;

    // Constructor.
    VoxelGrid3(
        const size_t        xres,
        const size_t        yres,
        const size_t        zres,
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
    // Numerical imprecisions on the input point are handled.
    void nearest_lookup(
        const Vector3Type&  point,
        ValueType           values[]) const;

    // Perform a trilinearly interpolated lookup of the voxel grid.
    // Numerical imprecisions on the input point are handled.
    void trilinear_lookup(
        const Vector3Type&  point,
        ValueType           values[]) const;

    // Perform a trilquadratically interpolated lookup of the voxel grid.
    // Numerical imprecisions on the input point are handled.
    template <size_t MaxChannelCount>
    void triquadratic_lookup(
        const Vector3Type&  point,
        ValueType           values[]) const;

  private:
    const size_t            m_xres;
    const size_t            m_yres;
    const size_t            m_zres;
    const ValueType         m_xbound;
    const ValueType         m_ybound;
    const ValueType         m_zbound;
    const size_t            m_nx;
    const size_t            m_ny;
    const size_t            m_nz;
    const size_t            m_channel_count;
    const size_t            m_line_size;
    const size_t            m_row_size;
    const size_t            m_slice_size;
    std::vector<ValueType>  m_values;
};


//
// VoxelGrid3 class implementation.
//

template <typename T>
VoxelGrid3<T>::VoxelGrid3(
    const size_t        xres,
    const size_t        yres,
    const size_t        zres,
    const size_t        channel_count)
  : m_xres(xres)
  , m_yres(yres)
  , m_zres(zres)
  , m_xbound(static_cast<ValueType>(xres - 2))
  , m_ybound(static_cast<ValueType>(yres - 2))
  , m_zbound(static_cast<ValueType>(zres - 2))
  , m_nx(xres + 1)
  , m_ny(yres + 1)
  , m_nz(zres + 1)
  , m_channel_count(channel_count)
  , m_line_size(m_channel_count * 3)
  , m_row_size(m_channel_count * m_nx)      // number of values in one row of voxels
  , m_slice_size(m_row_size * m_ny)         // number of values in one slice of voxels
{
    assert(m_xres > 1);
    assert(m_yres > 1);
    assert(m_zres > 1);
    assert(m_channel_count > 0);

    m_values.resize(m_nx * m_ny * m_nz * m_channel_count);

    std::memset(
        &m_values[0],
        0,
        m_values.size() * sizeof(ValueType));
}

template <typename T>
FORCE_INLINE size_t VoxelGrid3<T>::get_xres() const
{
    return m_xres;
}

template <typename T>
FORCE_INLINE size_t VoxelGrid3<T>::get_yres() const
{
    return m_yres;
}

template <typename T>
FORCE_INLINE size_t VoxelGrid3<T>::get_zres() const
{
    return m_zres;
}

template <typename T>
FORCE_INLINE size_t VoxelGrid3<T>::get_channel_count() const
{
    return m_channel_count;
}

template <typename T>
FORCE_INLINE T* VoxelGrid3<T>::voxel(
    const size_t        x,
    const size_t        y,
    const size_t        z)
{
    assert(x < m_nx);
    assert(y < m_ny);
    assert(z < m_nz);
    return &m_values[((z * m_ny + y) * m_nx + x) * m_channel_count];
}

template <typename T>
FORCE_INLINE const T* VoxelGrid3<T>::voxel(
    const size_t        x,
    const size_t        y,
    const size_t        z) const
{
    assert(x < m_nx);
    assert(y < m_ny);
    assert(z < m_nz);
    return &m_values[((z * m_ny + y) * m_nx + x) * m_channel_count];
}

template <typename T>
void VoxelGrid3<T>::nearest_lookup(
    const Vector3Type&  point,
    ValueType           values[]) const
{
    // Clamp the lookup point coordinates.
    const ValueType x = clamp(point.x, ValueType(0.0), m_xbound);
    const ValueType y = clamp(point.y, ValueType(0.0), m_ybound);
    const ValueType z = clamp(point.z, ValueType(0.0), m_zbound);

    // Compute the coordinates of the voxel containing the lookup point.
    const size_t ix = truncate<size_t>(x);
    const size_t iy = truncate<size_t>(y);
    const size_t iz = truncate<size_t>(z);

    // Return the values of that voxel.
    const ValueType* source = voxel(ix, iy, iz);
    for (size_t i = 0; i < m_channel_count; ++i)
        values[i] = source[i];
}

template <typename T>
void VoxelGrid3<T>::trilinear_lookup(
    const Vector3Type&      point,
    ValueType* RESTRICT     values) const
{
    // Clamp the lookup point coordinates.
    const ValueType x = clamp(point.x, ValueType(0.0), m_xbound);
    const ValueType y = clamp(point.y, ValueType(0.0), m_ybound);
    const ValueType z = clamp(point.z, ValueType(0.0), m_zbound);

    // Compute the coordinates of the voxel containing the lookup point.
    const size_t ix = truncate<size_t>(x);
    const size_t iy = truncate<size_t>(y);
    const size_t iz = truncate<size_t>(z);

    // Compute the interpolation weights.
    const ValueType x1 = x - ix;
    const ValueType y1 = y - iy;
    const ValueType z1 = z - iz;
    const ValueType x0 = ValueType(1.0) - x1;
    const ValueType y0 = ValueType(1.0) - y1;
    const ValueType z0 = ValueType(1.0) - z1;

    // Corners 000 and 100.
    const ValueType* RESTRICT source000 = voxel(ix + 0, iy + 0, iz + 0);
    const ValueType* RESTRICT source100 = source000 + m_channel_count;
    const ValueType y0z0 = y0 * z0;
    const ValueType weight000 = x0 * y0z0;
    const ValueType weight100 = x1 * y0z0;

    // Corner 010 and 110.
    const ValueType* RESTRICT source010 = voxel(ix + 0, iy + 1, iz + 0);
    const ValueType* RESTRICT source110 = source010 + m_channel_count;
    const ValueType y1z0 = y1 * z0;
    const ValueType weight010 = x0 * y1z0;
    const ValueType weight110 = x1 * y1z0;

    // Corner 001 and 101.
    const ValueType* RESTRICT source001 = voxel(ix + 0, iy + 0, iz + 1);
    const ValueType* RESTRICT source101 = source001 + m_channel_count;
    const ValueType y0z1 = y0 * z1;
    const ValueType weight001 = x0 * y0z1;
    const ValueType weight101 = x1 * y0z1;

    // Corner 011 and 111.
    const ValueType* RESTRICT source011 = voxel(ix + 0, iy + 1, iz + 1);
    const ValueType* RESTRICT source111 = source011 + m_channel_count;
    const ValueType y1z1 = y1 * z1;
    const ValueType weight011 = x0 * y1z1;
    const ValueType weight111 = x1 * y1z1;

    ValueType* RESTRICT values_ptr = values;

    for (size_t i = 0; i < m_channel_count; ++i)
    {
       *values_ptr++ =
           *source000++ * weight000 +
           *source100++ * weight100 +
           *source010++ * weight010 +
           *source110++ * weight110 +
           *source001++ * weight001 +
           *source101++ * weight101 +
           *source011++ * weight011 +
           *source111++ * weight111;
    }
}

namespace voxelgrid_impl
{

    //
    // Quadratic interpolation polynomial.
    //
    // The parameter t must be in [0, 1].
    // v0 is the value at t = -0.5.
    // v1 is the value at t =  0.5.
    // v2 is the value at t =  1.5.
    //
    // The polynomial was derived as follow.  We first start with the general form
    // of a degree 2 polynomial:
    //
    //   P(t) = a.t^2 + b.t + c      P'(t) = 2.a.t + b
    //
    // Then we compute a, b and c so that the following conditions are met:
    //
    //   P(0) = (v0 + v1) / 2        C0 continuity at t = 0
    //   P(1) = (v1 + v2) / 2        C0 continuity at t = 1
    //   P'(0) = v1 - v0             C1 continuity at t = 0
    //   P'(1) = v2 - v1             C1 continuity at t = 1
    //
    // One of the condition is redundant, as only three are necessary to completely
    // define the polynomial.
    //
    // Given the above conditions, we find the following coefficients:
    //
    //   a = (v0 - 2.v1 + v2) / 2
    //   b = v1 - v0
    //   c = (v0 + v1) / 2
    //
    // Finally, we define
    //
    //   Q(t) = 2.P(t)
    //
    // The function quadratic() implements Q(T),  avoiding two multiplications by 1/2,
    // meaning that the result returned by quadratic() will eventually have to be
    // multiplied again by 2 in order to get the correct answer.
    //

    template <typename T>
    FORCE_INLINE T quadratic(const T t, const T v0, const T v1, const T v2)
    {
        const T w = v0 - v1;
        return ((w - v1 + v2) * t - w - w) * t + v0 + v1;
    }

}   // namespace voxelgrid_impl

template <typename T>
template <size_t MaxChannelCount>
void VoxelGrid3<T>::triquadratic_lookup(
    const Vector3Type&  point,
    ValueType           values[]) const
{
    //
    // Reference: http://citeseer.ist.psu.edu/barthe02triquadratic.html
    //

    using namespace voxelgrid_impl;

    const ValueType x = point.x - ValueType(0.5);
    const ValueType y = point.y - ValueType(0.5);
    const ValueType z = point.z - ValueType(0.5);

    const size_t ix = truncate<size_t>(x);
    const size_t iy = truncate<size_t>(y);
    const size_t iz = truncate<size_t>(z);

    const ValueType tx = x - ix;
    const ValueType ty = y - iy;
    const ValueType tz = z - iz;

    ValueType v0[MaxChannelCount * 3];
    ValueType v1[MaxChannelCount * 3];
    ValueType v2[MaxChannelCount * 3];
    ValueType qz0[MaxChannelCount];
    ValueType qz1[MaxChannelCount];
    ValueType qz2[MaxChannelCount];

    typedef typename TypeConv<ValueType>::UInt UInt;

    const UInt* ptr0 = reinterpret_cast<const UInt*>(voxel(ix, iy, iz));
    const UInt* ptr1 = ptr0 + m_slice_size;
    const UInt* ptr2 = ptr1 + m_slice_size;

    for (size_t i = 0; i < m_line_size; ++i) ((UInt*)v0)[i] = ptr0[i]; ptr0 += m_row_size;
    for (size_t i = 0; i < m_line_size; ++i) ((UInt*)v1)[i] = ptr0[i]; ptr0 += m_row_size;
    for (size_t i = 0; i < m_line_size; ++i) ((UInt*)v2)[i] = ptr0[i];

    for (size_t i = 0; i < m_channel_count; ++i)
    {
        qz0[i] =
            quadratic(
                ty,
                quadratic(tx, v0[i], v0[i + m_channel_count], v0[i + 2 * m_channel_count]),
                quadratic(tx, v1[i], v1[i + m_channel_count], v1[i + 2 * m_channel_count]),
                quadratic(tx, v2[i], v2[i + m_channel_count], v2[i + 2 * m_channel_count]));
    }

    for (size_t i = 0; i < m_line_size; ++i) ((UInt*)v0)[i] = ptr1[i]; ptr1 += m_row_size;
    for (size_t i = 0; i < m_line_size; ++i) ((UInt*)v1)[i] = ptr1[i]; ptr1 += m_row_size;
    for (size_t i = 0; i < m_line_size; ++i) ((UInt*)v2)[i] = ptr1[i];

    for (size_t i = 0; i < m_channel_count; ++i)
    {
        qz1[i] =
            quadratic(
                ty,
                quadratic(tx, v0[i], v0[i + m_channel_count], v0[i + 2 * m_channel_count]),
                quadratic(tx, v1[i], v1[i + m_channel_count], v1[i + 2 * m_channel_count]),
                quadratic(tx, v2[i], v2[i + m_channel_count], v2[i + 2 * m_channel_count]));
    }

    for (size_t i = 0; i < m_line_size; ++i) ((UInt*)v0)[i] = ptr2[i]; ptr2 += m_row_size;
    for (size_t i = 0; i < m_line_size; ++i) ((UInt*)v1)[i] = ptr2[i]; ptr2 += m_row_size;
    for (size_t i = 0; i < m_line_size; ++i) ((UInt*)v2)[i] = ptr2[i];

    for (size_t i = 0; i < m_channel_count; ++i)
    {
        qz2[i] =
            quadratic(
                ty,
                quadratic(tx, v0[i], v0[i + m_channel_count], v0[i + 2 * m_channel_count]),
                quadratic(tx, v1[i], v1[i + m_channel_count], v1[i + 2 * m_channel_count]),
                quadratic(tx, v2[i], v2[i + m_channel_count], v2[i + 2 * m_channel_count]));
    }

    for (size_t i = 0; i < m_channel_count; ++i)
    {
        // Multiply by 0.125 (divide by 8) to compensate
        // for the missing division by 2 in quadratic().
        values[i] = quadratic(tz, qz0[i], qz1[i], qz2[i]);
        values[i] *= ValueType(0.125);
    }
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_VOXELGRID_H
