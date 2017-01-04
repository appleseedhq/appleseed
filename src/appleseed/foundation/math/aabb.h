
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_AABB_H
#define APPLESEED_FOUNDATION_MATH_AABB_H

// appleseed.foundation headers.
#include "foundation/math/minmax.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Imath headers.
#ifdef APPLESEED_ENABLE_IMATH_INTEROP
#include "foundation/platform/exrheaderguards.h"
BEGIN_EXR_INCLUDES
#include "OpenEXR/ImathBox.h"
END_EXR_INCLUDES
#endif

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <limits>

namespace foundation
{

//
// N-dimensional axis-aligned bounding box [min, max] class and operations.
// The boundary of the bounding box is considered to belong to the bounding box.
//

template <typename T, size_t N>
class AABB
{
  public:
    // Value, vector and AABB types.
    typedef T ValueType;
    typedef Vector<T, N> VectorType;
    typedef AABB<T, N> AABBType;

    // Dimension.
    static const size_t Dimension = N;

    // Bounds.
    VectorType min, max;

    // Constructors.
    AABB();                             // leave all components uninitialized
    AABB(
        const VectorType& min,          // lower bound
        const VectorType& max);         // upper bound

    // Construct a bounding box from another bounding box of a different type.
    template <typename U>
    AABB(const AABB<U, N>& rhs);

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

    // Implicit construction from an Imath::Box.
    AABB(const Imath::Box<typename ImathVecEquivalent<T, N>::Type>& rhs);

    // Reinterpret this bounding box as an Imath::Box.
    operator Imath::Box<typename ImathVecEquivalent<T, N>::Type>&();
    operator const Imath::Box<typename ImathVecEquivalent<T, N>::Type>&() const;

#endif

    // Return an invalidated bounding box.
    static AABBType invalid();

    // Compute the intersection between two bounding boxes.
    static AABBType intersect(const AABBType& a, const AABBType& b);

    // Return true if two bounding boxes overlap.
    static bool overlap(const AABBType& a, const AABBType& b);

    // Return the amount of overlapping between two bounding boxes.
    // Returns 0.0 if the bounding boxes are disjoint, 1.0 if one
    // is contained in the other.
    static ValueType overlap_ratio(const AABBType& a, const AABBType& b);

    // Return the ratio of the extent of a to the extent of b.
    static ValueType extent_ratio(const AABBType& a, const AABBType& b);

    // Unchecked array subscripting. [0] is min, [1] is max.
    VectorType& operator[](const size_t i);
    const VectorType& operator[](const size_t i) const;

    // Invalidate the bounding box (give it a negative volume).
    void invalidate();

    // Insert a point or another bounding box into the bounding box.
    void insert(const VectorType& v);
    void insert(const AABBType& b);

    // Grow the bounding box by a fixed amount in every dimension.
    void grow(const VectorType& v);

    // Robustly grow the bounding box by a given epsilon factor.
    void robust_grow(const ValueType eps);

    // Translate the bounding box.
    void translate(const VectorType& v);

    // Return true if the extent of the bounding box is positive or
    // null along all dimensions.
    bool is_valid() const;

    // Return the rank of the bounding box (the number of dimensions
    // along which the bounding box has a strictly positive extent).
    size_t rank() const;

    // Compute the center of the bounding box.
    VectorType center() const;
    ValueType center(const size_t dim) const;

    // Compute the extent of the bounding box.
    VectorType extent() const;
    ValueType extent(const size_t dim) const;

    // Return the diameter of the bounding box.
    ValueType diameter() const;

    // Return the radius of the bounding box.
    ValueType radius() const;

    // Return the volume of the bounding box.
    T volume() const;

    // Compute the 2^N corner points of the bounding box.
    size_t get_corner_count() const;
    VectorType compute_corner(const size_t i) const;

    // Return true if the bounding box contains a given point.
    bool contains(const VectorType& v) const;
};

// Exact inequality and equality tests.
template <typename T, size_t N> bool operator!=(const AABB<T, N>& lhs, const AABB<T, N>& rhs);
template <typename T, size_t N> bool operator==(const AABB<T, N>& lhs, const AABB<T, N>& rhs);

// Approximate equality tests.
template <typename T, size_t N> bool feq(const AABB<T, N>& lhs, const AABB<T, N>& rhs);
template <typename T, size_t N> bool feq(const AABB<T, N>& lhs, const AABB<T, N>& rhs, const T eps);

// Bounding box arithmetic.
template <typename T, size_t N> AABB<T, N>  operator+ (const AABB<T, N>& lhs, const AABB<T, N>& rhs);
template <typename T, size_t N> AABB<T, N>  operator* (const AABB<T, N>& lhs, const T rhs);
template <typename T, size_t N> AABB<T, N>  operator* (const T lhs, const AABB<T, N>& rhs);
template <typename T, size_t N> AABB<T, N>& operator+=(AABB<T, N>& lhs, const AABB<T, N>& rhs);
template <typename T, size_t N> AABB<T, N>& operator*=(AABB<T, N>& lhs, const T rhs);

// Compute the surface area of a 3D bounding box.
template <typename T> T half_surface_area(const AABB<T, 3>& bbox);
template <typename T> T surface_area(const AABB<T, 3>& bbox);


//
// Full specializations for 1D, 2D, 3D and 4D vectors of type int, float and double.
//

typedef AABB<int,    1> AABB1i;
typedef AABB<size_t, 1> AABB1u;
typedef AABB<float,  1> AABB1f;
typedef AABB<double, 1> AABB1d;

typedef AABB<int,    2> AABB2i;
typedef AABB<size_t, 2> AABB2u;
typedef AABB<float,  2> AABB2f;
typedef AABB<double, 2> AABB2d;

typedef AABB<int,    3> AABB3i;
typedef AABB<size_t, 3> AABB3u;
typedef AABB<float,  3> AABB3f;
typedef AABB<double, 3> AABB3d;

typedef AABB<int,    4> AABB4i;
typedef AABB<size_t, 4> AABB4u;
typedef AABB<float,  4> AABB4f;
typedef AABB<double, 4> AABB4d;


//
// N-dimensional axis-aligned bounding box class and operations.
//

template <typename T, size_t N>
inline AABB<T, N>::AABB()
{
}

template <typename T, size_t N>
inline AABB<T, N>::AABB(
    const VectorType& min_,
    const VectorType& max_)
  : min(min_)
  , max(max_)
{
}

template <typename T, size_t N>
template <typename U>
inline AABB<T, N>::AABB(const AABB<U, N>& rhs)
  : min(VectorType(rhs.min))
  , max(VectorType(rhs.max))
{
}

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

template <typename T, size_t N>
inline AABB<T, N>::AABB(const Imath::Box<typename ImathVecEquivalent<T, N>::Type>& rhs)
  : min(rhs.min)
  , max(rhs.max)
{
}

template <typename T, size_t N>
inline AABB<T, N>::operator Imath::Box<typename ImathVecEquivalent<T, N>::Type>&()
{
    return reinterpret_cast<Imath::Box<typename ImathVecEquivalent<T, N>::Type>&>(*this);
}

template <typename T, size_t N>
inline AABB<T, N>::operator const Imath::Box<typename ImathVecEquivalent<T, N>::Type>&() const
{
    return reinterpret_cast<const Imath::Box<typename ImathVecEquivalent<T, N>::Type>&>(*this);
}

#endif

template <typename T, size_t N>
inline AABB<T, N> AABB<T, N>::invalid()
{
    AABB<T, N> bbox;
    bbox.invalidate();
    return bbox;
}

template <typename T, size_t N>
inline AABB<T, N> AABB<T, N>::intersect(const AABBType& a, const AABBType& b)
{
    assert(a.is_valid());
    assert(b.is_valid());

    AABBType intersection;

    for (size_t i = 0; i < N; ++i)
    {
        intersection.min[i] = std::max(a.min[i], b.min[i]);
        intersection.max[i] = std::min(a.max[i], b.max[i]);
    }

    return intersection;
}

template <typename T, size_t N>
inline bool AABB<T, N>::overlap(const AABBType& a, const AABBType& b)
{
    assert(a.is_valid());
    assert(b.is_valid());

    for (size_t i = 0; i < N; ++i)
    {
        if (a.min[i] > b.max[i] || a.max[i] < b.min[i])
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline T AABB<T, N>::overlap_ratio(const AABBType& a, const AABBType& b)
{
    assert(a.is_valid());
    assert(b.is_valid());

    T ratio = T(1.0);

    for (size_t i = 0; i < N; ++i)
    {
        const T amin = a.min[i];
        const T amax = a.max[i];
        const T bmin = b.min[i];
        const T bmax = b.max[i];

        const T overlap = std::min(amax, bmax) - std::max(amin, bmin);

        if (overlap <= T(0.0))
            return T(0.0);

        ratio *= overlap / std::min(amax - amin, bmax - bmin);
    }

    return ratio;
}

template <typename T, size_t N>
inline T AABB<T, N>::extent_ratio(const AABBType& a, const AABBType& b)
{
    assert(a.is_valid());
    assert(b.is_valid());

    const VectorType ea = a.extent();
    const VectorType eb = b.extent();

    T ratio = T(1.0);

    for (size_t i = 0; i < N; ++i)
    {
        if (ea[i] != eb[i])
            ratio *= ea[i] / eb[i];
    }

    return ratio;
}

template <typename T, size_t N>
inline Vector<T, N>& AABB<T, N>::operator[](const size_t i)
{
    assert(i < 2);
    return (&min)[i];
}

template <typename T, size_t N>
inline const Vector<T, N>& AABB<T, N>::operator[](const size_t i) const
{
    assert(i < 2);
    return (&min)[i];
}

template <typename T, size_t N>
inline void AABB<T, N>::invalidate()
{
    for (size_t i = 0; i < N; ++i)
    {
        min[i] = std::numeric_limits<T>::max();
        max[i] = signed_min<T>();
    }
}

template <typename T, size_t N>
inline void AABB<T, N>::insert(const VectorType& v)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (min[i] > v[i])
            min[i] = v[i];
        if (max[i] < v[i])
            max[i] = v[i];
    }
}

template <typename T, size_t N>
inline void AABB<T, N>::insert(const AABBType& b)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (min[i] > b.min[i])
            min[i] = b.min[i];
        if (max[i] < b.max[i])
            max[i] = b.max[i];
    }
}

template <typename T, size_t N>
inline void AABB<T, N>::grow(const VectorType& v)
{
    assert(is_valid());

    min -= v;
    max += v;
}

template <typename T, size_t N>
inline void AABB<T, N>::robust_grow(const ValueType eps)
{
    assert(is_valid());

    const VectorType c = ValueType(0.5) * (min + max);
    const VectorType e = max - min;

    for (size_t i = 0; i < N; ++i)
    {
        const ValueType dominant_factor =
            foundation::max(    // needs full qualification
                std::abs(c[i]),
                e[i],
                ValueType(1.0));

        const ValueType delta = dominant_factor * eps;

        min[i] -= delta;
        max[i] += delta;
    }
}

template <typename T, size_t N>
inline void AABB<T, N>::translate(const VectorType& v)
{
    assert(is_valid());

    min += v;
    max += v;
}

template <typename T, size_t N>
inline bool AABB<T, N>::is_valid() const
{
    for (size_t i = 0; i < N; ++i)
    {
        // Return false if NaN values creep in.
        if (!(min[i] <= max[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline size_t AABB<T, N>::rank() const
{
    size_t r = 0;

    for (size_t i = 0; i < N; ++i)
    {
        if (min[i] < max[i])
            ++r;
    }

    return r;
}

template <typename T, size_t N>
inline Vector<T, N> AABB<T, N>::center() const
{
    assert(is_valid());

    return ValueType(0.5) * (min + max);
}

template <typename T, size_t N>
inline T AABB<T, N>::center(const size_t dim) const
{
    assert(is_valid());

    return ValueType(0.5) * (min[dim] + max[dim]);
}

template <typename T, size_t N>
inline Vector<T, N> AABB<T, N>::extent() const
{
    assert(is_valid());

    return max - min;
}

template <typename T, size_t N>
inline T AABB<T, N>::extent(const size_t dim) const
{
    assert(is_valid());

    return max[dim] - min[dim];
}

template <typename T, size_t N>
inline T AABB<T, N>::diameter() const
{
    assert(is_valid());

    return norm(extent());
}

template <typename T, size_t N>
inline T AABB<T, N>::radius() const
{
    assert(is_valid());

    return T(0.5) * diameter();
}

template <typename T, size_t N>
T AABB<T, N>::volume() const
{
    assert(is_valid());

    const VectorType e = max - min;

    ValueType volume = e[0];

    for (size_t i = 1; i < N; ++i)
        volume *= e[i];

    return volume;
}

template <typename T, size_t N>
inline size_t AABB<T, N>::get_corner_count() const
{
    return 1 << N;
}

template <typename T, size_t N>
Vector<T, N> AABB<T, N>::compute_corner(const size_t i) const
{
    assert(is_valid());

    VectorType p;

    for (size_t d = 0; d < N; ++d)
        p[d] = i & (size_t(1) << d) ? max[d] : min[d];

    return p;
}

template <typename T, size_t N>
inline bool AABB<T, N>::contains(const VectorType& v) const
{
    assert(is_valid());

    for (size_t i = 0; i < N; ++i)
    {
        if (v[i] < min[i] || v[i] > max[i])
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool operator!=(const AABB<T, N>& lhs, const AABB<T, N>& rhs)
{
    return lhs.min != rhs.min || lhs.max != rhs.max;
}

template <typename T, size_t N>
inline bool operator==(const AABB<T, N>& lhs, const AABB<T, N>& rhs)
{
    return !(lhs != rhs);
}

template <typename T, size_t N>
inline bool feq(const AABB<T, N>& lhs, const AABB<T, N>& rhs)
{
    return feq(lhs.min, rhs.min) && feq(lhs.max, rhs.max);
}

template <typename T, size_t N>
inline bool feq(const AABB<T, N>& lhs, const AABB<T, N>& rhs, const T eps)
{
    return feq(lhs.min, rhs.min, eps) && feq(lhs.max, rhs.max, eps);
}

template <typename T, size_t N>
inline AABB<T, N> operator+(const AABB<T, N>& lhs, const AABB<T, N>& rhs)
{
    return AABB<T, N>(lhs.min + rhs.min, lhs.max + rhs.max);
}

template <typename T, size_t N>
inline AABB<T, N> operator*(const AABB<T, N>& lhs, const T rhs)
{
    return AABB<T, N>(lhs.min * rhs, lhs.max * rhs);
}

template <typename T, size_t N>
inline AABB<T, N> operator*(const T lhs, const AABB<T, N>& rhs)
{
    return AABB<T, N>(lhs * rhs.min, lhs * rhs.max);
}

template <typename T, size_t N>
inline AABB<T, N>& operator+=(AABB<T, N>& lhs, const AABB<T, N>& rhs)
{
    lhs.min += rhs.min;
    lhs.max += rhs.max;
    return lhs;
}

template <typename T, size_t N>
inline AABB<T, N>& operator*=(AABB<T, N>& lhs, const T rhs)
{
    lhs.min *= rhs;
    lhs.max *= rhs;
    return lhs;
}

template <typename T>
inline T half_surface_area(const AABB<T, 3>& bbox)
{
    assert(bbox.is_valid());

    const Vector<T, 3> e = bbox.max - bbox.min;

    return e[0] * e[1] + e[0] * e[2] + e[1] * e[2];
}

template <typename T>
inline T surface_area(const AABB<T, 3>& bbox)
{
    const T h = half_surface_area(bbox);
    return h + h;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_AABB_H
