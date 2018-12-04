
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
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/utility/poison.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <limits>

namespace foundation
{

//
// Ray class.
//
// A ray is defined as a pair (origin, direction) where origin and direction are vectors
// of arbitrary type and dimension, and two abscissa tmin and tmax with tmin >= 0.0 and
// tmin <= tmax that define the ray interval. By convention, tmin is inclusive and tmax
// is exclusive, i.e. the ray interval is [tmin, tmax).
//

template <typename T, size_t N>
class Ray
{
  public:
    // Types.
    typedef T ValueType;
    typedef Vector<T, N> VectorType;
    typedef Ray<T, N> RayType;

    // Dimension.
    static const size_t Dimension = N;

    // Public members.
    VectorType  m_org;                      // ray origin
    VectorType  m_dir;                      // ray direction (not necessarily unit-length)
    ValueType   m_tmin;                     // beginning of the ray interval (inclusive)
    ValueType   m_tmax;                     // end of the ray interval (exclusive)

    // Constructors.
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Ray() = default;                        // leave all fields uninitialized
#else
    Ray() {}                                // leave all fields uninitialized
#endif
    Ray(
        const VectorType&   org,
        const VectorType&   dir,
        const ValueType     tmin = ValueType(0.0),
        const ValueType     tmax = std::numeric_limits<ValueType>::max());

    // Construct a ray from another ray of a different type.
    template <typename U>
    Ray(const Ray<U, N>& rhs);

    // Return true if the ray has finite length.
    bool is_finite() const;

    // Get length of the ray interval, assuming that m_tmax is finite.
    ValueType get_length() const;

    // Return the point of the ray at abscissa t, t >= 0.
    VectorType point_at(const ValueType t) const;
};

// Poisoning.
template <typename T, size_t N>
class PoisonImpl<Ray<T, N>>
{
  public:
    static void do_poison(Ray<T, N>& ray);
};

// Exact inequality and equality tests.
template <typename T, size_t N> bool operator!=(const Ray<T, N>& lhs, const Ray<T, N>& rhs);
template <typename T, size_t N> bool operator==(const Ray<T, N>& lhs, const Ray<T, N>& rhs);

// Approximate equality tests.
template <typename T, size_t N> bool feq(const Ray<T, N>& lhs, const Ray<T, N>& rhs);
template <typename T, size_t N> bool feq(const Ray<T, N>& lhs, const Ray<T, N>& rhs, const T eps);


//
// Full specializations for 2D, 3D and 4D rays of type float and double.
//

typedef Ray<float,  2> Ray2f;
typedef Ray<double, 2> Ray2d;
typedef Ray<float,  3> Ray3f;
typedef Ray<double, 3> Ray3d;
typedef Ray<float,  4> Ray4f;
typedef Ray<double, 4> Ray4d;


//
// Complementary information about a ray.
//

template <typename T, size_t N>
class RayInfo
{
  public:
    // Types.
    typedef T ValueType;
    typedef Vector<T, N> VectorType;
    typedef Ray<T, N> RayType;
    typedef RayInfo<T, N> RayInfoType;

    // Dimension.
    static const size_t Dimension = N;

    // Reciprocal of the ray direction.
    VectorType m_rcp_dir;

    // Sign of the ray direction (for the i'th component, the sign value
    // is 1 if the component is positive or null, and 0 if the component
    // is strictly negative).
    Vector<uint32, N> m_sgn_dir;

    // Constructors.
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    RayInfo() = default;                    // leave all fields uninitialized
#else
    RayInfo() {}                            // leave all fields uninitialized
#endif
    explicit RayInfo(const RayType& ray);   // initialize with a ray

    // Construct ray info from other ray info of a different type.
    template <typename U>
    RayInfo(const RayInfo<U, N>& rhs);
};

#ifdef APPLESEED_USE_SSE

template <>
class RayInfo<double, 3>
{
  public:
    // Types.
    typedef double ValueType;
    typedef Vector3d VectorType;
    typedef Ray3d RayType;
    typedef RayInfo<double, 3> RayInfoType;

    // Dimension.
    static const size_t Dimension = 3;

    // Reciprocal of the ray direction.
    APPLESEED_SIMD4_ALIGN VectorType m_rcp_dir;

    // Sign of the ray direction (for the i'th component, the sign value
    // is 1 if the component is positive or null, and 0 if the component
    // is strictly negative).
    APPLESEED_SIMD4_ALIGN Vector<uint32, 4> m_sgn_dir;

    // Constructors.
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    RayInfo() = default;                    // leave all fields uninitialized
#else
    RayInfo() {}                            // leave all fields uninitialized
#endif
    explicit RayInfo(const RayType& ray);   // initialize with a ray

    // Construct ray info from other ray info of a different type.
    template <typename U>
    RayInfo(const RayInfo<U, 3>& rhs);
};

#endif  // APPLESEED_USE_SSE


//
// Full specializations of RayInfo for 2D, 3D and 4D rays of type float and double.
//

typedef RayInfo<float,  2> RayInfo2f;
typedef RayInfo<double, 2> RayInfo2d;
typedef RayInfo<float,  3> RayInfo3f;
typedef RayInfo<double, 3> RayInfo3d;
typedef RayInfo<float,  4> RayInfo4f;
typedef RayInfo<double, 4> RayInfo4d;


//
// Ray class implementation.
//

template <typename T, size_t N>
inline Ray<T, N>::Ray(
    const VectorType&   org,
    const VectorType&   dir,
    const ValueType     tmin,
    const ValueType     tmax)
  : m_org(org)
  , m_dir(dir)
  , m_tmin(tmin)
  , m_tmax(tmax)
{
}

template <typename T, size_t N>
template <typename U>
inline Ray<T, N>::Ray(const Ray<U, N>& rhs)
  : m_org(rhs.m_org)
  , m_dir(rhs.m_dir)
  , m_tmin(static_cast<T>(rhs.m_tmin))
  , m_tmax(static_cast<T>(rhs.m_tmax))
{
}

template <typename T, size_t N>
inline bool Ray<T, N>::is_finite() const
{
    return m_tmax < std::numeric_limits<ValueType>::max();
}

template <typename T, size_t N>
inline T Ray<T, N>::get_length() const
{
    assert(is_finite());
    return (m_tmax - m_tmin) * foundation::norm(m_dir);
}

template <typename T, size_t N>
inline typename Ray<T, N>::VectorType Ray<T, N>::point_at(const ValueType t) const
{
    return m_org + t * m_dir;
}

template <typename T, size_t N>
void PoisonImpl<Ray<T, N>>::do_poison(Ray<T, N>& ray)
{
    poison(ray.m_org);
    poison(ray.m_dir);
    poison(ray.m_tmin);
    poison(ray.m_tmax);
}

template <typename T, size_t N>
inline bool operator!=(const Ray<T, N>& lhs, const Ray<T, N>& rhs)
{
    return
           lhs.m_org  != rhs.m_org
        || lhs.m_dir  != rhs.m_dir
        || lhs.m_tmin != rhs.m_tmin
        || lhs.m_tmax != rhs.m_tmax;
}

template <typename T, size_t N>
inline bool operator==(const Ray<T, N>& lhs, const Ray<T, N>& rhs)
{
    return !(lhs != rhs);
}

template <typename T, size_t N>
inline bool feq(const Ray<T, N>& lhs, const Ray<T, N>& rhs)
{
    return
           feq(lhs.m_org,  rhs.m_org)
        && feq(lhs.m_dir,  rhs.m_dir)
        && feq(lhs.m_tmin, rhs.m_tmin)
        && feq(lhs.m_tmax, rhs.m_tmax);
}

template <typename T, size_t N>
inline bool feq(const Ray<T, N>& lhs, const Ray<T, N>& rhs, const T eps)
{
    return
           feq(lhs.m_org,  rhs.m_org,  eps)
        && feq(lhs.m_dir,  rhs.m_dir,  eps)
        && feq(lhs.m_tmin, rhs.m_tmin, eps)
        && feq(lhs.m_tmax, rhs.m_tmax, eps);
}


//
// RayInfo class implementation.
//

template <typename T, size_t N>
inline RayInfo<T, N>::RayInfo(const RayType& ray)
{
    for (size_t i = 0; i < Dimension; ++i)
    {
        m_rcp_dir[i] = ValueType(1.0) / ray.m_dir[i];
        m_sgn_dir[i] = m_rcp_dir[i] >= ValueType(0.0) ? 1 : 0;
    }
}

template <typename T, size_t N>
template <typename U>
inline RayInfo<T, N>::RayInfo(const RayInfo<U, N>& rhs)
  : m_rcp_dir(rhs.m_rcp_dir)
{
    for (size_t i = 0; i < Dimension; ++i)
        m_sgn_dir[i] = rhs.m_sgn_dir[i];
}

#ifdef APPLESEED_USE_SSE

APPLESEED_FORCE_INLINE RayInfo<double, 3>::RayInfo(const RayType& ray)
{
    const __m128d one = _mm_set1_pd(1.0);
    const __m128d rcp_dir0 = _mm_div_pd(one, _mm_loadu_pd(&ray.m_dir[0]));
    const __m128d rcp_dir2 = _mm_div_pd(one, _mm_set1_pd(ray.m_dir[2]));

    _mm_store_pd(&m_rcp_dir[0], rcp_dir0);
    _mm_store_sd(&m_rcp_dir[2], rcp_dir2);

    const __m128d zero = _mm_setzero_pd();
    const __m128d sgn_dir0 = _mm_cmpge_pd(rcp_dir0, zero);
    const __m128d sgn_dir2 = _mm_cmpge_pd(rcp_dir2, zero);
    const __m128i mask = _mm_set_epi32(0, 1, 0, 1);
    const __m128i sgn0 = _mm_and_si128(_mm_castpd_si128(sgn_dir0), mask);
    const __m128i sgn2 = _mm_and_si128(_mm_castpd_si128(sgn_dir2), mask);
    const __m128i sgn = _mm_unpacklo_epi64(_mm_shuffle_epi32(sgn0, _MM_SHUFFLE(3, 3, 2, 0)), sgn2);

    _mm_store_si128((__m128i*)&m_sgn_dir[0], sgn);
}

template <typename U>
inline RayInfo<double, 3>::RayInfo(const RayInfo<U, 3>& rhs)
  : m_rcp_dir(rhs.m_rcp_dir)
{
    for (size_t i = 0; i < Dimension; ++i)
        m_sgn_dir[i] = rhs.m_sgn_dir[i];
}

#endif  // APPLESEED_USE_SSE

}   // namespace foundation
