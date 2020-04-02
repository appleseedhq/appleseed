
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
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/utility/poison.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>

namespace foundation
{

//
// Shevtsov-Soupikov-Kapustin ray-triangle intersection test.
//
// Reference:
//
//   http://www.graphicon.ru/2007/proceedings/Papers/Paper_46.pdf
//

template <typename T>
struct TriangleSSK
{
    // Types.
    typedef T ValueType;
    typedef Vector<T, 3> VectorType;
    typedef Ray<T, 3> RayType;

    // Scaled normal (the third component is equal to 1.0).
    ValueType       m_nu;
    ValueType       m_nv;

    // Dot product between triangle vertex and scaled normal.
    ValueType       m_np;

    // Triangle vertex.
    ValueType       m_pu;
    ValueType       m_pv;

    // Index of principal component.
    std::uint32_t   m_ci;

    // Triangle edges.
    ValueType       m_e0u;
    ValueType       m_e0v;
    ValueType       m_e1u;
    ValueType       m_e1v;

    // Constructors.
    TriangleSSK();
    TriangleSSK(
        const VectorType&   v0,
        const VectorType&   v1,
        const VectorType&   v2);

    // Construct a triangle from another triangle of a different type.
    template <typename U>
    TriangleSSK(const TriangleSSK<U>& rhs);

    bool intersect(
        const RayType&      ray,
        ValueType&          t,
        ValueType&          u,
        ValueType&          v) const;

    bool intersect(const RayType& ray) const;
};

template <typename T>
struct TriangleSSKSupportPlane
{
    // Types.
    typedef T ValueType;
    typedef Vector<T, 3> VectorType;
    typedef Ray<T, 3> RayType;

    ValueType       m_nu;
    ValueType       m_nv;
    ValueType       m_np;
    std::uint32_t   m_ci;

    // Constructors.
    TriangleSSKSupportPlane();
    explicit TriangleSSKSupportPlane(const TriangleSSK<T>& triangle);

    void initialize(const TriangleSSK<T>& triangle);

    ValueType intersect(
        const VectorType&   org,
        const VectorType&   dir) const;
};

// Poisoning.
template <typename T>
class PoisonImpl<TriangleSSKSupportPlane<T>>
{
  public:
    static void do_poison(TriangleSSKSupportPlane<T>& plane);
};


//
// TriangleSSK class implementation.
//

template <typename T>
inline TriangleSSK<T>::TriangleSSK()
{
}

template <typename T>
inline TriangleSSK<T>::TriangleSSK(
    const VectorType&       v0,
    const VectorType&       v1,
    const VectorType&       v2)
{
    // Compute triangle edges.
    const VectorType e0 = v1 - v0;
    const VectorType e1 = v2 - v0;

    // Compute triangle normal.
    const VectorType n = cross(e0, e1);

    // Store triangle normal.
    const size_t w = max_abs_index(n);
    const size_t u = (2 - w) >> 1;
    const size_t v = 3 - u - w;
    assert(std::abs(n[w]) > ValueType(0.0));
    ValueType rcp_nw = ValueType(1.0) / n[w];
    m_nu = n[u] * rcp_nw;
    m_nv = n[v] * rcp_nw;
    m_ci = static_cast<std::uint32_t>(w);

    // Store triangle vertex.
    m_pu = v0[u];
    m_pv = v0[v];

    // Compute and store dot product between triangle vertex and scaled normal.
    m_np = m_nu * m_pu + m_nv * m_pv + v0[w];

    // Store triangle edges.
    if (w == 1) rcp_nw = -rcp_nw;
    m_e0u = e0[u] * rcp_nw;
    m_e0v = e0[v] * rcp_nw;
    m_e1u = e1[u] * rcp_nw;
    m_e1v = e1[v] * rcp_nw;
}

template <typename T>
template <typename U>
APPLESEED_FORCE_INLINE TriangleSSK<T>::TriangleSSK(const TriangleSSK<U>& rhs)
  : m_nu (static_cast<ValueType>(rhs.m_nu))
  , m_nv (static_cast<ValueType>(rhs.m_nv))
  , m_np (static_cast<ValueType>(rhs.m_np))
  , m_pu (static_cast<ValueType>(rhs.m_pu))
  , m_pv (static_cast<ValueType>(rhs.m_pv))
  , m_ci (rhs.m_ci)
  , m_e0u(static_cast<ValueType>(rhs.m_e0u))
  , m_e0v(static_cast<ValueType>(rhs.m_e0v))
  , m_e1u(static_cast<ValueType>(rhs.m_e1u))
  , m_e1v(static_cast<ValueType>(rhs.m_e1v))
{
}

template <>
APPLESEED_FORCE_INLINE bool TriangleSSK<float>::intersect(
    const RayType&          ray,
    ValueType&              t,
    ValueType&              u,
    ValueType&              v) const
{
    // Retrieve indices.
    const size_t k = m_ci;
    const size_t i = (2 - k) >> 1;
    const size_t j = 3 - i - k;

    // Retrieve ray origin and direction.
    const ValueType ou = ray.m_org[i];
    const ValueType ov = ray.m_org[j];
    const ValueType ow = ray.m_org[k];
    const ValueType du = ray.m_dir[i];
    const ValueType dv = ray.m_dir[j];
    const ValueType dw = ray.m_dir[k];

    // Compute the barycentric coordinates of the intersection point.
    const ValueType det = du * m_nu + dv * m_nv + dw;
    const ValueType tprime = m_np - (ou * m_nu + ov * m_nv + ow);
    const ValueType Du = du * tprime - (m_pu - ou) * det;
    const ValueType Dv = dv * tprime - (m_pv - ov) * det;
    const ValueType uprime = m_e1v * Du - m_e1u * Dv;
    const ValueType vprime = m_e0u * Dv - m_e0v * Du;
    const ValueType wprime = det - uprime - vprime;

    // Check that the intersection point lies inside the triangle.
#ifdef APPLESEED_USE_SSE
    APPLESEED_SIMD4_ALIGN float detarray[4] = { uprime, uprime, vprime, wprime };
    const __m128 mu = _mm_load_ps(detarray);
    const __m128 mv = _mm_shuffle_ps(mu, mu, _MM_SHUFFLE(2, 3, 3, 2));
    const __m128 product = _mm_mul_ps(mu, mv);
    const __m128 zero = _mm_set1_ps(0.0f);
    const __m128 cmp = _mm_cmpge_ps(product, zero);
    const int mask = _mm_movemask_ps(cmp);
    if (mask != 0xF)
        return false;
#else
    if (uprime * vprime < ValueType(0.0) ||
        uprime * wprime < ValueType(0.0) ||
        vprime * wprime < ValueType(0.0))
        return false;
#endif

    const ValueType rdet = ValueType(1.0) / det;

    // Calculate t parameter and test bounds.
    t = tprime * rdet;
    if (t >= ray.m_tmax || t < ray.m_tmin)
        return false;

    // Calculate u and v parameters.
    u = uprime * rdet;
    v = vprime * rdet;
    return true;
}

template <>
APPLESEED_FORCE_INLINE bool TriangleSSK<double>::intersect(
    const RayType&          ray,
    ValueType&              t,
    ValueType&              u,
    ValueType&              v) const
{
    // Retrieve indices.
    const size_t k = m_ci;
    const size_t i = (2 - k) >> 1;
    const size_t j = 3 - i - k;

    // Retrieve ray origin and direction.
    const ValueType ou = ray.m_org[i];
    const ValueType ov = ray.m_org[j];
    const ValueType ow = ray.m_org[k];
    const ValueType du = ray.m_dir[i];
    const ValueType dv = ray.m_dir[j];
    const ValueType dw = ray.m_dir[k];

    // Compute the barycentric coordinates of the intersection point.
    const ValueType det = du * m_nu + dv * m_nv + dw;
    const ValueType tprime = m_np - (ou * m_nu + ov * m_nv + ow);
    const ValueType Du = du * tprime - (m_pu - ou) * det;
    const ValueType Dv = dv * tprime - (m_pv - ov) * det;
    const ValueType uprime = m_e1v * Du - m_e1u * Dv;
    const ValueType vprime = m_e0u * Dv - m_e0v * Du;
    const ValueType wprime = det - uprime - vprime;

    // Check that the intersection point lies inside the triangle.
#ifdef APPLESEED_USE_SSE
    const __m128d zero = _mm_set1_pd(0.0);
    const __m128d mdetu = _mm_set1_pd(uprime);
    const __m128d mdetv = _mm_set1_pd(vprime);
    const __m128d mdetvw = _mm_set_pd(vprime, wprime);
    const int mask =
          _mm_movemask_pd(_mm_cmpge_pd(_mm_mul_pd(mdetu, mdetvw), zero))
        & _mm_movemask_pd(_mm_cmpge_pd(_mm_mul_pd(mdetv, mdetvw), zero));
    if (mask != 3)
        return false;
#else
    if (uprime * vprime < ValueType(0.0) ||
        uprime * wprime < ValueType(0.0) ||
        vprime * wprime < ValueType(0.0))
        return false;
#endif

    const ValueType rdet = ValueType(1.0) / det;

    // Calculate t parameter and test bounds.
    t = tprime * rdet;
    if (t >= ray.m_tmax || t < ray.m_tmin)
        return false;

    // Calculate u and v parameters.
    u = uprime * rdet;
    v = vprime * rdet;
    return true;
}

template <>
APPLESEED_FORCE_INLINE bool TriangleSSK<float>::intersect(const RayType& ray) const
{
    // Retrieve indices.
    const size_t k = m_ci;
    const size_t i = (2 - k) >> 1;
    const size_t j = 3 - i - k;

    // Retrieve ray origin and direction.
    const ValueType ou = ray.m_org[i];
    const ValueType ov = ray.m_org[j];
    const ValueType ow = ray.m_org[k];
    const ValueType du = ray.m_dir[i];
    const ValueType dv = ray.m_dir[j];
    const ValueType dw = ray.m_dir[k];

    // Compute the barycentric coordinates of the intersection point.
    const ValueType det = du * m_nu + dv * m_nv + dw;
    const ValueType tprime = m_np - (ou * m_nu + ov * m_nv + ow);
    const ValueType Du = du * tprime - (m_pu - ou) * det;
    const ValueType Dv = dv * tprime - (m_pv - ov) * det;
    const ValueType uprime = m_e1v * Du - m_e1u * Dv;
    const ValueType vprime = m_e0u * Dv - m_e0v * Du;
    const ValueType wprime = det - uprime - vprime;

    // Check that the intersection point lies inside the triangle.
#ifdef APPLESEED_USE_SSE
    APPLESEED_SIMD4_ALIGN float detarray[4] = { uprime, uprime, vprime, wprime };
    const __m128 mu = _mm_load_ps(detarray);
    const __m128 mv = _mm_shuffle_ps(mu, mu, _MM_SHUFFLE(2, 3, 3, 2));
    const __m128 product = _mm_mul_ps(mu, mv);
    const __m128 zero = _mm_set1_ps(0.0f);
    const __m128 cmp = _mm_cmpge_ps(product, zero);
    const int mask = _mm_movemask_ps(cmp);
    if (mask != 0xF)
        return false;
#else
    if (uprime * vprime < 0.0f ||
        uprime * wprime < 0.0f ||
        vprime * wprime < 0.0f)
        return false;
#endif

    // Calculate t parameter and test bounds.
    const ValueType t = tprime / det;
    return t >= ray.m_tmin && t < ray.m_tmax;
}

template <>
APPLESEED_FORCE_INLINE bool TriangleSSK<double>::intersect(const RayType& ray) const
{
    // Retrieve indices.
    const size_t k = m_ci;
    const size_t i = (2 - k) >> 1;
    const size_t j = 3 - i - k;

    // Retrieve ray origin and direction.
    const ValueType ou = ray.m_org[i];
    const ValueType ov = ray.m_org[j];
    const ValueType ow = ray.m_org[k];
    const ValueType du = ray.m_dir[i];
    const ValueType dv = ray.m_dir[j];
    const ValueType dw = ray.m_dir[k];

    // Compute the barycentric coordinates of the intersection point.
    const ValueType det = du * m_nu + dv * m_nv + dw;
    const ValueType tprime = m_np - (ou * m_nu + ov * m_nv + ow);
    const ValueType Du = du * tprime - (m_pu - ou) * det;
    const ValueType Dv = dv * tprime - (m_pv - ov) * det;
    const ValueType uprime = m_e1v * Du - m_e1u * Dv;
    const ValueType vprime = m_e0u * Dv - m_e0v * Du;
    const ValueType wprime = det - uprime - vprime;

    // Check that the intersection point lies inside the triangle.
#ifdef APPLESEED_USE_SSE
    const __m128d zero = _mm_set1_pd(0.0);
    const __m128d mdetu = _mm_set1_pd(uprime);
    const __m128d mdetv = _mm_set1_pd(vprime);
    const __m128d mdetvw = _mm_set_pd(vprime, wprime);
    const int mask =
          _mm_movemask_pd(_mm_cmpge_pd(_mm_mul_pd(mdetu, mdetvw), zero))
        & _mm_movemask_pd(_mm_cmpge_pd(_mm_mul_pd(mdetv, mdetvw), zero));
    if (mask != 3)
        return false;
#else
    if (uprime * vprime < 0.0 ||
        uprime * wprime < 0.0 ||
        vprime * wprime < 0.0)
        return false;
#endif

    // Calculate t parameter and test bounds.
    const ValueType t = tprime / det;
    return t >= ray.m_tmin && t < ray.m_tmax;
}


//
// TriangleSSKSupportPlane class implementation.
//

template <typename T>
inline TriangleSSKSupportPlane<T>::TriangleSSKSupportPlane()
{
}

template <typename T>
inline TriangleSSKSupportPlane<T>::TriangleSSKSupportPlane(const TriangleSSK<T>& triangle)
{
    initialize(triangle);
}

template <typename T>
inline void TriangleSSKSupportPlane<T>::initialize(const TriangleSSK<T>& triangle)
{
    m_nu = triangle.m_nu;
    m_nv = triangle.m_nv;
    m_np = triangle.m_np;
    m_ci = triangle.m_ci;
}

template <typename T>
inline T TriangleSSKSupportPlane<T>::intersect(
    const VectorType&       org,
    const VectorType&       dir) const
{
    // Retrieve indices.
    const size_t k = m_ci;
    const size_t i = (2 - k) >> 1;
    const size_t j = 3 - i - k;

    // Retrieve ray origin and direction.
    const ValueType ou = org[i];
    const ValueType ov = org[j];
    const ValueType ow = org[k];
    const ValueType du = dir[i];
    const ValueType dv = dir[j];
    const ValueType dw = dir[k];

    // Calculate and return t parameter.
    const ValueType det = du * m_nu + dv * m_nv + dw;
    const ValueType tprime = m_np - (ou * m_nu + ov * m_nv + ow);
    return tprime / det;
}

template <typename T>
void PoisonImpl<TriangleSSKSupportPlane<T>>::do_poison(TriangleSSKSupportPlane<T>& plane)
{
    always_poison(plane.m_nu);
    always_poison(plane.m_nv);
    always_poison(plane.m_np);
    always_poison(plane.m_ci);
}

}   // namespace foundation
