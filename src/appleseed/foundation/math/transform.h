
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_MATH_TRANSFORM_H
#define APPLESEED_FOUNDATION_MATH_TRANSFORM_H

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/matrix.h"
#include "foundation/math/quaternion.h"
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>

namespace foundation
{

//
// The Transform class represents a 3D affine transformation, and allows
// transformation of points, vectors, normals, rays and bounding boxes
// from parent space to local space (as defined by the transform) and
// from local space to parent space.
//
// Note: normals and ray directions are not required to be unit-length,
// and are never normalized after transformation.
//

template <typename T>
class Transform
{
  public:
    // Matrix and transform types.
    typedef Matrix<T, 4, 4> MatrixType;
    typedef Transform<T> TransformType;

    // Constructors.
    Transform();                                // leave the transformation uninitialized
    explicit Transform(
        const MatrixType& local_to_parent);
    Transform(
        const MatrixType& local_to_parent,
        const MatrixType& parent_to_local);     // must be equal to inverse(local_to_parent)

    // Return the identity transform.
    static Transform identity();

    // Retrieve the transformation matrices.
    const MatrixType& get_local_to_parent() const;
    const MatrixType& get_parent_to_local() const;

    // Compose two transformations.
    TransformType operator*(const TransformType& rhs) const;

    // Transform a 3D point.
    template <typename U> Vector<U, 3> point_to_local(const Vector<U, 3>& p) const;
    template <typename U> Vector<U, 3> point_to_parent(const Vector<U, 3>& p) const;

    // Transform a 3D vector.
    template <typename U> Vector<U, 3> vector_to_local(const Vector<U, 3>& v) const;
    template <typename U> Vector<U, 3> vector_to_parent(const Vector<U, 3>& v) const;

    // Transform a 3D normal.
    template <typename U> Vector<U, 3> normal_to_local(const Vector<U, 3>& n) const;
    template <typename U> Vector<U, 3> normal_to_parent(const Vector<U, 3>& n) const;

    // Transform a 3D ray.
    template <typename U> Ray<U, 3> to_local(const Ray<U, 3>& r) const;
    template <typename U> Ray<U, 3> to_parent(const Ray<U, 3>& r) const;

    // Transform a 3D axis-aligned bounding box.
    // If the bounding box is invalid, it is returned unmodified.
    template <typename U> AABB<U, 3> to_local(const AABB<U, 3>& b) const;
    template <typename U> AABB<U, 3> to_parent(const AABB<U, 3>& b) const;

  private:
    template <typename>
    friend class TransformInterpolator;

    MatrixType  m_local_to_parent;
    MatrixType  m_parent_to_local;
};

// Exact inequality and equality tests.
template <typename T> bool operator!=(const Transform<T>& lhs, const Transform<T>& rhs);
template <typename T> bool operator==(const Transform<T>& lhs, const Transform<T>& rhs);

// Approximate equality tests.
template <typename T> bool feq(const Transform<T>& lhs, const Transform<T>& rhs);
template <typename T> bool feq(const Transform<T>& lhs, const Transform<T>& rhs, const T eps);


//
// Specializations for single and double precision transformations.
//

typedef Transform<float>  Transformf;
typedef Transform<double> Transformd;


//
// The TransformInterpolator class allows to interpolate between two
// rigid transformations (combinations of rotations and translations).
//

template <typename T>
class TransformInterpolator
{
  public:
    // Matrix and transform types.
    typedef Matrix<T, 4, 4> MatrixType;
    typedef Transform<T> TransformType;

    // Constructors.
    TransformInterpolator();                    // leave the interpolator uninitialized
    TransformInterpolator(
        const TransformType& from,
        const TransformType& to);

    // Set the initial and final transformations.
    // Returns true on success, false otherwise.
    bool set_transforms(
        const TransformType& from,
        const TransformType& to);

    TransformType evaluate(const T t) const;

  private:
    Vector<T, 3>  m_s0, m_s1;
    Quaternion<T> m_q0, m_q1;
    Vector<T, 3>  m_t0, m_t1;
};


//
// Specializations for single and double precision transformation interpolators.
//

typedef TransformInterpolator<float>  TransformInterpolatorf;
typedef TransformInterpolator<double> TransformInterpolatord;


//
// Transform class implementation.
//

template <typename T>
inline Transform<T>::Transform()
{
}

template <typename T>
inline Transform<T>::Transform(const MatrixType& local_to_parent)
  : m_local_to_parent(local_to_parent)
  , m_parent_to_local(inverse(local_to_parent))
{
}

template <typename T>
inline Transform<T>::Transform(
    const MatrixType& local_to_parent,
    const MatrixType& parent_to_local)
  : m_local_to_parent(local_to_parent)
  , m_parent_to_local(parent_to_local)
{
    assert(feq(m_local_to_parent * m_parent_to_local, MatrixType::identity(), make_eps<T>(1.0e-4f, 1.0e-6)));
}

template <typename T>
inline Transform<T> Transform<T>::identity()
{
    return Transform(MatrixType::identity(), MatrixType::identity());
}

template <typename T>
inline const Matrix<T, 4, 4>& Transform<T>::get_local_to_parent() const
{
    return m_local_to_parent;
}

template <typename T>
inline const Matrix<T, 4, 4>& Transform<T>::get_parent_to_local() const
{
    return m_parent_to_local;
}

template <typename T>
inline Transform<T> Transform<T>::operator*(const TransformType& rhs) const
{
    return
        Transform<T>(
            rhs.m_local_to_parent * m_local_to_parent,
            m_parent_to_local * rhs.m_parent_to_local);
}

template <typename T>
template <typename U>
inline Vector<U, 3> Transform<T>::point_to_local(const Vector<U, 3>& p) const
{
    Vector<U, 3> res;

    res.x = static_cast<U>(m_parent_to_local[ 0] * T(p.x) +
                           m_parent_to_local[ 1] * T(p.y) +
                           m_parent_to_local[ 2] * T(p.z) +
                           m_parent_to_local[ 3]);

    res.y = static_cast<U>(m_parent_to_local[ 4] * T(p.x) +
                           m_parent_to_local[ 5] * T(p.y) +
                           m_parent_to_local[ 6] * T(p.z) +
                           m_parent_to_local[ 7]);

    res.z = static_cast<U>(m_parent_to_local[ 8] * T(p.x) +
                           m_parent_to_local[ 9] * T(p.y) +
                           m_parent_to_local[10] * T(p.z) +
                           m_parent_to_local[11]);

    const U w =
            static_cast<U>(m_parent_to_local[12] * T(p.x) +
                           m_parent_to_local[13] * T(p.y) +
                           m_parent_to_local[14] * T(p.z) +
                           m_parent_to_local[15]);

    assert(w != U(0.0));

    if (w != U(1.0))
        res /= w;

    return res;
}

template <typename T>
template <typename U>
inline Vector<U, 3> Transform<T>::point_to_parent(const Vector<U, 3>& p) const
{
    Vector<U, 3> res;

    res.x = static_cast<U>(m_local_to_parent[ 0] * T(p.x) +
                           m_local_to_parent[ 1] * T(p.y) +
                           m_local_to_parent[ 2] * T(p.z) +
                           m_local_to_parent[ 3]);

    res.y = static_cast<U>(m_local_to_parent[ 4] * T(p.x) +
                           m_local_to_parent[ 5] * T(p.y) +
                           m_local_to_parent[ 6] * T(p.z) +
                           m_local_to_parent[ 7]);

    res.z = static_cast<U>(m_local_to_parent[ 8] * T(p.x) +
                           m_local_to_parent[ 9] * T(p.y) +
                           m_local_to_parent[10] * T(p.z) +
                           m_local_to_parent[11]);

    const U w =
            static_cast<U>(m_local_to_parent[12] * T(p.x) +
                           m_local_to_parent[13] * T(p.y) +
                           m_local_to_parent[14] * T(p.z) +
                           m_local_to_parent[15]);

    assert(w != U(0.0));

    if (w != U(1.0))
        res /= w;

    return res;
}

template <typename T>
template <typename U>
inline Vector<U, 3> Transform<T>::vector_to_local(const Vector<U, 3>& v) const
{
    Vector<U, 3> res;

    res.x = static_cast<U>(m_parent_to_local[ 0] * T(v.x) +
                           m_parent_to_local[ 1] * T(v.y) +
                           m_parent_to_local[ 2] * T(v.z));

    res.y = static_cast<U>(m_parent_to_local[ 4] * T(v.x) +
                           m_parent_to_local[ 5] * T(v.y) +
                           m_parent_to_local[ 6] * T(v.z));

    res.z = static_cast<U>(m_parent_to_local[ 8] * T(v.x) +
                           m_parent_to_local[ 9] * T(v.y) +
                           m_parent_to_local[10] * T(v.z));

    return res;
}

template <typename T>
template <typename U>
inline Vector<U, 3> Transform<T>::vector_to_parent(const Vector<U, 3>& v) const
{
    Vector<U, 3> res;

    res.x = static_cast<U>(m_local_to_parent[ 0] * T(v.x) +
                           m_local_to_parent[ 1] * T(v.y) +
                           m_local_to_parent[ 2] * T(v.z));

    res.y = static_cast<U>(m_local_to_parent[ 4] * T(v.x) +
                           m_local_to_parent[ 5] * T(v.y) +
                           m_local_to_parent[ 6] * T(v.z));

    res.z = static_cast<U>(m_local_to_parent[ 8] * T(v.x) +
                           m_local_to_parent[ 9] * T(v.y) +
                           m_local_to_parent[10] * T(v.z));

    return res;
}

template <typename T>
template <typename U>
inline Vector<U, 3> Transform<T>::normal_to_local(const Vector<U, 3>& n) const
{
    Vector<U, 3> res;

    res.x = static_cast<U>(m_local_to_parent[ 0] * T(n.x) +
                           m_local_to_parent[ 4] * T(n.y) +
                           m_local_to_parent[ 8] * T(n.z));

    res.y = static_cast<U>(m_local_to_parent[ 1] * T(n.x) +
                           m_local_to_parent[ 5] * T(n.y) +
                           m_local_to_parent[ 9] * T(n.z));

    res.z = static_cast<U>(m_local_to_parent[ 2] * T(n.x) +
                           m_local_to_parent[ 6] * T(n.y) +
                           m_local_to_parent[10] * T(n.z));

    return res;
}

template <typename T>
template <typename U>
inline Vector<U, 3> Transform<T>::normal_to_parent(const Vector<U, 3>& n) const
{
    Vector<U, 3> res;

    res.x = static_cast<U>(m_parent_to_local[ 0] * T(n.x) +
                           m_parent_to_local[ 4] * T(n.y) +
                           m_parent_to_local[ 8] * T(n.z));

    res.y = static_cast<U>(m_parent_to_local[ 1] * T(n.x) +
                           m_parent_to_local[ 5] * T(n.y) +
                           m_parent_to_local[ 9] * T(n.z));

    res.z = static_cast<U>(m_parent_to_local[ 2] * T(n.x) +
                           m_parent_to_local[ 6] * T(n.y) +
                           m_parent_to_local[10] * T(n.z));

    return res;
}

template <typename T>
template <typename U>
inline Ray<U, 3> Transform<T>::to_local(const Ray<U, 3>& r) const
{
    Ray<U, 3> res;

    res.m_org = point_to_local(r.m_org);
    res.m_dir = vector_to_local(r.m_dir);
    res.m_tmin = r.m_tmin;
    res.m_tmax = r.m_tmax;

    return res;
}

template <typename T>
template <typename U>
inline Ray<U, 3> Transform<T>::to_parent(const Ray<U, 3>& r) const
{
    Ray<U, 3> res;

    res.m_org = point_to_parent(r.m_org);
    res.m_dir = vector_to_parent(r.m_dir);
    res.m_tmin = r.m_tmin;
    res.m_tmax = r.m_tmax;

    return res;
}

template <typename T>
template <typename U>
inline AABB<U, 3> Transform<T>::to_local(const AABB<U, 3>& b) const
{
    //
    // Note: there are more efficient (although possibly less precise) ways
    // of transforming a bounding box. Here are some references:
    //
    //   http://www.cs.unc.edu/~zhangh/technotes/bbox.pdf
    //   http://www.ics.uci.edu/~arvo/code/TransformingBoxes.c
    //
    //   "Transforming Axis-Aligned Bounding Boxes",
    //   by Jim Arvo, in "Graphics Gems", Academic Press, 1990.
    //

    if (!b.is_valid())
        return b;

    AABB<U, 3> res;
    res.invalidate();

    res.insert(point_to_local(Vector<U, 3>(b[0][0], b[0][1], b[0][2])));
    res.insert(point_to_local(Vector<U, 3>(b[0][0], b[0][1], b[1][2])));
    res.insert(point_to_local(Vector<U, 3>(b[0][0], b[1][1], b[1][2])));
    res.insert(point_to_local(Vector<U, 3>(b[0][0], b[1][1], b[0][2])));
    res.insert(point_to_local(Vector<U, 3>(b[1][0], b[1][1], b[0][2])));
    res.insert(point_to_local(Vector<U, 3>(b[1][0], b[1][1], b[1][2])));
    res.insert(point_to_local(Vector<U, 3>(b[1][0], b[0][1], b[1][2])));
    res.insert(point_to_local(Vector<U, 3>(b[1][0], b[0][1], b[0][2])));

    return res;
}

template <typename T>
template <typename U>
inline AABB<U, 3> Transform<T>::to_parent(const AABB<U, 3>& b) const
{
    if (!b.is_valid())
        return b;

    AABB<U, 3> res;
    res.invalidate();

    res.insert(point_to_parent(Vector<U, 3>(b[0][0], b[0][1], b[0][2])));
    res.insert(point_to_parent(Vector<U, 3>(b[0][0], b[0][1], b[1][2])));
    res.insert(point_to_parent(Vector<U, 3>(b[0][0], b[1][1], b[1][2])));
    res.insert(point_to_parent(Vector<U, 3>(b[0][0], b[1][1], b[0][2])));
    res.insert(point_to_parent(Vector<U, 3>(b[1][0], b[1][1], b[0][2])));
    res.insert(point_to_parent(Vector<U, 3>(b[1][0], b[1][1], b[1][2])));
    res.insert(point_to_parent(Vector<U, 3>(b[1][0], b[0][1], b[1][2])));
    res.insert(point_to_parent(Vector<U, 3>(b[1][0], b[0][1], b[0][2])));

    return res;
}

template <typename T>
inline bool operator!=(const Transform<T>& lhs, const Transform<T>& rhs)
{
    return lhs.get_local_to_parent() != rhs.get_local_to_parent();
}

template <typename T>
inline bool operator==(const Transform<T>& lhs, const Transform<T>& rhs)
{
    return lhs.get_local_to_parent() == rhs.get_local_to_parent();
}

template <typename T>
inline bool feq(const Transform<T>& lhs, const Transform<T>& rhs)
{
    return feq(lhs.get_local_to_parent(), rhs.get_local_to_parent());
}

template <typename T>
inline bool feq(const Transform<T>& lhs, const Transform<T>& rhs, const T eps)
{
    return feq(lhs.get_local_to_parent(), rhs.get_local_to_parent(), eps);
}


//
// TransformInterpolator class implementation.
//

template <typename T>
inline TransformInterpolator<T>::TransformInterpolator()
{
}

template <typename T>
inline TransformInterpolator<T>::TransformInterpolator(
    const TransformType& from,
    const TransformType& to)
{
    set_transforms(from, to);
}

template <typename T>
bool TransformInterpolator<T>::set_transforms(
    const TransformType& from,
    const TransformType& to)
{
    from.get_local_to_parent().decompose(m_s0, m_q0, m_t0);
    to.get_local_to_parent().decompose(m_s1, m_q1, m_t1);

    if (dot(m_q0, m_q1) < T(0.0))
        m_q1 = -m_q1;

    const T Eps = make_eps<T>(1.0e-4f, 1.0e-6);

    return is_normalized(m_q0, Eps) && is_normalized(m_q1, Eps);
}

template <typename T>
inline Transform<T> TransformInterpolator<T>::evaluate(const T t) const
{
    // Interpolate the scaling, rotation and translation components.
    const Vector<T, 3> s = lerp(m_s0, m_s1, t);
    const Quaternion<T> q = slerp(m_q0, m_q1, t);
    const Vector<T, 3> p = lerp(m_t0, m_t1, t);

    //
    // Unoptimized implementation:
    //
    //     // Compute the local-to-parent matrix.
    //     const Matrix<T, 4, 4> smat(Matrix<T, 4, 4>::scaling(s));
    //     const Matrix<T, 4, 4> rmat(Matrix<T, 4, 4>::rotation(q));
    //     const Matrix<T, 4, 4> tmat(Matrix<T, 4, 4>::translation(p));
    //     Matrix<T, 4, 4> local_to_parent = smat;
    //     local_to_parent = rmat * local_to_parent;
    //     local_to_parent = tmat * local_to_parent;
    //     
    //     // Compute the parent-to-local matrix.
    //     const Vector<T, 3> inv_s(T(1.0) / s[0], T(1.0) / s[1], T(1.0) / s[2]);
    //     const Matrix<T, 4, 4> inv_smat(Matrix<T, 4, 4>::scaling(inv_s));
    //     const Matrix<T, 4, 4> inv_rmat(transpose(rmat));
    //     const Matrix<T, 4, 4> inv_tmat(Matrix<T, 4, 4>::translation(-p));
    //     Matrix<T, 4, 4> parent_to_local = inv_tmat;
    //     parent_to_local = inv_rmat * parent_to_local;
    //     parent_to_local = inv_smat * parent_to_local;
    //

    Transform<T> result;

    //
    // Compute the local-to-parent matrix.
    //

    const Matrix<T, 3, 3> rmat(Matrix<T, 3, 3>::rotation(q));

    result.m_local_to_parent[ 0] = rmat[0] * s.x;
    result.m_local_to_parent[ 1] = rmat[1] * s.y;
    result.m_local_to_parent[ 2] = rmat[2] * s.z;
    result.m_local_to_parent[ 3] = p.x;

    result.m_local_to_parent[ 4] = rmat[3] * s.x;
    result.m_local_to_parent[ 5] = rmat[4] * s.y;
    result.m_local_to_parent[ 6] = rmat[5] * s.z;
    result.m_local_to_parent[ 7] = p.y;

    result.m_local_to_parent[ 8] = rmat[6] * s.x;
    result.m_local_to_parent[ 9] = rmat[7] * s.y;
    result.m_local_to_parent[10] = rmat[8] * s.z;
    result.m_local_to_parent[11] = p.z;

    result.m_local_to_parent[12] = T(0.0);
    result.m_local_to_parent[13] = T(0.0);
    result.m_local_to_parent[14] = T(0.0);
    result.m_local_to_parent[15] = T(1.0);

    //
    // Compute the local-to-parent matrix.
    //

    const double rcp_sx = T(1.0) / s[0];
    result.m_parent_to_local[ 0] = rmat[0] * rcp_sx;
    result.m_parent_to_local[ 1] = rmat[3] * rcp_sx;
    result.m_parent_to_local[ 2] = rmat[6] * rcp_sx;
    result.m_parent_to_local[ 3] = -(result.m_parent_to_local[ 0] * p.x + result.m_parent_to_local[ 1] * p.y + result.m_parent_to_local[ 2] * p.z);

    const double rcp_sy = T(1.0) / s[1];
    result.m_parent_to_local[ 4] = rmat[1] * rcp_sy;
    result.m_parent_to_local[ 5] = rmat[4] * rcp_sy;
    result.m_parent_to_local[ 6] = rmat[7] * rcp_sy;
    result.m_parent_to_local[ 7] = -(result.m_parent_to_local[ 4] * p.x + result.m_parent_to_local[ 5] * p.y + result.m_parent_to_local[ 6] * p.z);

    const double rcp_sz = T(1.0) / s[2];
    result.m_parent_to_local[ 8] = rmat[2] * rcp_sz;
    result.m_parent_to_local[ 9] = rmat[5] * rcp_sz;
    result.m_parent_to_local[10] = rmat[8] * rcp_sz;
    result.m_parent_to_local[11] = -(result.m_parent_to_local[ 8] * p.x + result.m_parent_to_local[ 9] * p.y + result.m_parent_to_local[10] * p.z);

    result.m_parent_to_local[12] = T(0.0);
    result.m_parent_to_local[13] = T(0.0);
    result.m_parent_to_local[14] = T(0.0);
    result.m_parent_to_local[15] = T(1.0);

    return result;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_TRANSFORM_H
