
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
    Transform();                            // leave the transformation uninitialized
    explicit Transform(
        const MatrixType& local_to_parent);
    Transform(
        const MatrixType& local_to_parent,
        const MatrixType& parent_to_local);

    // Return the identity transform.
    static Transform identity();

    // Retrieve the transformation matrices.
    const MatrixType& get_local_to_parent() const;
    const MatrixType& get_parent_to_local() const;

    // Compose two transformations.
    TransformType operator*(const TransformType& rhs) const;

    // Transform a 3D point.
    template <typename U>
    Vector<U, 3> transform_point_to_local(const Vector<U, 3>& p) const;
    template <typename U>
    Vector<U, 3> transform_point_to_parent(const Vector<U, 3>& p) const;

    // Transform a 3D vector.
    template <typename U>
    Vector<U, 3> transform_vector_to_local(const Vector<U, 3>& v) const;
    template <typename U>
    Vector<U, 3> transform_vector_to_parent(const Vector<U, 3>& v) const;

    // Transform a 3D normal.
    template <typename U>
    Vector<U, 3> transform_normal_to_local(const Vector<U, 3>& n) const;
    template <typename U>
    Vector<U, 3> transform_normal_to_parent(const Vector<U, 3>& n) const;

    // Transform a 3D ray.
    template <typename U>
    Ray<U, 3> transform_to_local(const Ray<U, 3>& r) const;
    template <typename U>
    Ray<U, 3> transform_to_parent(const Ray<U, 3>& r) const;

    // Transform a 3D axis-aligned bounding box.
    // If the bounding box is invalid, it is returned unmodified.
    template <typename U>
    AABB<U, 3> transform_to_local(const AABB<U, 3>& b) const;
    template <typename U>
    AABB<U, 3> transform_to_parent(const AABB<U, 3>& b) const;

  private:
    MatrixType  m_local_to_parent;
    MatrixType  m_parent_to_local;
};


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
    TransformInterpolator();                // leave the interpolator uninitialized
    TransformInterpolator(
        const TransformType& from,
        const TransformType& to);

    // Set the initial and final transformations.
    void set_transforms(
        const TransformType& from,
        const TransformType& to);

    TransformType evaluate(const T t) const;

  private:
    Vector<T, 3>  m_t0, m_t1;
    Quaternion<T> m_q0, m_q1;
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
}

template <typename T>
inline Transform<T> Transform<T>::identity()
{
    return Transform(MatrixType::identity());
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
    return Transform<T>(
        rhs.m_local_to_parent * m_local_to_parent,
        m_parent_to_local * rhs.m_parent_to_local);
}

template <typename T>
template <typename U>
inline Vector<U, 3> Transform<T>::transform_point_to_local(const Vector<U, 3>& p) const
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
inline Vector<U, 3> Transform<T>::transform_point_to_parent(const Vector<U, 3>& p) const
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
inline Vector<U, 3> Transform<T>::transform_vector_to_local(const Vector<U, 3>& v) const
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
inline Vector<U, 3> Transform<T>::transform_vector_to_parent(const Vector<U, 3>& v) const
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
inline Vector<U, 3> Transform<T>::transform_normal_to_local(const Vector<U, 3>& n) const
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
inline Vector<U, 3> Transform<T>::transform_normal_to_parent(const Vector<U, 3>& n) const
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
inline Ray<U, 3> Transform<T>::transform_to_local(const Ray<U, 3>& r) const
{
    Ray<U, 3> res;
    res.m_org = transform_point_to_local(r.m_org);
    res.m_dir = transform_vector_to_local(r.m_dir);
    res.m_tmin = r.m_tmin;
    res.m_tmax = r.m_tmax;
    return res;
}

template <typename T>
template <typename U>
inline Ray<U, 3> Transform<T>::transform_to_parent(const Ray<U, 3>& r) const
{
    Ray<U, 3> res;
    res.m_org = transform_point_to_parent(r.m_org);
    res.m_dir = transform_vector_to_parent(r.m_dir);
    res.m_tmin = r.m_tmin;
    res.m_tmax = r.m_tmax;
    return res;
}

template <typename T>
template <typename U>
inline AABB<U, 3> Transform<T>::transform_to_local(const AABB<U, 3>& b) const
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
    res.insert(transform_point_to_local(Vector<U, 3>(b[0][0], b[0][1], b[0][2])));
    res.insert(transform_point_to_local(Vector<U, 3>(b[0][0], b[0][1], b[1][2])));
    res.insert(transform_point_to_local(Vector<U, 3>(b[0][0], b[1][1], b[1][2])));
    res.insert(transform_point_to_local(Vector<U, 3>(b[0][0], b[1][1], b[0][2])));
    res.insert(transform_point_to_local(Vector<U, 3>(b[1][0], b[1][1], b[0][2])));
    res.insert(transform_point_to_local(Vector<U, 3>(b[1][0], b[1][1], b[1][2])));
    res.insert(transform_point_to_local(Vector<U, 3>(b[1][0], b[0][1], b[1][2])));
    res.insert(transform_point_to_local(Vector<U, 3>(b[1][0], b[0][1], b[0][2])));
    return res;
}

template <typename T>
template <typename U>
inline AABB<U, 3> Transform<T>::transform_to_parent(const AABB<U, 3>& b) const
{
    if (!b.is_valid())
        return b;

    AABB<U, 3> res;
    res.invalidate();
    res.insert(transform_point_to_parent(Vector<U, 3>(b[0][0], b[0][1], b[0][2])));
    res.insert(transform_point_to_parent(Vector<U, 3>(b[0][0], b[0][1], b[1][2])));
    res.insert(transform_point_to_parent(Vector<U, 3>(b[0][0], b[1][1], b[1][2])));
    res.insert(transform_point_to_parent(Vector<U, 3>(b[0][0], b[1][1], b[0][2])));
    res.insert(transform_point_to_parent(Vector<U, 3>(b[1][0], b[1][1], b[0][2])));
    res.insert(transform_point_to_parent(Vector<U, 3>(b[1][0], b[1][1], b[1][2])));
    res.insert(transform_point_to_parent(Vector<U, 3>(b[1][0], b[0][1], b[1][2])));
    res.insert(transform_point_to_parent(Vector<U, 3>(b[1][0], b[0][1], b[0][2])));
    return res;
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
void TransformInterpolator<T>::set_transforms(
    const TransformType& from,
    const TransformType& to)
{
    const MatrixType& from_matrix = from.get_local_to_parent();
    const MatrixType& to_matrix = to.get_local_to_parent();

    m_t0 = from_matrix.extract_translation();
    m_q0 = from_matrix.extract_unit_quaternion();

    m_t1 = to_matrix.extract_translation();
    m_q1 = to_matrix.extract_unit_quaternion();

    if (m_q0.s * m_q1.s < T(0.0))
        m_q1 = -m_q1;
}

template <typename T>
inline Transform<T> TransformInterpolator<T>::evaluate(const T t) const
{
    const MatrixType translation = MatrixType::translation(lerp(m_t0, m_t1, t));
    const MatrixType rotation = MatrixType::rotation(slerp(m_q0, m_q1, t));

    return
        Transform<T>(
            translation * rotation,
            transpose(rotation) * (-translation));
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_TRANSFORM_H
