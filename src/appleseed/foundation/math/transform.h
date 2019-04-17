
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
#include "foundation/math/aabb.h"
#include "foundation/math/matrix.h"
#include "foundation/math/quaternion.h"
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/poison.h"

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
    // Matrix, vector and transform types.
    typedef Matrix<T, 4, 4> MatrixType;
    typedef Vector<T, 3> VectorType;
    typedef Transform<T> TransformType;

    // Constructors.
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Transform() = default;                      // leave the transformation uninitialized
#else
    Transform() {}                              // leave the transformation uninitialized
#endif
    explicit Transform(                         // throws a foundation::ExceptionSingularMatrix exception if local_to_parent is singular
        const MatrixType& local_to_parent);
    Transform(
        const MatrixType& local_to_parent,
        const MatrixType& parent_to_local);     // must be equal to inverse(local_to_parent)

    // Construct and return the identity transform.
    static TransformType make_identity();

    // Return the identity transform.
    static const Transform& identity();

    // Build a transform using a given local-to-parent transform.
    // The parent-to-local transform will be computed using matrix inversion.
    static Transform from_local_to_parent(const MatrixType& m);

    // Set the transformation matrices.
    void set_local_to_parent(const MatrixType& m);
    void set_parent_to_local(const MatrixType& m);

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

    // Retrieve the origin in parent space.
    VectorType get_parent_origin() const;

    // Retrieve the primary axes in parent space.
    VectorType get_parent_x() const;
    VectorType get_parent_y() const;
    VectorType get_parent_z() const;

    // Returns true if the transform swaps the handedness.
    bool swaps_handedness() const;

  private:
    template <typename> friend class TransformInterpolator;
    template <typename> friend class PoisonImpl;

    MatrixType  m_local_to_parent;
    MatrixType  m_parent_to_local;

    // The identity transform returned by identity().
    static const TransformType m_identity;
};

// Poisoning.
template <typename T>
class PoisonImpl<Transform<T>>
{
  public:
    static void do_poison(Transform<T>& transform);
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
// The TransformInterpolator class allows to interpolate between two rigid
// transformations (any combination of rotations, translations and scalings).
//

template <typename T>
class TransformInterpolator
{
  public:
    // Matrix and transform types.
    typedef Matrix<T, 4, 4> MatrixType;
    typedef Transform<T> TransformType;

    // Constructors.
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    TransformInterpolator() = default;          // leave the interpolator uninitialized
#else
    TransformInterpolator() {}                  // leave the interpolator uninitialized
#endif
    TransformInterpolator(
        const TransformType&    from,
        const TransformType&    to);

    // Set the initial and final transformations.
    // Returns true on success, false otherwise.
    bool set_transforms(
        const TransformType&    from,
        const TransformType&    to);

    // Return the start and end scaling, rotation and translation.
    const Vector<T, 3>& get_s0() const;
    const Vector<T, 3>& get_s1() const;
    const Quaternion<T>& get_q0() const;
    const Quaternion<T>& get_q1() const;
    const Vector<T, 3>& get_t0() const;
    const Vector<T, 3>& get_t1() const;

    // Compute the transform for any value of the interpolation parameter t.
    // Returns the initial transform for t == 0 and the final transform for t == 1.
    void evaluate(
        const T                 t,
        TransformType&          result) const;

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
const Transform<T> Transform<T>::m_identity(Transform<T>::make_identity());

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
    assert(
        feq(
            m_local_to_parent * m_parent_to_local,
            MatrixType::make_identity(),
            make_eps<T>(1.0e-4f, 1.0e-6)));
}

template <typename T>
void PoisonImpl<Transform<T>>::do_poison(Transform<T>& transform)
{
    always_poison(transform.m_local_to_parent);
    always_poison(transform.m_parent_to_local);
}

template <typename T>
Transform<T> Transform<T>::make_identity()
{
    return Transform(MatrixType::make_identity(), MatrixType::make_identity());
}

template <typename T>
inline const Transform<T>& Transform<T>::identity()
{
    return m_identity;
}

template <typename T>
inline Transform<T> Transform<T>::from_local_to_parent(const MatrixType& m)
{
    return Transform(m, inverse(m));
}

template <typename T>
inline void Transform<T>::set_local_to_parent(const MatrixType& m)
{
    m_local_to_parent = m;
}

template <typename T>
inline void Transform<T>::set_parent_to_local(const MatrixType& m)
{
    m_parent_to_local = m;
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
inline typename Transform<T>::VectorType Transform<T>::get_parent_origin() const
{
    VectorType res(
        m_local_to_parent[ 3],
        m_local_to_parent[ 7],
        m_local_to_parent[11]);

    const T w = m_local_to_parent[15];

    assert(w != T(0.0));

    if (w != T(1.0))
        res /= w;

    return res;
}

template <typename T>
inline typename Transform<T>::VectorType Transform<T>::get_parent_x() const
{
    return
        VectorType(
            m_local_to_parent[ 0],
            m_local_to_parent[ 4],
            m_local_to_parent[ 8]);
}

template <typename T>
inline typename Transform<T>::VectorType Transform<T>::get_parent_y() const
{
    return
        VectorType(
            m_local_to_parent[ 1],
            m_local_to_parent[ 5],
            m_local_to_parent[ 9]);
}

template <typename T>
inline typename Transform<T>::VectorType Transform<T>::get_parent_z() const
{
    return
        VectorType(
            m_local_to_parent[ 2],
            m_local_to_parent[ 6],
            m_local_to_parent[10]);
}

template <typename T>
inline bool Transform<T>::swaps_handedness() const
{
    // We can test any of the matrices because if a matrix swaps the handedness, its inverse does too.
    // gcc 4.8 needs the extra foundation qualifier.
    return foundation::swaps_handedness(m_local_to_parent);
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

    // Handle double-cover.
    if (dot(m_q0, m_q1) < T(0.0))
        m_q1 = -m_q1;

    const T Eps = make_eps<T>(1.0e-4f, 1.0e-6);
    return is_normalized(m_q0, Eps) && is_normalized(m_q1, Eps);
}

template <typename T>
inline const Vector<T, 3>& TransformInterpolator<T>::get_s0() const
{
    return m_s0;
}

template <typename T>
inline const Vector<T, 3>& TransformInterpolator<T>::get_s1() const
{
    return m_s1;
}

template <typename T>
inline const Quaternion<T>& TransformInterpolator<T>::get_q0() const
{
    return m_q0;
}

template <typename T>
inline const Quaternion<T>& TransformInterpolator<T>::get_q1() const
{
    return m_q1;
}

template <typename T>
inline const Vector<T, 3>& TransformInterpolator<T>::get_t0() const
{
    return m_t0;
}

template <typename T>
inline const Vector<T, 3>& TransformInterpolator<T>::get_t1() const
{
    return m_t1;
}

template <typename T>
inline void TransformInterpolator<T>::evaluate(const T t, Transform<T>& result) const
{
    //
    // Unoptimized implementation:
    //
    //     // Interpolate the scaling, rotation and translation components.
    //     const Vector<T, 3> s = lerp(m_s0, m_s1, t);
    //     const Quaternion<T> q = slerp(m_q0, m_q1, t);
    //     const Vector<T, 3> p = lerp(m_t0, m_t1, t);
    //
    //     // Compute the local-to-parent matrix.
    //     const Matrix<T, 4, 4> smat(Matrix<T, 4, 4>::make_scaling(s));
    //     const Matrix<T, 4, 4> rmat(Matrix<T, 4, 4>::make_rotation(q));
    //     const Matrix<T, 4, 4> tmat(Matrix<T, 4, 4>::make_translation(p));
    //     Matrix<T, 4, 4> local_to_parent = smat;
    //     local_to_parent = rmat * local_to_parent;
    //     local_to_parent = tmat * local_to_parent;
    //
    //     // Compute the parent-to-local matrix.
    //     const Vector<T, 3> inv_s(T(1.0) / s[0], T(1.0) / s[1], T(1.0) / s[2]);
    //     const Matrix<T, 4, 4> inv_smat(Matrix<T, 4, 4>::make_scaling(inv_s));
    //     const Matrix<T, 4, 4> inv_rmat(transpose(rmat));
    //     const Matrix<T, 4, 4> inv_tmat(Matrix<T, 4, 4>::make_translation(-p));
    //     Matrix<T, 4, 4> parent_to_local = inv_tmat;
    //     parent_to_local = inv_rmat * parent_to_local;
    //     parent_to_local = inv_smat * parent_to_local;
    //

    const Quaternion<T> q = fast_slerp(m_q0, m_q1, t);

    const T rtx = q.v[0] + q.v[0];
    const T rty = q.v[1] + q.v[1];
    const T rtz = q.v[2] + q.v[2];
    const T twx = rtx * q.s;
    const T twy = rty * q.s;
    const T twz = rtz * q.s;
    const T txx = rtx * q.v[0];
    const T txy = rty * q.v[0];
    const T txz = rtz * q.v[0];
    const T tyy = rty * q.v[1];
    const T tyz = rtz * q.v[1];
    const T tzz = rtz * q.v[2];

    result.m_local_to_parent[ 0] = result.m_parent_to_local[ 0] = T(1.0) - (tyy + tzz);
    result.m_local_to_parent[ 4] = result.m_parent_to_local[ 1] = txy + twz;
    result.m_local_to_parent[ 8] = result.m_parent_to_local[ 2] = txz - twy;
    result.m_local_to_parent[ 1] = result.m_parent_to_local[ 4] = txy - twz;
    result.m_local_to_parent[ 5] = result.m_parent_to_local[ 5] = T(1.0) - (txx + tzz);
    result.m_local_to_parent[ 9] = result.m_parent_to_local[ 6] = tyz + twx;
    result.m_local_to_parent[ 2] = result.m_parent_to_local[ 8] = txz + twy;
    result.m_local_to_parent[ 6] = result.m_parent_to_local[ 9] = tyz - twx;
    result.m_local_to_parent[10] = result.m_parent_to_local[10] = T(1.0) - (txx + tyy);

    const Vector<T, 3> s = lerp(m_s0, m_s1, t);

    result.m_local_to_parent[ 0] *= s.x;
    result.m_local_to_parent[ 4] *= s.x;
    result.m_local_to_parent[ 8] *= s.x;

    result.m_local_to_parent[ 1] *= s.y;
    result.m_local_to_parent[ 5] *= s.y;
    result.m_local_to_parent[ 9] *= s.y;

    result.m_local_to_parent[ 2] *= s.z;
    result.m_local_to_parent[ 6] *= s.z;
    result.m_local_to_parent[10] *= s.z;

    const T rcp_sx = T(1.0) / s[0];
    result.m_parent_to_local[ 0] *= rcp_sx;
    result.m_parent_to_local[ 1] *= rcp_sx;
    result.m_parent_to_local[ 2] *= rcp_sx;

    const T rcp_sy = T(1.0) / s[1];
    result.m_parent_to_local[ 4] *= rcp_sy;
    result.m_parent_to_local[ 5] *= rcp_sy;
    result.m_parent_to_local[ 6] *= rcp_sy;

    const T rcp_sz = T(1.0) / s[2];
    result.m_parent_to_local[ 8] *= rcp_sz;
    result.m_parent_to_local[ 9] *= rcp_sz;
    result.m_parent_to_local[10] *= rcp_sz;

    const Vector<T, 3> p = lerp(m_t0, m_t1, t);

    result.m_parent_to_local[ 3] = -(result.m_parent_to_local[ 0] * p.x + result.m_parent_to_local[ 1] * p.y + result.m_parent_to_local[ 2] * p.z);
    result.m_local_to_parent[ 3] = p.x;
    result.m_parent_to_local[ 7] = -(result.m_parent_to_local[ 4] * p.x + result.m_parent_to_local[ 5] * p.y + result.m_parent_to_local[ 6] * p.z);
    result.m_local_to_parent[ 7] = p.y;
    result.m_parent_to_local[11] = -(result.m_parent_to_local[ 8] * p.x + result.m_parent_to_local[ 9] * p.y + result.m_parent_to_local[10] * p.z);
    result.m_local_to_parent[11] = p.z;

    result.m_local_to_parent[12] = result.m_parent_to_local[12] = T(0.0);
    result.m_local_to_parent[13] = result.m_parent_to_local[13] = T(0.0);
    result.m_local_to_parent[14] = result.m_parent_to_local[14] = T(0.0);
    result.m_local_to_parent[15] = result.m_parent_to_local[15] = T(1.0);
}

}   // namespace foundation
