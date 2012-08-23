//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012 Esteban Tovagliari.
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

#ifndef APPLESEED_PYTHON_UNALIGNED_TRANSFORM44_H
#define APPLESEED_PYTHON_UNALIGNED_TRANSFORM44_H

#include "unaligned_matrix44.h"

#include "foundation/math/transform.h"

namespace foundation
{

template<class T>
class UnalignedTransform44
{
  public:
    typedef UnalignedMatrix44<T> MatrixType;

    UnalignedTransform44() {}

    template<class U>
    explicit UnalignedTransform44(const Transform<U>& xform)
    {
        m_local_to_parent = UnalignedMatrix44<T>(xform.get_local_to_parent());
        m_parent_to_local = UnalignedMatrix44<T>(xform.get_parent_to_local());
    }

    explicit UnalignedTransform44(const MatrixType& local_to_parent)
    {
        m_local_to_parent = UnalignedMatrix44<T>( local_to_parent);
        m_parent_to_local = invert_matrix(m_local_to_parent);
    }

    UnalignedTransform44(const MatrixType& local_to_parent, const MatrixType& parent_to_local)
    {
        Matrix<T, 4, 4> aligned_local_to_parent = local_to_parent.as_foundation_matrix();
        Matrix<T, 4, 4> aligned_parent_to_local = parent_to_local.as_foundation_matrix();

        // check that local_to_parent * aligned_parent_to_local == identity, if not, throw an exception before the assert fires.
        if (!feq(aligned_local_to_parent * aligned_parent_to_local, Matrix<T, 4, 4>::identity(), make_eps<T>(1.0e-6f, 1.0e-9)))
        {
            m_local_to_parent = local_to_parent;
            m_parent_to_local = parent_to_local;
        }
        else
        {
            PyErr_SetString(PyExc_RuntimeError, "Matrices passed to appleseed.Transform are not inverses" );
            boost::python::throw_error_already_set();
        }
    }

    static UnalignedTransform44<T> identity()
    {
        return UnalignedTransform44<T>(MatrixType::identity(), MatrixType::identity());
    }

    template<class U>
    Transform<U> as_foundation_transform() const
    {
        return Transform<U>(get_local_to_parent().as_foundation_matrix(),
                             get_parent_to_local().as_foundation_matrix());
    }

    const MatrixType& get_local_to_parent() const { return m_local_to_parent;}
    const MatrixType& get_parent_to_local() const { return m_parent_to_local;}

    UnalignedTransform44<T> operator*(const UnalignedTransform44<T>& rhs) const
    {
        return UnalignedTransform44<T>(this->as_foundation_transform<T>() * rhs.as_foundation_transform<T>());
    }

    Vector<T, 3> point_to_local(const Vector<T, 3>& p) const
    {
        return m_parent_to_local.transform_point(p);
    }

    Vector<T, 3> point_to_parent(const Vector<T, 3>& p) const
    {
        return m_local_to_parent.transform_point(p);
    }

    Vector<T, 3> vector_to_local(const Vector<T, 3>& v) const
    {
        return m_parent_to_local.transform_vector(v);
    }

    Vector<T, 3> vector_to_parent(const Vector<T, 3>& v) const
    {
        return m_local_to_parent.transform_vector(v);
    }

    Vector<T, 3> normal_to_local(const Vector<T, 3>& n) const
    {
        Vector<T, 3> res;

        res.x = m_local_to_parent[ 0] * n.x +
                m_local_to_parent[ 4] * n.y +
                m_local_to_parent[ 8] * n.z;

        res.y = m_local_to_parent[ 1] * n.x +
                m_local_to_parent[ 5] * n.y +
                m_local_to_parent[ 9] * n.z;

        res.z = m_local_to_parent[ 2] * n.x +
                m_local_to_parent[ 6] * n.y +
                m_local_to_parent[10] * n.z;

        return res;
    }

    Vector<T, 3> normal_to_parent(const Vector<T, 3>& n) const
    {
        Vector<T, 3> res;

        res.x = m_parent_to_local[ 0] * n.x +
                m_parent_to_local[ 4] * n.y +
                m_parent_to_local[ 8] * n.z;

        res.y = m_parent_to_local[ 1] * n.x +
                m_parent_to_local[ 5] * n.y +
                m_parent_to_local[ 9] * n.z;

        res.z = m_parent_to_local[ 2] * n.x +
                m_parent_to_local[ 6] * n.y +
                m_parent_to_local[10] * n.z;

        return res;
    }

  //private:
    MatrixType m_local_to_parent;
    MatrixType m_parent_to_local;
};

template <class T>
std::ostream& operator<<(std::ostream& s, const UnalignedTransform44<T>& xform)
{
    return s << xform.template as_foundation_transform<T>();
}

}       // namespace foundation

#endif  // !APPLESEED_PYTHON_UNALIGNED_TRANSFORM44_H
