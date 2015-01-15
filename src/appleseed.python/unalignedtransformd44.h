
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_PYTHON_UNALIGNEDTRANSFORMD44_H
#define APPLESEED_PYTHON_UNALIGNEDTRANSFORMD44_H

// appleseed.python headers.
#include "pyseed.h" // has to be first, to avoid redefinition warnings
#include "unalignedmatrix44.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"

namespace foundation
{

class UnalignedTransformd44
{
  public:
    UnalignedTransformd44() {}

    template <class T>
    explicit UnalignedTransformd44(const Transform<T>& xform)
      : m_local_to_parent(UnalignedMatrix44<double>(xform.get_local_to_parent()))
      , m_parent_to_local(UnalignedMatrix44<double>(xform.get_parent_to_local()))
    {
    }

    template <class T>
    explicit UnalignedTransformd44(const UnalignedMatrix44<T>& local_to_parent)
      : m_local_to_parent(local_to_parent)
      , m_parent_to_local(invert_matrix(m_local_to_parent))
    {
    }

    template <class T>
    UnalignedTransformd44(const UnalignedMatrix44<T>& local_to_parent,
                          const UnalignedMatrix44<T>& parent_to_local)
    {
        Matrix<T, 4, 4> aligned_local_to_parent = local_to_parent.as_foundation_matrix();
        Matrix<T, 4, 4> aligned_parent_to_local = parent_to_local.as_foundation_matrix();

        // Check that local_to_parent * aligned_parent_to_local == identity, if not, throw an exception before the assert fires.
        if (feq(aligned_local_to_parent * aligned_parent_to_local, Matrix<T, 4, 4>::identity(), make_eps<T>(1.0e-6f, 1.0e-9)))
        {
            m_local_to_parent = UnalignedMatrix44<double>(local_to_parent);
            m_parent_to_local = UnalignedMatrix44<double>(parent_to_local);
        }
        else
        {
            PyErr_SetString(PyExc_RuntimeError, "Matrices passed to appleseed.Transform are not inverses of each other");
            boost::python::throw_error_already_set();
        }
    }

    static UnalignedTransformd44 identity()
    {
        return UnalignedTransformd44(UnalignedMatrix44<double>::identity(),
                                     UnalignedMatrix44<double>::identity());
    }

    Transform<double> as_foundation_transform() const
    {
        return Transform<double>(get_local_to_parent().as_foundation_matrix(),
                                 get_parent_to_local().as_foundation_matrix());
    }

    const UnalignedMatrix44<double>& get_local_to_parent() const { return m_local_to_parent;}
    const UnalignedMatrix44<double>& get_parent_to_local() const { return m_parent_to_local;}

    UnalignedTransformd44 operator*(const UnalignedTransformd44& rhs) const
    {
        return UnalignedTransformd44(this->as_foundation_transform() * rhs.as_foundation_transform());
    }

    template <class T>
    Vector<T, 3> point_to_local(const Vector<T, 3>& p) const
    {
        return m_parent_to_local.transform_point(p);
    }

    template <class T>
    Vector<T, 3> point_to_parent(const Vector<T, 3>& p) const
    {
        return m_local_to_parent.transform_point(p);
    }

    template <class T>
    Vector<T, 3> vector_to_local(const Vector<T, 3>& v) const
    {
        return m_parent_to_local.transform_vector(v);
    }

    template <class T>
    Vector<T, 3> vector_to_parent(const Vector<T, 3>& v) const
    {
        return m_local_to_parent.transform_vector(v);
    }

    template <class T>
    Vector<T, 3> normal_to_local(const Vector<T, 3>& n) const
    {
        Vector<T, 3> res;

        res.x = static_cast<T>(m_local_to_parent[ 0] * n.x +
                               m_local_to_parent[ 4] * n.y +
                               m_local_to_parent[ 8] * n.z);

        res.y = static_cast<T>(m_local_to_parent[ 1] * n.x +
                               m_local_to_parent[ 5] * n.y +
                               m_local_to_parent[ 9] * n.z);

        res.z = static_cast<T>(m_local_to_parent[ 2] * n.x +
                               m_local_to_parent[ 6] * n.y +
                               m_local_to_parent[10] * n.z);

        return res;
    }

    template <class T>
    Vector<T, 3> normal_to_parent(const Vector<T, 3>& n) const
    {
        Vector<T, 3> res;

        res.x = static_cast<T>(m_parent_to_local[ 0] * n.x +
                               m_parent_to_local[ 4] * n.y +
                               m_parent_to_local[ 8] * n.z);

        res.y = static_cast<T>(m_parent_to_local[ 1] * n.x +
                               m_parent_to_local[ 5] * n.y +
                               m_parent_to_local[ 9] * n.z);

        res.z = static_cast<T>(m_parent_to_local[ 2] * n.x +
                               m_parent_to_local[ 6] * n.y +
                               m_parent_to_local[10] * n.z);

        return res;
    }

  private:
    UnalignedMatrix44<double> m_local_to_parent;
    UnalignedMatrix44<double> m_parent_to_local;
};

inline std::ostream& operator<<(std::ostream& s, const UnalignedTransformd44& xform)
{
    return s << xform.as_foundation_transform();
}

}       // namespace foundation

#endif  // !APPLESEED_PYTHON_UNALIGNEDTRANSFORMD44_H
