
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_PYTHON_UNALIGNEDTRANSFORM_H
#define APPLESEED_PYTHON_UNALIGNEDTRANSFORM_H

// appleseed.python headers.
#include "pyseed.h" // has to be first, to avoid redefinition warnings
#include "unalignedmatrix44.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"

namespace foundation
{

template <typename T>
class UnalignedTransform
{
  public:
    UnalignedTransform()
      : m_local_to_parent(UnalignedMatrix44<T>::identity())
      , m_parent_to_local(UnalignedMatrix44<T>::identity())
    {
    }

    explicit UnalignedTransform(const Transform<T>& xform)
      : m_local_to_parent(UnalignedMatrix44<T>(xform.get_local_to_parent()))
      , m_parent_to_local(UnalignedMatrix44<T>(xform.get_parent_to_local()))
    {
    }

    explicit UnalignedTransform(const UnalignedMatrix44<T>& local_to_parent)
      : m_local_to_parent(local_to_parent)
      , m_parent_to_local(invert_matrix(m_local_to_parent))
    {
    }

    template <typename U>
    explicit UnalignedTransform(const UnalignedTransform<U>& other)
      : m_local_to_parent(other.get_local_to_parent())
      , m_parent_to_local(other.get_parent_to_local())
    {
    }

    UnalignedTransform(
        const UnalignedMatrix44<T>& local_to_parent,
        const UnalignedMatrix44<T>& parent_to_local)
    {
        Matrix<T, 4, 4> aligned_local_to_parent = local_to_parent.as_foundation_matrix();
        Matrix<T, 4, 4> aligned_parent_to_local = parent_to_local.as_foundation_matrix();

        // Check that local_to_parent * aligned_parent_to_local == identity, if not, throw an exception before the assert fires.
        if (feq(aligned_local_to_parent * aligned_parent_to_local, Matrix<T, 4, 4>::identity(), make_eps<T>(1.0e-6f, 1.0e-9)))
        {
            m_local_to_parent = UnalignedMatrix44<T>(local_to_parent);
            m_parent_to_local = UnalignedMatrix44<T>(parent_to_local);
        }
        else
        {
            PyErr_SetString(PyExc_RuntimeError, "Matrices passed to appleseed.Transform are not inverses of each other");
            boost::python::throw_error_already_set();
        }
    }

    static UnalignedTransform identity()
    {
        return UnalignedTransform(
            UnalignedMatrix44<T>::identity(),
            UnalignedMatrix44<T>::identity());
    }

    Transform<T> as_foundation_transform() const
    {
        return Transform<T>(
            get_local_to_parent().as_foundation_matrix(),
            get_parent_to_local().as_foundation_matrix());
    }

    const UnalignedMatrix44<T>& get_local_to_parent() const { return m_local_to_parent;}
    const UnalignedMatrix44<T>& get_parent_to_local() const { return m_parent_to_local;}

    UnalignedTransform operator*(const UnalignedTransform& rhs) const
    {
        return UnalignedTransform(this->as_foundation_transform() * rhs.as_foundation_transform());
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
        return m_local_to_parent.transform_normal(n);
    }

    Vector<T, 3> normal_to_parent(const Vector<T, 3>& n) const
    {
        return m_parent_to_local.transform_normal(n);
    }

  private:
    UnalignedMatrix44<T> m_local_to_parent;
    UnalignedMatrix44<T> m_parent_to_local;
};

template <typename T>
inline std::ostream& operator<<(std::ostream& s, const UnalignedTransform<T>& xform)
{
    return s << xform.as_foundation_transform();
}

typedef UnalignedTransform<float>  UnalignedTransformf;
typedef UnalignedTransform<double> UnalignedTransformd;

}       // namespace foundation

#endif  // !APPLESEED_PYTHON_UNALIGNEDTRANSFORM_H
