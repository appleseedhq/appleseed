
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_UTILITY_IOSTREAMOP_H
#define APPLESEED_FOUNDATION_UTILITY_IOSTREAMOP_H

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/aabb.h"
#include "foundation/math/matrix.h"
#include "foundation/math/quaternion.h"
#include "foundation/math/ray.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cstddef>
#include <iostream>
#include <string>
#include <vector>

namespace std
{

//
// I/O of common standard types from/to C++ streams.
//

// std::vector.
template <typename Type, typename Allocator>
std::ostream& operator<<(std::ostream& s, const std::vector<Type, Allocator>& vector);
template <typename Allocator>
std::ostream& operator<<(std::ostream& s, const std::vector<std::string, Allocator>& vector);
template <typename Allocator>
std::ostream& operator<<(std::ostream& s, const std::vector<char*, Allocator>& vector);
template <typename Allocator>
std::ostream& operator<<(std::ostream& s, const std::vector<const char*, Allocator>& vector);

}   // namespace std


//
// I/O of common appleseed types from/to C++ streams.
//

namespace foundation
{

// foundation::Array.
std::ostream& operator<<(std::ostream& s, const FloatArray& array);
std::istream& operator>>(std::istream& s, FloatArray& array);
std::ostream& operator<<(std::ostream& s, const DoubleArray& array);
std::istream& operator>>(std::istream& s, DoubleArray& array);

// foundation::Vector.
template <typename T, size_t N>
std::ostream& operator<<(std::ostream& s, const Vector<T, N>& vector);
template <typename T, size_t N>
std::istream& operator>>(std::istream& s, Vector<T, N>& vector);

// foundation::AABB.
template <typename T, size_t N>
std::ostream& operator<<(std::ostream& s, const AABB<T, N>& aabb);
template <typename T, size_t N>
std::istream& operator>>(std::istream& s, AABB<T, N>& aabb);

// foundation::Matrix.
template <typename T, size_t M, size_t N>
std::ostream& operator<<(std::ostream& s, const Matrix<T, M, N>& matrix);
template <typename T, size_t M, size_t N>
std::istream& operator>>(std::istream& s, Matrix<T, M, N>& matrix);

// foundation::Quaternion.
template <typename T>
std::ostream& operator<<(std::ostream& s, const Quaternion<T>& quat);
template <typename T>
std::istream& operator>>(std::istream& s, Quaternion<T>& quat);

// foundation::Color.
template <typename T, size_t N>
std::ostream& operator<<(std::ostream& s, const Color<T, N>& color);
template <typename T, size_t N>
std::istream& operator>>(std::istream& s, Color<T, N>& color);

// foundation::RegularSpectrum.
template <typename T, size_t N>
std::ostream& operator<<(std::ostream& s, const RegularSpectrum<T, N>& spectrum);
template <typename T, size_t N>
std::istream& operator>>(std::istream& s, RegularSpectrum<T, N>& spectrum);

// foundation::Ray.
template <typename T, size_t N>
std::ostream& operator<<(std::ostream& s, const Ray<T, N>& ray);
template <typename T, size_t N>
std::istream& operator>>(std::istream& s, Ray<T, N>& ray);

// foundation::Transform.
template <typename T>
std::ostream& operator<<(std::ostream& s, const Transform<T>& transform);

}   // namespace foundation


//
// iostream operators implementation.
//

namespace foundation
{

namespace impl
{
    template <typename Sequence>
    std::ostream& write_sequence(std::ostream& s, const Sequence& sequence, const size_t n)
    {
        if (n > 0)
        {
            for (size_t i = 0; i < n - 1; ++i)
                s << sequence[i] << ' ';
            s << sequence[n - 1];
        }

        return s;
    }

    template <typename Sequence>
    std::ostream& write_sequence_quotes(std::ostream& s, const Sequence& sequence, const size_t n)
    {
        if (n > 0)
        {
            for (size_t i = 0; i < n - 1; ++i)
                s << "\"" << sequence[i] << "\" ";
            s << "\"" << sequence[n - 1] << "\"";
        }

        return s;
    }

    template <typename Sequence>
    std::istream& read_sequence(std::istream& s, Sequence& sequence, const size_t n)
    {
        for (size_t i = 0; i < n; ++i)
            s >> sequence[i];

        return s;
    }
}

}   // namespace foundation

namespace std
{

template <typename Type, typename Allocator>
std::ostream& operator<<(std::ostream& s, const std::vector<Type, Allocator>& vector)
{
    return foundation::impl::write_sequence(s, vector, vector.size());
}

template <typename Allocator>
std::ostream& operator<<(std::ostream& s, const std::vector<std::string, Allocator>& vector)
{
    return foundation::impl::write_sequence_quotes(s, vector, vector.size());
}

template <typename Allocator>
std::ostream& operator<<(std::ostream& s, const std::vector<char*, Allocator>& vector)
{
    return foundation::impl::write_sequence_quotes(s, vector, vector.size());
}

template <typename Allocator>
std::ostream& operator<<(std::ostream& s, const std::vector<const char*, Allocator>& vector)
{
    return foundation::impl::write_sequence_quotes(s, vector, vector.size());
}

}   // namespace std

namespace foundation {

namespace impl
{
    template <typename ArrayType>
    std::istream& read_array(std::istream& s, ArrayType& array)
    {
        std::string token;

        while (s >> token)
            array.push_back(from_string<typename ArrayType::value_type>(token));

        // Clear the fail bit, reaching eof is not an error.
        if (s.eof())
            s.clear(s.rdstate() & ~std::ios::failbit);

        return s;
    }
}

inline std::ostream& operator<<(std::ostream& s, const FloatArray& array)
{
    return impl::write_sequence(s, array, array.size());
}

inline std::istream& operator>>(std::istream& s, FloatArray& array)
{
    return impl::read_array(s, array);
}

inline std::ostream& operator<<(std::ostream& s, const DoubleArray& array)
{
    return impl::write_sequence(s, array, array.size());
}

inline std::istream& operator>>(std::istream& s, DoubleArray& array)
{
    return impl::read_array(s, array);
}

template <typename T, size_t N>
std::ostream& operator<<(std::ostream& s, const Vector<T, N>& vector)
{
    return impl::write_sequence(s, vector, N);
}

template <typename T, size_t N>
std::istream& operator>>(std::istream& s, Vector<T, N>& vector)
{
    return impl::read_sequence(s, vector, N);
}

template <typename T, size_t N>
std::ostream& operator<<(std::ostream& s, const AABB<T, N>& aabb)
{
    return s << aabb.min << ' ' << aabb.max;
}

template <typename T, size_t N>
std::istream& operator>>(std::istream& s, AABB<T, N>& aabb)
{
    s >> aabb.min;
    s >> aabb.max;
    return s;
}

template <typename T, size_t M, size_t N>
std::ostream& operator<<(std::ostream& s, const Matrix<T, M, N>& matrix)
{
    return impl::write_sequence(s, matrix, M * N);
}

template <typename T, size_t M, size_t N>
std::istream& operator>>(std::istream& s, Matrix<T, M, N>& matrix)
{
    return impl::read_sequence(s, matrix, M * N);
}

template <typename T>
std::ostream& operator<<(std::ostream& s, const Quaternion<T>& quat)
{
    s << quat.s << ' ';
    s << quat.v;
    return s;
}

template <typename T>
std::istream& operator>>(std::istream& s, Quaternion<T>& quat)
{
    s >> quat.s;
    s >> quat.v;
    return s;
}

template <typename T, size_t N>
std::ostream& operator<<(std::ostream& s, const Color<T, N>& color)
{
    return impl::write_sequence(s, color, N);
}

template <typename T, size_t N>
std::istream& operator>>(std::istream& s, Color<T, N>& color)
{
    return impl::read_sequence(s, color, N);
}

template <typename T, size_t N>
std::ostream& operator<<(std::ostream& s, const RegularSpectrum<T, N>& spectrum)
{
    return impl::write_sequence(s, spectrum, N);
}

template <typename T, size_t N>
std::istream& operator>>(std::istream& s, RegularSpectrum<T, N>& spectrum)
{
    return impl::read_sequence(s, spectrum, N);
}

template <typename T, size_t N>
std::ostream& operator<<(std::ostream& s, const Ray<T, N>& ray)
{
    s << ray.m_org  << ' ';
    s << ray.m_dir  << ' ';
    s << ray.m_tmin << ' ';
    s << ray.m_tmax;
    return s;
}

template <typename T, size_t N>
std::istream& operator>>(std::istream& s, Ray<T, N>& ray)
{
    s = impl::read_sequence(s, ray.m_org, N);
    s = impl::read_sequence(s, ray.m_dir, N);
    s >> ray.m_tmin;
    s >> ray.m_tmax;
    return s;
}

template <typename T>
std::ostream& operator<<(std::ostream& s, const Transform<T>& transform)
{
    return s << transform.get_local_to_parent();
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_IOSTREAMOP_H
