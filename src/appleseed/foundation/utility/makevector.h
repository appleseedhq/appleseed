
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

// Standard headers.
#include <cassert>
#include <cstdarg>
#include <cstddef>
#include <string>
#include <vector>

namespace foundation
{

//
// Build a std::vector<> out of a set of POD values.
//

std::vector<std::string> make_vector(const char* v1);
std::vector<std::string> make_vector(const char* v1, const char* v2);
std::vector<std::string> make_vector(const char* v1, const char* v2, const char* v3);
std::vector<std::string> make_vector(const char* v1, const char* v2, const char* v3, const char* v4);
std::vector<std::string> make_vector(const char* v1, const char* v2, const char* v3, const char* v4, const char* v5);
std::vector<std::string> make_vector(const char* v1, const char* v2, const char* v3, const char* v4, const char* v5, const char* v6);
std::vector<std::string> make_vector(const char* v1, const char* v2, const char* v3, const char* v4, const char* v5, const char* v6, const char* v7);

template <typename T> std::vector<T> make_vector(const T& v1);
template <typename T> std::vector<T> make_vector(const T& v1, const T& v2);
template <typename T> std::vector<T> make_vector(const T& v1, const T& v2, const T& v3);
template <typename T> std::vector<T> make_vector(const T& v1, const T& v2, const T& v3, const T& v4);
template <typename T> std::vector<T> make_vector(const T& v1, const T& v2, const T& v3, const T& v4, const T& v5);
template <typename T> std::vector<T> make_vector(const T& v1, const T& v2, const T& v3, const T& v4, const T& v5, const T& v6);
template <typename T> std::vector<T> make_vector(const T& v1, const T& v2, const T& v3, const T& v4, const T& v5, const T& v6, const T& v7);

std::vector<std::string> make_vector_n(const size_t n, const char* val, ...);

template <typename T>
std::vector<T> make_vector_n(const size_t n, const T& val, ...);


//
// Implementation.
//

inline std::vector<std::string> make_vector(const char* v1)
{
    std::vector<std::string> vec;
    vec.emplace_back(v1);
    return vec;
}

inline std::vector<std::string> make_vector(const char* v1, const char* v2)
{
    std::vector<std::string> vec;
    vec.emplace_back(v1);
    vec.emplace_back(v2);
    return vec;
}

inline std::vector<std::string> make_vector(const char* v1, const char* v2, const char* v3)
{
    std::vector<std::string> vec;
    vec.emplace_back(v1);
    vec.emplace_back(v2);
    vec.emplace_back(v3);
    return vec;
}

inline std::vector<std::string> make_vector(const char* v1, const char* v2, const char* v3, const char* v4)
{
    std::vector<std::string> vec;
    vec.emplace_back(v1);
    vec.emplace_back(v2);
    vec.emplace_back(v3);
    vec.emplace_back(v4);
    return vec;
}

inline std::vector<std::string> make_vector(const char* v1, const char* v2, const char* v3, const char* v4, const char* v5)
{
    std::vector<std::string> vec;
    vec.emplace_back(v1);
    vec.emplace_back(v2);
    vec.emplace_back(v3);
    vec.emplace_back(v4);
    vec.emplace_back(v5);
    return vec;
}

inline std::vector<std::string> make_vector(const char* v1, const char* v2, const char* v3, const char* v4, const char* v5, const char* v6)
{
    std::vector<std::string> vec;
    vec.emplace_back(v1);
    vec.emplace_back(v2);
    vec.emplace_back(v3);
    vec.emplace_back(v4);
    vec.emplace_back(v5);
    vec.emplace_back(v6);
    return vec;
}

inline std::vector<std::string> make_vector(const char* v1, const char* v2, const char* v3, const char* v4, const char* v5, const char* v6, const char* v7)
{
    std::vector<std::string> vec;
    vec.emplace_back(v1);
    vec.emplace_back(v2);
    vec.emplace_back(v3);
    vec.emplace_back(v4);
    vec.emplace_back(v5);
    vec.emplace_back(v6);
    vec.emplace_back(v7);
    return vec;
}

template <typename T>
std::vector<T> make_vector(const T& v1)
{
    std::vector<T> vec;
    vec.push_back(v1);
    return vec;
}

template <typename T>
std::vector<T> make_vector(const T& v1, const T& v2)
{
    std::vector<T> vec;
    vec.push_back(v1);
    vec.push_back(v2);
    return vec;
}

template <typename T>
std::vector<T> make_vector(const T& v1, const T& v2, const T& v3)
{
    std::vector<T> vec;
    vec.push_back(v1);
    vec.push_back(v2);
    vec.push_back(v3);
    return vec;
}

template <typename T>
std::vector<T> make_vector(const T& v1, const T& v2, const T& v3, const T& v4)
{
    std::vector<T> vec;
    vec.push_back(v1);
    vec.push_back(v2);
    vec.push_back(v3);
    vec.push_back(v4);
    return vec;
}

template <typename T>
std::vector<T> make_vector(const T& v1, const T& v2, const T& v3, const T& v4, const T& v5)
{
    std::vector<T> vec;
    vec.push_back(v1);
    vec.push_back(v2);
    vec.push_back(v3);
    vec.push_back(v4);
    vec.push_back(v5);
    return vec;
}

template <typename T>
std::vector<T> make_vector(const T& v1, const T& v2, const T& v3, const T& v4, const T& v5, const T& v6)
{
    std::vector<T> vec;
    vec.push_back(v1);
    vec.push_back(v2);
    vec.push_back(v3);
    vec.push_back(v4);
    vec.push_back(v5);
    vec.push_back(v6);
    return vec;
}

template <typename T>
std::vector<T> make_vector(const T& v1, const T& v2, const T& v3, const T& v4, const T& v5, const T& v6, const T& v7)
{
    std::vector<T> vec;
    vec.push_back(v1);
    vec.push_back(v2);
    vec.push_back(v3);
    vec.push_back(v4);
    vec.push_back(v5);
    vec.push_back(v6);
    vec.push_back(v7);
    return vec;
}

inline std::vector<std::string> make_vector_n(const size_t n, const char* val, ...)
{
    assert(n > 0);

    std::vector<std::string> vec;
    vec.emplace_back(val);

    va_list argptr;
    va_start(argptr, val);

    for (size_t i = 1; i < n; ++i)
        vec.emplace_back(va_arg(argptr, char*));

    va_end(argptr);

    return vec;
}

template <typename T>
std::vector<T> make_vector_n(const size_t n, const T& val, ...)
{
    assert(n > 0);

    std::vector<T> vec;
    vec.push_back(val);

    va_list argptr;
    va_start(argptr, val);

    for (size_t i = 1; i < n; ++i)
        vec.push_back(va_arg(argptr, T));

    va_end(argptr);

    return vec;
}

}   // namespace foundation
