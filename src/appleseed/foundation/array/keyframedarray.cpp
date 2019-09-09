
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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

// Interface header.
#include "keyframedarray.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <utility>

namespace foundation
{

//
// KeyFramedArray class implementation.
//

KeyFramedArray::KeyFramedArray()
  : m_keys(nullptr)
  , m_key_count(0)
{
}

KeyFramedArray::KeyFramedArray(const ArrayType type, const size_t size, const size_t num_keys)
  : m_key_count(num_keys)
{
    m_keys = new Array[num_keys];

    for (size_t i = 0; i < num_keys; ++i)
        m_keys[i] = Array(type, size);
}

KeyFramedArray::KeyFramedArray(Array&& first_key, const size_t num_keys)
  : m_key_count(num_keys)
{
    m_keys = new Array[num_keys];

    for (size_t i = 1; i < num_keys; ++i)
        m_keys[i] = Array(first_key.type(), first_key.size());

    m_keys[0] = std::move(first_key);
}

KeyFramedArray::~KeyFramedArray()
{
    delete[] m_keys;
}

KeyFramedArray::KeyFramedArray(const KeyFramedArray& rhs)
{
    m_key_count = rhs.m_key_count;
    m_keys = new Array[m_key_count];
    std::copy(rhs.begin(), rhs.end(), begin());
}

KeyFramedArray::KeyFramedArray(KeyFramedArray&& rhs) APPLESEED_NOEXCEPT
  : m_keys(rhs.m_keys)
  , m_key_count(rhs.m_key_count)
{
    rhs.m_key_count = 0;
    rhs.m_keys = nullptr;
}

KeyFramedArray& KeyFramedArray::operator=(const KeyFramedArray& rhs)
{
    KeyFramedArray tmp(rhs);
    std::swap(*this, tmp);
    return *this;
}

KeyFramedArray& KeyFramedArray::operator=(KeyFramedArray&& rhs) APPLESEED_NOEXCEPT
{
    KeyFramedArray tmp(std::move(rhs));
    std::swap(m_keys, tmp.m_keys);
    m_key_count = tmp.m_key_count;
    return *this;
}

ArrayType KeyFramedArray::type() const
{
    assert(!is_moved());
    return m_keys[0].type();
}

void KeyFramedArray::resize(const size_t size, const size_t keys)
{
    KeyFramedArray tmp(Array(type(), size), keys);
    std::swap(*this, tmp);
}

const Array* KeyFramedArray::begin() const
{
    return m_keys;
}

const Array* KeyFramedArray::end() const
{
    return m_keys + m_key_count;
}

Array* KeyFramedArray::begin()
{
    return m_keys;
}

Array* KeyFramedArray::end()
{
    return m_keys + m_key_count;
}

size_t KeyFramedArray::get_key_count() const
{
    assert(!is_moved());
    return m_key_count;
}

void KeyFramedArray::set_key_count(const size_t keys)
{
    assert(keys > 0);
    assert(!is_moved());
    KeyFramedArray tmp(std::move(m_keys[0]), keys);
    std::swap(*this, tmp);
}

const Array& KeyFramedArray::get_key(const size_t i) const
{
    assert(!is_moved());
    assert(i < m_key_count);
    return m_keys[i];
}

Array& KeyFramedArray::get_key(const size_t i)
{
    assert(!is_moved());
    assert(i < m_key_count);
    return m_keys[i];
}

void KeyFramedArray::shrink_to_fit()
{
    for (size_t i = 0; i < m_key_count; ++i)
        m_keys[i].shrink_to_fit();
}

bool KeyFramedArray::check_consistency() const
{
    if (m_key_count > 1)
    {
        auto first_type = m_keys[0].type();
        auto first_size = m_keys[0].size();

        for (size_t i = 0; i < m_key_count; ++i)
        {
            if (m_keys[i].type() != first_type)
                return false;

            if (m_keys[i].size() != first_size)
                return false;
        }
    }

    return true;
}

bool KeyFramedArray::all_keyframes_equal() const
{
    if (m_key_count > 1)
    {
        // Check that the keyframes are not empty.
        if (std::all_of(begin(), end(), [](const Array& x) { return x.empty(); }))
            return true;

        // Check that all the keyframe values are equal to the first.
        for (size_t i = 1; i < m_key_count; ++i)
        {
            if (m_keys[i] != m_keys[0])
                return false;
        }
    }

    return true;
}

bool KeyFramedArray::operator==(const KeyFramedArray& rhs) const
{
    assert(!is_moved());
    assert(!rhs.is_moved());

    if (m_key_count != rhs.m_key_count)
        return false;

    return std::equal(begin(), end(), rhs.begin());
}

bool KeyFramedArray::operator!=(const KeyFramedArray& rhs) const
{
    return !(*this == rhs);
}

bool KeyFramedArray::is_moved() const
{
    return m_keys == nullptr;
}

}   // namespace foundation
