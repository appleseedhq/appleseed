
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "array.h"

// appleseed.foundation headers.
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <cstdint>

namespace foundation
{

//
// Array class implementation.
//

template <typename T>
Array::Concept* Array::Model<T>::copy() const
{
    return new Array::Model<T>(*this);
}

template <typename T>
ArrayType Array::Model<T>::type() const
{
    return ArrayTraits<T>::array_type();
}

template <typename T>
size_t Array::Model<T>::item_size() const
{
    return sizeof(T);
}

template <typename T>
bool Array::Model<T>::empty() const
{
    return m_items.empty();
}

template <typename T>
size_t Array::Model<T>::size() const
{
    return m_items.size();
}

template <typename T>
size_t Array::Model<T>::capacity() const
{
    return m_items.capacity();
}

template <typename T>
void Array::Model<T>::clear()
{
    m_items.clear();
}

template <typename T>
void Array::Model<T>::reserve(const size_t n)
{
    m_items.reserve(n);
}

template <typename T>
void Array::Model<T>::resize(const size_t n)
{
    m_items.resize(n);
}

template <typename T>
void Array::Model<T>::shrink_to_fit()
{
    m_items.shrink_to_fit();
}

template <typename T>
void Array::Model<T>::push_back(const void* p)
{
    m_items.push_back(*reinterpret_cast<const T*>(p));
}

template <typename T>
const void* Array::Model<T>::begin() const
{
    return m_items.data();
}

template <typename T>
const void* Array::Model<T>::end() const
{
    return
          reinterpret_cast<const std::int8_t*>(m_items.data())
        + sizeof(T) * m_items.size();
}

template <typename T>
void* Array::Model<T>::begin()
{
    return m_items.data();
}

template <typename T>
void* Array::Model<T>::end()
{
    return
          reinterpret_cast<std::int8_t*>(m_items.data())
        + sizeof(T) * m_items.size();
}

template <typename T>
bool Array::Model<T>::equals(const Concept* rhs) const
{
    const Array::Model<T>* m = reinterpret_cast<const Array::Model<T>*>(rhs);
    return m_items == m->m_items;
}

Array::Array()
  : m_self(nullptr)
{
}

Array::Array(const ArrayType type, const size_t size)
{
#define ARRAY_INIT_CASE(type_enum, type_name) \
    case type_enum: \
        m_self = new Model<type_name>(); \
        break;

    switch (type)
    {
      ARRAY_INIT_CASE(UInt8Type, std::uint8_t)
      ARRAY_INIT_CASE(UInt16Type, std::uint16_t)
      ARRAY_INIT_CASE(UInt32Type, std::uint32_t)
      ARRAY_INIT_CASE(FloatType, float)
      ARRAY_INIT_CASE(Vector2fType, Vector2f)
      ARRAY_INIT_CASE(Vector3fType, Vector3f)
      ARRAY_INIT_CASE(CompressedUnitVectorType, CompressedUnitVector)
      ARRAY_INIT_CASE(Color3fType, Color3f)
      assert_otherwise;
    }

#undef ARRAY_INIT_CASE

    if (size != 0)
        resize(size);
}

Array::~Array()
{
    delete m_self;
}

Array::Array(const Array& rhs)
{
    m_self = rhs.m_self->copy();
}

Array::Array(Array&& rhs) APPLESEED_NOEXCEPT
{
    m_self = rhs.m_self;
    rhs.m_self = nullptr;
}

Array& Array::operator=(const Array& rhs)
{
    Concept* tmp(rhs.m_self->copy());

    delete m_self;
    m_self = tmp;

    return *this;
}

Array& Array::operator=(Array&& rhs) APPLESEED_NOEXCEPT
{
    delete m_self;
    m_self = rhs.m_self;
    rhs.m_self = nullptr;
    return *this;
}

ArrayType Array::type() const
{
    assert(!is_moved());
    return m_self->type();
}

bool Array::empty() const
{
    assert(!is_moved());
    return m_self->empty();
}

size_t Array::size() const
{
    assert(!is_moved());
    return m_self->size();
}

size_t Array::item_size() const
{
    assert(!is_moved());
    return m_self->item_size();
}

size_t Array::capacity() const
{
    assert(!is_moved());
    return m_self->capacity();
}

void Array::clear()
{
    assert(!is_moved());
    m_self->clear();
}

void Array::reserve(const size_t n)
{
    assert(!is_moved());
    m_self->reserve(n);
}

void Array::resize(const size_t n)
{
    assert(!is_moved());
    m_self->resize(n);
}

const void* Array::data() const
{
    assert(!is_moved());
    return begin();
}

void* Array::data()
{
    assert(!is_moved());
    return begin();
}

void Array::shrink_to_fit()
{
    assert(!is_moved());
    m_self->shrink_to_fit();
}

void Array::push_back(const void* p)
{
    assert(!is_moved());
    m_self->push_back(p);
}

const void* Array::begin() const
{
    assert(!is_moved());
    return m_self->begin();
}

const void* Array::end() const
{
    assert(!is_moved());
    return m_self->end();
}

void* Array::begin()
{
    assert(!is_moved());
    return m_self->begin();
}

void* Array::end()
{
    assert(!is_moved());
    return m_self->end();
}

bool Array::operator==(const Array& rhs) const
{
    assert(!is_moved());
    assert(!rhs.is_moved());

    return type() == rhs.type() && m_self->equals(rhs.m_self);
}

bool Array::operator!=(const Array& rhs) const
{
    assert(!is_moved());
    assert(!rhs.is_moved());

    return type() != rhs.type() || !m_self->equals(rhs.m_self);
}

bool Array::is_moved() const
{
    return m_self == nullptr;
}

}   // namespace foundation
