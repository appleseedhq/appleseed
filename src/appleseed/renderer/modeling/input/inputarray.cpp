
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

// Interface header.
#include "inputarray.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <cassert>
#include <cstring>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// InputArray class implementation.
//

struct InputArray::Impl
{
    struct InputDecl
    {
        string          m_name;
        InputFormat     m_format;
        bool            m_has_default_value;
        string          m_default_value;
        Source*         m_source;
    };

    typedef vector<InputDecl> InputDeclVector;

    InputDeclVector m_input_decls;
};

InputArray::InputArray()
  : impl(new Impl())
{
}

InputArray::~InputArray()
{
    for (each<Impl::InputDeclVector> i = impl->m_input_decls; i; ++i)
        delete i->m_source;

    delete impl;
}

void InputArray::declare(
    const char*         name,
    const InputFormat   format,
    const char*         default_value)
{
    assert(name);

    Impl::InputDecl decl;
    decl.m_name = name;
    decl.m_format = format;
    decl.m_has_default_value = default_value != 0;
    if (default_value)
        decl.m_default_value = default_value;
    decl.m_source = 0;

    impl->m_input_decls.push_back(decl);
}

InputArray::iterator InputArray::begin()
{
    return iterator(this, 0);
}

InputArray::iterator InputArray::end()
{
    return iterator(this, impl->m_input_decls.size());
}

InputArray::const_iterator InputArray::begin() const
{
    return const_iterator(this, 0);
}

InputArray::const_iterator InputArray::end() const
{
    return const_iterator(this, impl->m_input_decls.size());
}

InputArray::iterator InputArray::find(const char* name)
{
    assert(name);

    const size_t input_count = impl->m_input_decls.size();

    for (size_t i = 0; i < input_count; ++i)
    {
        if (strcmp(impl->m_input_decls[i].m_name.c_str(), name) == 0)
            return iterator(this, i);
    }

    return end();
}

InputArray::const_iterator InputArray::find(const char* name) const
{
    assert(name);

    const size_t input_count = impl->m_input_decls.size();

    for (size_t i = 0; i < input_count; ++i)
    {
        if (strcmp(impl->m_input_decls[i].m_name.c_str(), name) == 0)
            return const_iterator(this, i);
    }

    return end();
}

Source* InputArray::source(const char* name) const
{
    assert(name);

    for (const_each<Impl::InputDeclVector> i = impl->m_input_decls; i; ++i)
    {
        if (strcmp(i->m_name.c_str(), name) == 0)
            return i->m_source;
    }

    return 0;
}

namespace
{
    // Not the same as ALIGNOF(), see http://stackoverflow.com/q/11545153/393756.
    template <typename Target>
    size_t offset_of()
    {
        struct S { char c; Target t; };
        return offsetof(S, t);
    }

    // Can't use offsetof() on non-POD types.
    template <>
    size_t offset_of<Spectrum>()
    {
        return ALIGNOF(Spectrum);
    }

    template <typename Target, typename T>
    T align_to(const T x)
    {
        return align(x, offset_of<Target>());
    }
}

size_t InputArray::compute_data_size() const
{
    size_t size = 0;

    for (const_each<Impl::InputDeclVector> i = impl->m_input_decls; i; ++i)
    {
        switch (i->m_format)
        {
          case InputFormatScalar:
            size = align_to<double>(size);
            size += sizeof(double);
            break;

          case InputFormatSpectrum:
            size = align_to<Spectrum>(size);
            size += sizeof(Spectrum);
            size += sizeof(Alpha);
            break;
        }
    }

    size = align(size, 16);

    return size;
}

void InputArray::evaluate(
    TextureCache&       texture_cache,
    const Vector2d&     uv,
    void*               values,
    const size_t        offset) const
{
    assert(values);

    uint8* ptr = static_cast<uint8*>(values) + offset;
    assert(is_aligned(ptr, 16));

    for (const_each<Impl::InputDeclVector> i = impl->m_input_decls; i; ++i)
    {
        switch (i->m_format)
        {
          case InputFormatScalar:
            ptr = align_to<double>(ptr);
            if (i->m_source)
            {
                i->m_source->evaluate(
                    texture_cache,
                    uv,
                    *reinterpret_cast<double*>(ptr));
            }
            ptr += sizeof(double);
            break;

          case InputFormatSpectrum:
            ptr = align_to<Spectrum>(ptr);
            if (i->m_source)
            {
                i->m_source->evaluate(
                    texture_cache,
                    uv,
                    *reinterpret_cast<Spectrum*>(ptr),
                    *reinterpret_cast<Alpha*>(ptr + sizeof(Spectrum)));
            }
            ptr += sizeof(Spectrum);
            ptr += sizeof(Alpha);
            break;
        }
    }
}

void InputArray::evaluate_uniforms(
    void*               values,
    const size_t        offset) const
{
    assert(values);

    uint8* ptr = static_cast<uint8*>(values) + offset;
    assert(is_aligned(ptr, 16));

    for (const_each<Impl::InputDeclVector> i = impl->m_input_decls; i; ++i)
    {
        switch (i->m_format)
        {
          case InputFormatScalar:
            ptr = align_to<double>(ptr);
            if (i->m_source && i->m_source->is_uniform())
                i->m_source->evaluate_uniform(*reinterpret_cast<double*>(ptr));
            ptr += sizeof(double);
            break;

          case InputFormatSpectrum:
            ptr = align_to<Spectrum>(ptr);
            if (i->m_source && i->m_source->is_uniform())
            {
                i->m_source->evaluate_uniform(
                    *reinterpret_cast<Spectrum*>(ptr),
                    *reinterpret_cast<Alpha*>(ptr + sizeof(Spectrum)));
            }
            ptr += sizeof(Spectrum);
            ptr += sizeof(Alpha);
            break;
        }
    }
}


//
// InputArray::const_iterator class implementation.
//

InputArray::const_iterator::const_iterator(const InputArray* array, const size_t index)
  : m_input_array(array)
  , m_input_index(index)
{
}

InputArray::const_iterator::const_iterator(const iterator& rhs)
  : m_input_array(rhs.m_input_array)
  , m_input_index(rhs.m_input_index)
{
}

InputArray::const_iterator::const_iterator(const const_iterator& rhs)
  : m_input_array(rhs.m_input_array)
  , m_input_index(rhs.m_input_index)
{
}

InputArray::const_iterator& InputArray::const_iterator::operator=(const const_iterator& rhs)
{
    m_input_array = rhs.m_input_array;
    m_input_index = rhs.m_input_index;
    return *this;
}

bool InputArray::const_iterator::operator==(const const_iterator& rhs) const
{
    return
        m_input_index == rhs.m_input_index &&
        m_input_array == rhs.m_input_array;
}

bool InputArray::const_iterator::operator!=(const const_iterator& rhs) const
{
    return
        m_input_index != rhs.m_input_index ||
        m_input_array != rhs.m_input_array;
}

InputArray::const_iterator& InputArray::const_iterator::operator++()
{
    ++m_input_index;
    return *this;
}

InputArray::const_iterator& InputArray::const_iterator::operator--()
{
    --m_input_index;
    return *this;
}

const InputArray::const_iterator& InputArray::const_iterator::operator*() const
{
    return *this;
}

const char* InputArray::const_iterator::name() const
{
    return m_input_array->impl->m_input_decls[m_input_index].m_name.c_str();
}

InputFormat InputArray::const_iterator::format() const
{
    return m_input_array->impl->m_input_decls[m_input_index].m_format;
}

const char* InputArray::const_iterator::default_value() const
{
    const Impl::InputDecl& decl = m_input_array->impl->m_input_decls[m_input_index];
    return decl.m_has_default_value ? decl.m_default_value.c_str() : 0;
}

Source* InputArray::const_iterator::source() const
{
    return m_input_array->impl->m_input_decls[m_input_index].m_source;
}


//
// InputArray::iterator class implementation.
//

InputArray::iterator::iterator(const InputArray* array, const size_t index)
  : const_iterator(array, index)
{
}

InputArray::iterator::iterator(const iterator& rhs)
  : const_iterator(rhs)
{
}

InputArray::iterator& InputArray::iterator::operator=(const iterator& rhs)
{
    m_input_array = rhs.m_input_array;
    m_input_index = rhs.m_input_index;
    return *this;
}

InputArray::iterator& InputArray::iterator::operator++()
{
    ++m_input_index;
    return *this;
}

InputArray::iterator& InputArray::iterator::operator--()
{
    --m_input_index;
    return *this;
}

InputArray::iterator& InputArray::iterator::operator*()
{
    return *this;
}

void InputArray::iterator::bind(Source* source)
{
    Impl::InputDecl& decl = m_input_array->impl->m_input_decls[m_input_index];
    delete decl.m_source;
    decl.m_source = source;
}

}   // namespace renderer
