
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/memory.h"

// Standard headers.
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
        bool            m_is_optional;
        Source*         m_source;
    };

    typedef vector<InputDecl> InputDeclVector;

    InputDeclVector m_input_decls;
};

// Constructor.
InputArray::InputArray()
  : impl(new Impl())
{
}

// Destructor.
InputArray::~InputArray()
{
    for (each<Impl::InputDeclVector> i = impl->m_input_decls; i; ++i)
        delete i->m_source;

    delete impl;
}

// Declare an input.
void InputArray::declare(
    const char*         name,
    const InputFormat   format,
    const bool          is_optional)
{
    assert(name);

    Impl::InputDecl decl;
    decl.m_name = name;
    decl.m_format = format;
    decl.m_is_optional = is_optional;
    decl.m_source = 0;

    impl->m_input_decls.push_back(decl);
}

// Return an iterator to the first entry.
InputArray::iterator InputArray::begin() const
{
    return iterator(this, 0);
}

// Return an iterator one beyond the last entry.
InputArray::iterator InputArray::end() const
{
    return iterator(this, impl->m_input_decls.size());
}

// Find a given input.
InputArray::iterator InputArray::find(const char* name) const
{
    assert(name);

    const size_t input_count = impl->m_input_decls.size();

    for (size_t i = 0; i < input_count; ++i)
    {
        if (impl->m_input_decls[i].m_name == name)
            return iterator(this, i);
    }

    return end();
}

// Get the source bound to a given input.
Source* InputArray::source(const char* name) const
{
    assert(name);

    for (const_each<Impl::InputDeclVector> i = impl->m_input_decls; i; ++i)
    {
        if (i->m_name == name)
            return i->m_source;
    }

    return 0;
}

// Evaluate all inputs into a preallocated block of memory.
void InputArray::evaluate(
    TextureCache&       texture_cache,
    const InputParams&  params,
    void*               values) const
{
    assert(values);

    uint8* ptr = static_cast<uint8*>(values);

    for (const_each<Impl::InputDeclVector> i = impl->m_input_decls; i; ++i)
    {
        switch (i->m_format)
        {
          case InputFormatScalar:
            ptr = align(ptr, 8);
            if (i->m_source)
            {
                i->m_source->evaluate(
                    texture_cache,
                    params,
                    *reinterpret_cast<double*>(ptr));
            }
            ptr += sizeof(double);
            break;

          case InputFormatSpectrum:
            ptr = align(ptr, 16);
            if (i->m_source)
            {
                i->m_source->evaluate(
                    texture_cache,
                    params,
                    *reinterpret_cast<Spectrum*>(ptr),
                    *reinterpret_cast<Alpha*>(ptr + sizeof(Spectrum)));
            }
            ptr += sizeof(Spectrum);
            ptr += sizeof(Alpha);
            break;
        }
    }
}

// Evaluate all uniform inputs.
void InputArray::evaluate_uniforms(void* values) const
{
    assert(values);

    uint8* ptr = static_cast<uint8*>(values);

    for (const_each<Impl::InputDeclVector> i = impl->m_input_decls; i; ++i)
    {
        switch (i->m_format)
        {
          case InputFormatScalar:
            ptr = align(ptr, 8);
            if (i->m_source &&
                i->m_source->is_uniform())
                i->m_source->evaluate_uniform(*reinterpret_cast<double*>(ptr));
            ptr += sizeof(double);
            break;

          case InputFormatSpectrum:
            ptr = align(ptr, 16);
            if (i->m_source &&
                i->m_source->is_uniform())
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
// InputArray::iterator class implementation.
//

// Constructors.
InputArray::iterator::iterator(const InputArray* array, const size_t index)
  : m_input_array(array)
  , m_input_index(index)
{
}
InputArray::iterator::iterator(const iterator& rhs)
  : m_input_array(rhs.m_input_array)
  , m_input_index(rhs.m_input_index)
{
}

// Assignment operator.
InputArray::iterator& InputArray::iterator::operator=(const iterator& rhs)
{
    m_input_array = rhs.m_input_array;
    m_input_index = rhs.m_input_index;
    return *this;
}

// Equality and inequality tests.
bool InputArray::iterator::operator==(const iterator& rhs) const
{
    return
        m_input_index == rhs.m_input_index &&
        m_input_array == rhs.m_input_array;
}
bool InputArray::iterator::operator!=(const iterator& rhs) const
{
    return
        m_input_index != rhs.m_input_index ||
        m_input_array != rhs.m_input_array;
}

// Preincrement and predecrement operators.
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

// Dereference operator.
InputArray::iterator& InputArray::iterator::operator*()
{
    return *this;
}

// Get the name of the entry.
const char* InputArray::iterator::name() const
{
    return m_input_array->impl->m_input_decls[m_input_index].m_name.c_str();
}

// Get the format of the input.
InputFormat InputArray::iterator::format() const
{
    return m_input_array->impl->m_input_decls[m_input_index].m_format;
}

// Return true if the input is optional, false otherwise.
bool InputArray::iterator::is_optional() const
{
    return m_input_array->impl->m_input_decls[m_input_index].m_is_optional;
}

// Bind a source to this input.
void InputArray::iterator::bind(Source* source)
{
    m_input_array->impl->m_input_decls[m_input_index].m_source = source;
}

// Get the source bound to this input (or 0 if no source is bound).
Source* InputArray::iterator::source() const
{
    return m_input_array->impl->m_input_decls[m_input_index].m_source;
}

}   // namespace renderer
