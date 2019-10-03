
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

// Interface header.
#include "specializedapiarrays.h"

// Standard headers.
#include <cassert>
#include <string>
#include <vector>

namespace foundation
{

APPLESEED_DEFINE_APIARRAY(FloatArray);
APPLESEED_DEFINE_APIARRAY(DoubleArray);
APPLESEED_DEFINE_APIARRAY(DictionaryArray);


//
// StringArray class implementation.
//

struct StringArray::Impl
  : public std::vector<std::string>
{
};

StringArray::StringArray()
  : impl(new Impl())
{
}

StringArray::StringArray(const StringArray& rhs)
  : impl(new Impl(*rhs.impl))
{
}

StringArray::StringArray(
    const size_type     size,
    const value_type*   values)
  : impl(new Impl())
{
    assert(size > 0);
    assert(values);

    impl->resize(size);

    for (size_t i = 0; i < size; ++i)
        (*impl)[i] = values[i];
}

StringArray::~StringArray()
{
    delete impl;
}

StringArray& StringArray::operator=(const StringArray& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

StringArray::size_type StringArray::size() const
{
    return impl->size();
}

bool StringArray::empty() const
{
    return impl->empty();
}

void StringArray::clear()
{
    impl->clear();
}

void StringArray::reserve(const size_type count)
{
    impl->reserve(count);
}

void StringArray::resize(const size_type new_size)
{
    impl->resize(new_size);
}

void StringArray::push_back(const value_type val)
{
    impl->push_back(val);
}

void StringArray::set(const size_type pos, const value_type val)
{
    assert(val);
    (*impl)[pos] = val;
}

StringArray::value_type StringArray::operator[](const size_type pos) const
{
    return (*impl)[pos].c_str();
}

}   // namespace foundation
