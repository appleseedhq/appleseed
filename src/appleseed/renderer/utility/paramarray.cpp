
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
#include "paramarray.h"

// Standard headers.
#include <cstddef>
#include <vector>

using namespace foundation;

namespace renderer
{

//
// ParamArray class implementation.
//

namespace
{
    static const char* PartSeparator = ".";

    const ParamArray m_empty_param_array;
}

ParamArray::ParamArray()
{
}

ParamArray::ParamArray(const ParamArray& rhs)
  : Dictionary(rhs)
{
}

ParamArray::ParamArray(const Dictionary& dictionary)
  : Dictionary(dictionary)
{
}

ParamArray& ParamArray::operator=(const ParamArray& rhs)
{
    Dictionary::operator=(rhs);
    return *this;
}

ParamArray& ParamArray::insert_path(const char* path, const char* value)
{
    assert(path);
    assert(value);

    std::vector<std::string> parts;
    tokenize(path, PartSeparator, parts);

    assert(!parts.empty());

    Dictionary* leaf = this;

    for (size_t i = 0; i < parts.size() - 1; ++i)
    {
        const std::string& part = parts[i];

        if (!leaf->dictionaries().exist(part.c_str()))
            leaf->insert(part.c_str(), Dictionary());

        leaf = &leaf->dictionary(part.c_str());
    }

    leaf->insert(parts.back().c_str(), value);

    return *this;
}

bool ParamArray::exist_path(const char* path) const
{
    assert(path);

    std::vector<std::string> parts;
    tokenize(path, PartSeparator, parts);

    assert(!parts.empty());

    const Dictionary* leaf = this;

    for (size_t i = 0; i < parts.size() - 1; ++i)
    {
        if (!leaf->dictionaries().exist(parts[i].c_str()))
            return false;

        leaf = &leaf->dictionary(parts[i].c_str());
    }

    return leaf->strings().exist(parts.back().c_str());
}

const char* ParamArray::get_path(const char* path) const
{
    assert(path);

    std::vector<std::string> parts;
    tokenize(path, PartSeparator, parts);

    assert(!parts.empty());

    const Dictionary* leaf = this;

    for (size_t i = 0; i < parts.size() - 1; ++i)
        leaf = &leaf->dictionary(parts[i].c_str());

    return leaf->strings().get(parts.back().c_str());
}

ParamArray& ParamArray::remove_path(const char* path)
{
    assert(path);

    std::vector<std::string> parts;
    tokenize(path, PartSeparator, parts);

    assert(!parts.empty());

    Dictionary* leaf = this;

    for (size_t i = 0; i < parts.size() - 1; ++i)
    {
        if (!leaf->dictionaries().exist(parts[i].c_str()))
            return *this;

        leaf = &leaf->dictionary(parts[i].c_str());
    }

    leaf->strings().remove(parts.back().c_str());

    return *this;
}

ParamArray& ParamArray::push(const char* name)
{
    assert(name);

    if (!dictionaries().exist(name))
        dictionaries().insert(name, ParamArray());

    return static_cast<ParamArray&>(dictionaries().get(name));
}

const ParamArray& ParamArray::child(const char* name) const
{
    assert(name);

    return dictionaries().exist(name)
        ? static_cast<const ParamArray&>(dictionaries().get(name))
        : m_empty_param_array;
}

}   // namespace renderer
