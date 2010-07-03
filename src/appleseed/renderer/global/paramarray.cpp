
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
#include "paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// ParamArray class implementation.
//

namespace
{
    const ParamArray empty_param_array;
}

void ParamArray::insert_path(const char* path, const char* value)
{
    assert(path);
    assert(value);

    vector<string> parts;
    tokenize(path, ".", parts);

    Dictionary* leaf = this;

    if (parts.size() > 1)
    {
        for (size_t i = 0; i < parts.size() - 1; ++i)
        {
            const string& part = parts[i];

            if (!leaf->dictionaries().exist(part))
                leaf->insert(part, Dictionary());

            leaf = &leaf->dictionary(part);
        }
    }

    leaf->insert(parts.back(), value);
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
        : empty_param_array;
}

namespace
{
    void merge_dictionaries(Dictionary& dest, const Dictionary& source)
    {
        // Merge strings.
        for (const_each<StringDictionary> i = source.strings(); i; ++i)
            dest.insert(i->name(), i->value());

        // Recursively merge dictionaries.
        for (const_each<DictionaryDictionary> i = source.dictionaries(); i; ++i)
        {
            if (dest.dictionaries().exist(i->name()))
            {
                merge_dictionaries(dest.dictionary(i->name()), i->value());
            }
            else
            {
                dest.insert(i->name(), i->value());
            }
        }
    }
}

void ParamArray::merge(const ParamArray& rhs)
{
    merge_dictionaries(*this, rhs);
}

}   // namespace renderer
