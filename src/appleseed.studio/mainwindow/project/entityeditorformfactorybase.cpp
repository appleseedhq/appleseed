
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "entityeditorformfactorybase.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <cstddef>

using namespace foundation;
using namespace std;

namespace appleseed {
namespace studio {

const string EntityEditorFormFactoryBase::NameParameter = "__name";
const string EntityEditorFormFactoryBase::ModelParameter = "__model";

EntityEditorFormFactoryBase::EntityEditorFormFactoryBase(const string& entity_name)
  : m_entity_name(entity_name)
{
}

void EntityEditorFormFactoryBase::add_name_input_metadata(
    const Dictionary&                   input_values,
    InputMetadataCollection&            metadata) const
{
    const string name = get_value(input_values, NameParameter, m_entity_name);

    metadata.push_back(
        Dictionary()
            .insert("name", NameParameter)
            .insert("label", "Name")
            .insert("type", "text")
            .insert("use", "required")
            .insert("default", name)
            .insert("focus", "true"));
}

void EntityEditorFormFactoryBase::add_input_metadata(
    const DictionaryArray&              input_metadata,
    const Dictionary&                   input_values,
    InputMetadataCollection&            metadata) const
{
    for (size_t i = 0; i < input_metadata.size(); ++i)
    {
        Dictionary im = input_metadata[i];

        const string widget_name = im.get<string>("name");
        if (input_values.strings().exist(widget_name))
            im.insert("default", input_values.get<string>(widget_name));

        metadata.push_back(im);
    }
}

}   // namespace studio
}   // namespace appleseed
