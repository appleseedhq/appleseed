
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
#include "entityeditorformfactorybase.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

namespace appleseed {
namespace studio {

const std::string EntityEditorFormFactoryBase::NameParameter = "__name";
const std::string EntityEditorFormFactoryBase::ModelParameter = "__model";

EntityEditorFormFactoryBase::EntityEditorFormFactoryBase(const std::string& entity_name)
  : m_entity_name(entity_name)
{
}

void EntityEditorFormFactoryBase::add_name_input_metadata(
    const Dictionary&                   input_values,
    InputMetadataCollection&            metadata) const
{
    const std::string name = get_value(input_values, NameParameter, m_entity_name);

    metadata.push_back(
        Dictionary()
            .insert("name", NameParameter)
            .insert("label", "Name")
            .insert("type", "text")
            .insert("use", "required")
            .insert("value", name)
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
        const std::string input_name = im.get<std::string>("name");

        im.insert("value",
            input_values.strings().exist(input_name.c_str()) ? input_values.get<std::string>(input_name.c_str()) :
            im.strings().exist("default") ? im.get<std::string>("default") :
            "");

        metadata.push_back(im);
    }
}

}   // namespace studio
}   // namespace appleseed
