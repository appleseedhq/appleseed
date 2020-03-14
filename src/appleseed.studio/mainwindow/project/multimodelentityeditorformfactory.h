
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

// appleseed.studio headers.
#include "mainwindow/project/entityeditorformfactorybase.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"

// Standard headers.
#include <cstddef>
#include <string>

namespace appleseed {
namespace studio {

template <typename FactoryRegistrar>
class MultiModelEntityEditorFormFactory
  : public EntityEditorFormFactoryBase
{
  public:
    MultiModelEntityEditorFormFactory(
        const FactoryRegistrar&         factory_registrar,
        const std::string&              entity_name);

    void update(
        const foundation::Dictionary&   values,
        InputMetadataCollection&        metadata) const override;

  private:
    const FactoryRegistrar&     m_factory_registrar;

    std::string add_model_widget_definition(
        const foundation::Dictionary&   values,
        InputMetadataCollection&        metadata) const;
};


//
// MultiModelEntityEditorFormFactory class implementation.
//

template <typename FactoryRegistrar>
MultiModelEntityEditorFormFactory<FactoryRegistrar>::MultiModelEntityEditorFormFactory(
    const FactoryRegistrar&             factory_registrar,
    const std::string&                  entity_name)
  : EntityEditorFormFactoryBase(entity_name)
  , m_factory_registrar(factory_registrar)
{
}

template <typename FactoryRegistrar>
void MultiModelEntityEditorFormFactory<FactoryRegistrar>::update(
    const foundation::Dictionary&       values,
    InputMetadataCollection&            metadata) const
{
    metadata.clear();

    add_name_input_metadata(values, metadata);

    const std::string model =
        add_model_widget_definition(values, metadata);

    if (!model.empty())
    {
        const typename FactoryRegistrar::FactoryType* factory =
            m_factory_registrar.lookup(model.c_str());

        add_input_metadata(
            factory->get_input_metadata(),
            values,
            metadata);
    }
}

template <typename FactoryRegistrar>
std::string MultiModelEntityEditorFormFactory<FactoryRegistrar>::add_model_widget_definition(
    const foundation::Dictionary&       values,
    InputMetadataCollection&            metadata) const
{
    const typename FactoryRegistrar::FactoryArrayType factories =
        m_factory_registrar.get_factories();

    foundation::Dictionary model_items;
    std::string default_model;

    for (size_t i = 0; i < factories.size(); ++i)
    {
        const foundation::Dictionary model_metadata = factories[i]->get_model_metadata();
        const std::string model_name = model_metadata.get("name");

        model_items.insert(model_metadata.get("label"), model_name);

        if (model_metadata.strings().exist("default_model") &&
            model_metadata.get<bool>("default_model"))
            default_model = model_name;
    }

    if (default_model.empty())
        default_model = factories[0]->get_model();

    const std::string model = get_value(values, ModelParameter, default_model);

    metadata.push_back(
        foundation::Dictionary()
            .insert("name", ModelParameter.c_str())
            .insert("label", "Model")
            .insert("type", "enumeration")
            .insert("items", model_items)
            .insert("use", "required")
            .insert("value", model)
            .insert("on_change", "rebuild_form"));

    return model;
}

}   // namespace studio
}   // namespace appleseed
