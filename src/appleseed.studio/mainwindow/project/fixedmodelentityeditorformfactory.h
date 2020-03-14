
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
class FixedModelEntityEditorFormFactory
  : public EntityEditorFormFactoryBase
{
  public:
    FixedModelEntityEditorFormFactory(
        const FactoryRegistrar&         factory_registrar,
        const std::string&              entity_name,
        const std::string&              model);

    void update(
        const foundation::Dictionary&   values,
        InputMetadataCollection&        metadata) const override;

  private:
    const FactoryRegistrar&     m_factory_registrar;
    const std::string           m_model;
};


//
// FixedModelEntityEditorFormFactory class implementation.
//

template <typename FactoryRegistrar>
FixedModelEntityEditorFormFactory<FactoryRegistrar>::FixedModelEntityEditorFormFactory(
    const FactoryRegistrar&             factory_registrar,
    const std::string&                  entity_name,
    const std::string&                  model)
  : EntityEditorFormFactoryBase(entity_name)
  , m_factory_registrar(factory_registrar)
  , m_model(model)
{
}

template <typename FactoryRegistrar>
void FixedModelEntityEditorFormFactory<FactoryRegistrar>::update(
    const foundation::Dictionary&       values,
    InputMetadataCollection&            metadata) const
{
    metadata.clear();
    add_name_input_metadata(values, metadata);

    const typename FactoryRegistrar::FactoryType* factory =
        m_factory_registrar.lookup(m_model.c_str());

    // We insert only the current model, so the model of the entity cannot be changed.
    foundation::Dictionary model_items;
    model_items.insert(factory->get_model_metadata().get("label"), m_model);

    metadata.push_back(
        foundation::Dictionary()
            .insert("name", ModelParameter.c_str())
            .insert("label", "Model")
            .insert("type", "enumeration")
            .insert("items", model_items)
            .insert("use", "required")
            .insert("value", m_model)
            .insert("on_change", "rebuild_form"));

    add_input_metadata(
        factory->get_input_metadata(),
        values,
        metadata);
}

}   // namespace studio
}   // namespace appleseed
