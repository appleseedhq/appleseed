
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORFORMFACTORY_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORFORMFACTORY_H

// appleseed.studio headers.
#include "mainwindow/project/entityeditorwindow.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cstddef>
#include <string>

namespace appleseed {
namespace studio {

template <typename FactoryRegistrar>
class EntityEditorFormFactory
  : public EntityEditorWindow::IFormFactory
{
  public:
    typedef EntityEditorWindow::WidgetDefinitionCollection WidgetDefinitionCollection;

    EntityEditorFormFactory(
        const FactoryRegistrar&         factory_registrar,
        const std::string&              name_suggestion);

    virtual void update(
        const foundation::Dictionary&   values,
        WidgetDefinitionCollection&     definitions) const;

  private:
    const FactoryRegistrar&     m_factory_registrar;
    const std::string           m_name_suggestion;
};


//
// EntityEditorFormFactory class implementation.
//

template <typename FactoryRegistrar>
EntityEditorFormFactory<FactoryRegistrar>::EntityEditorFormFactory(
    const FactoryRegistrar&         factory_registrar,
    const std::string&              name_suggestion)
  : m_factory_registrar(factory_registrar)
  , m_name_suggestion(name_suggestion)
{
}

template <typename FactoryRegistrar>
void EntityEditorFormFactory<FactoryRegistrar>::update(
    const foundation::Dictionary&   values,
    WidgetDefinitionCollection&     definitions) const
{
    definitions.clear();

    const std::string name = get_value(values, "name", m_name_suggestion);

    definitions.push_back(
        foundation::Dictionary()
            .insert("name", "name")
            .insert("label", "Name")
            .insert("widget", "text_box")
            .insert("use", "required")
            .insert("default", name)
            .insert("focus", "true"));

    const typename FactoryRegistrar::FactoryArrayType factories =
        m_factory_registrar.get_factories();

    foundation::Dictionary model_items;
    for (size_t i = 0; i < factories.size(); ++i)
    {
        model_items.insert(
            factories[i]->get_human_readable_model(),
            factories[i]->get_model());
    }

    const std::string model =
        get_value(
            values,
            "model",
            factories.empty() ? "" : factories[0]->get_model());

    definitions.push_back(
        foundation::Dictionary()
            .insert("name", "model")
            .insert("label", "Model")
            .insert("widget", "dropdown_list")
            .insert("dropdown_items", model_items)
            .insert("use", "required")
            .insert("default", model)
            .insert("on_change", "rebuild_form"));

    if (!model.empty())
    {
        const typename FactoryRegistrar::FactoryType* factory =
            m_factory_registrar.lookup(model.c_str());

        const DictionaryArray properties = factory->get_widget_definitions();

        for (size_t i = 0; i < properties.size(); ++i)
            definitions.push_back(properties[i]);
    }
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORFORMFACTORY_H
