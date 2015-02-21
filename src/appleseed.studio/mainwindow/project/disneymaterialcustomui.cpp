
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Marius Avram, The appleseedhq Organization
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
#include "disneymaterialcustomui.h"

// appleseed.studio headers.
#include "mainwindow/project/disneymateriallayerui.h"

// appleseed.renderer headers.
#include "renderer/api/material.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Qt headers.
#include <QPushButton>
#include <QVBoxLayout>

// Standard headers.
#include <algorithm>
#include <cassert>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

//
// DisneyMaterialCustomUI class implementation.
//

namespace
{
    int get_layer_number(const Dictionary& values)
    {
        return
            values.strings().exist("layer_number")
                ? values.get<int>("layer_number")
                : 0;
    }

    struct LayerDefinitionOrderPredicate
    {
        bool operator()(const Dictionary& lhs, const Dictionary& rhs) const
        {
            return get_layer_number(lhs) < get_layer_number(rhs);
        }
    };

    vector<Dictionary> collect_layer_definitions(const Dictionary& values)
    {
        vector<Dictionary> layer_definitions;

        for (const_each<DictionaryDictionary> i = values.dictionaries(); i; ++i)
            layer_definitions.push_back(i->value());

        sort(
            layer_definitions.begin(),
            layer_definitions.end(),
            LayerDefinitionOrderPredicate());

        return layer_definitions;
    }
}

DisneyMaterialCustomUI::DisneyMaterialCustomUI(
    const Project&      project,
    ParamArray&         settings)
  : m_project(project)
  , m_settings(settings)
  , m_parent(0)
  , m_layout(0)
{
}

void DisneyMaterialCustomUI::create_widgets(
    QVBoxLayout*        layout,
    const Dictionary&   values)
{
    m_parent = layout->parentWidget();
    m_layout = layout;

    // Add New Layer button.
    m_add_layer_button = new QPushButton("Add New Layer");
    m_layout->addWidget(m_add_layer_button);
    connect(m_add_layer_button, SIGNAL(clicked()), this, SLOT(slot_add_layer()));

    // Stretcher at the bottom.
    m_layout->addStretch(1);

    const vector<Dictionary> layer_definitions = collect_layer_definitions(values);

    if (layer_definitions.empty())
    {
        // Insert the initial layer.
        append_new_layer(make_new_layer_values());
    }
    else
    {
        for (const_each<vector<Dictionary> > i = layer_definitions; i; ++i)
            append_new_layer(*i);
    }
}

Dictionary DisneyMaterialCustomUI::get_values() const
{
    Dictionary values;

    for (size_t i = 0; i < m_layers.size(); ++i)
    {
        Dictionary layer_values = m_layers[i]->get_values();

        values.insert(
            layer_values.get<string>("layer_name"),
            layer_values.insert("layer_number", i));
    }

    return values;
}

void DisneyMaterialCustomUI::slot_add_layer()
{
    append_new_layer(make_new_layer_values());

    emit signal_apply();
}

void DisneyMaterialCustomUI::slot_move_layer_up(QWidget* layer_widget)
{
    const size_t layer_index = find_layer_index_by_widget(layer_widget);

    if (layer_index > 0)
    {
        swap(m_layers[layer_index], m_layers[layer_index - 1]);

        const int layer_widget_index = m_layout->indexOf(layer_widget);
        m_layout->takeAt(layer_widget_index);
        m_layout->insertWidget(layer_widget_index - 1, layer_widget);
    }

    emit signal_apply();
}

void DisneyMaterialCustomUI::slot_move_layer_down(QWidget* layer_widget)
{
    const size_t layer_index = find_layer_index_by_widget(layer_widget);

    if (layer_index < m_layers.size() - 1)
    {
        swap(m_layers[layer_index], m_layers[layer_index + 1]);

        const int layer_widget_index = m_layout->indexOf(layer_widget);
        m_layout->takeAt(layer_widget_index);
        m_layout->insertWidget(layer_widget_index + 1, layer_widget);
    }

    emit signal_apply();
}

void DisneyMaterialCustomUI::slot_delete_layer(QWidget* layer_widget)
{
    const size_t layer_index = find_layer_index_by_widget(layer_widget);

    m_layers.erase(m_layers.begin() + layer_index);

    m_layout->removeWidget(layer_widget);
    layer_widget->deleteLater();

    emit signal_apply();
}

size_t DisneyMaterialCustomUI::find_layer_index_by_widget(const QWidget* layer_widget) const
{
    for (size_t i = 0; i < m_layers.size(); ++i)
    {
        if (m_layers[i] == layer_widget)
            return i;
    }

    assert(!"Could not find layer widget.");
    return ~0;
}

vector<string> DisneyMaterialCustomUI::collect_layer_names() const
{
    vector<string> names;

    for (size_t i = 0; i < m_layers.size(); ++i)
        names.push_back(m_layers[i]->get_values().get<string>("layer_name"));

    return names;
}

Dictionary DisneyMaterialCustomUI::make_new_layer_values()
{
    return
        DisneyMaterialLayer::get_default_values()
            .insert("layer_name", make_unique_name("layer", collect_layer_names()));
}

void DisneyMaterialCustomUI::append_new_layer(const Dictionary& values)
{
    DisneyMaterialLayerUI* layer_widget =
        new DisneyMaterialLayerUI(m_parent, m_project, m_settings, values);

    connect(
        layer_widget, SIGNAL(signal_move_layer_up(QWidget*)),
        this, SLOT(slot_move_layer_up(QWidget*)));
    connect(
        layer_widget, SIGNAL(signal_move_layer_down(QWidget*)),
        this, SLOT(slot_move_layer_down(QWidget*)));
    connect(
        layer_widget, SIGNAL(signal_delete_layer(QWidget*)),
        this, SLOT(slot_delete_layer(QWidget*)));
    connect(
        layer_widget, SIGNAL(signal_apply()),
        this, SIGNAL(signal_apply()));

    m_layers.push_back(layer_widget);

    m_layout->insertWidget(m_layout->count() - 2, layer_widget);
}

}   // namespace studio
}   // namespace appleseed
