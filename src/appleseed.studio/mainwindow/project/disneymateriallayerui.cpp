
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Marius Avram, The appleseedhq Organization
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
#include "disneymateriallayerui.h"

// appleseed.studio headers.
#include "mainwindow/project/disneymaterialcustomui.h"

// Qt headers.
#include <QFormLayout>
#include <QStyle>
#include <Qt>
#include <QToolButton>
#include <QVBoxLayout>
#include <QWidget>

using namespace foundation;
using namespace std;

namespace appleseed {
namespace studio {

DisneyMaterialLayerUI::DisneyMaterialLayerUI(
    const string&               layer_name,
    DisneyMaterialCustomUI*     entity_editor,
    QVBoxLayout*                parent_layout,
    QWidget*                    parent)
  : m_layer_name(layer_name)
  , m_entity_editor(entity_editor)
  , QFrame(parent)
  , m_parent_layout(parent_layout)
  , m_is_folded(false)
{
    setObjectName("material_editor_layer");

    m_spacer = new QWidget();
    QHBoxLayout* spacer_layout = new QHBoxLayout(m_spacer);
    spacer_layout->setSpacing(0);

    QVBoxLayout *layout = new QVBoxLayout(this);
    m_parent_layout->insertWidget(m_parent_layout->count() - 2, this);

    QWidget *button_box = new QWidget(this);
    QHBoxLayout *button_box_layout = new QHBoxLayout(button_box);
    button_box_layout->setSpacing(0);
    button_box_layout->setMargin(0);
    layout->addWidget(button_box);

    m_inner_layout = new QFormLayout();
    m_inner_layout->setSpacing(7);
    layout->addLayout(m_inner_layout);

    // Folding button.
    m_fold_arrow_disabled = QIcon(":/widgets/header_arrow_down_disabled.png");
    m_fold_arrow_enabled = QIcon(":/widgets/scrollbar_arrow_right_disabled.png");
    m_fold_button = new QToolButton(button_box);
    m_fold_button->setIcon(m_fold_arrow_disabled);
    button_box_layout->addWidget(m_fold_button);
    connect(m_fold_button, SIGNAL(clicked()), this, SLOT(slot_fold()));

    button_box_layout->addStretch(1);

    // Up button.
    QIcon arrow_up = QIcon(":icons/layer_arrow_up.png");
    QToolButton* up_button = new QToolButton(button_box);
    up_button->setIcon(arrow_up);
    button_box_layout->addWidget(up_button);
    connect(up_button, SIGNAL(clicked()), this, SLOT(slot_move_layer_up()));

    // Down button.
    QIcon arrow_down = QIcon(":icons/layer_arrow_down.png");
    QToolButton* down_button = new QToolButton(button_box);
    down_button->setIcon(arrow_down);
    button_box_layout->addWidget(down_button);
    connect(down_button, SIGNAL(clicked()), this, SLOT(slot_move_layer_down()));

    // Close button.
    QIcon close = QIcon(":/icons/close.png");
    QToolButton* close_button = new QToolButton(button_box);
    close_button->setIcon(close);
    button_box_layout->addWidget(close_button);
    connect(close_button, SIGNAL(clicked()), this, SLOT(slot_delete_layer()));
}

void DisneyMaterialLayerUI::mousePressEvent(QMouseEvent* event)
{
    for (int i = 1; i < m_parent_layout->count() - 2; ++i)
    {
        QLayoutItem* layout_item = m_parent_layout->itemAt(i);
        QWidget* widget = layout_item->widget();
        if (widget->objectName() == "selected_material_editor_layer")
        {
            widget->setObjectName("material_editor_layer");
            style()->unpolish(widget);
            style()->polish(widget);
            break;
        }
    }
    setObjectName("selected_material_editor_layer");
    style()->unpolish(this);
    style()->polish(this);
}

void DisneyMaterialLayerUI::mouseDoubleClickEvent(QMouseEvent* event)
{
    fold_layer();
}

QFormLayout* DisneyMaterialLayerUI::get_layout()
{
    return m_inner_layout;
}

void DisneyMaterialLayerUI::fold_layer()
{
    if (m_is_folded)
    {
        m_inner_layout->setSpacing(7);
        m_inner_layout->removeWidget(m_spacer);
        m_spacer->hide();
        m_fold_button->setIcon(m_fold_arrow_disabled);
    }

    for (int i=2; i<m_inner_layout->count(); ++i)
    {
        QWidget* widget = m_inner_layout->itemAt(i)->widget();
        if (widget)
            m_is_folded ? widget->show() : widget->hide();

        QLayout* vertical_layout = m_inner_layout->itemAt(i)->layout();
        if (vertical_layout)
        {
            for (int j=0; j<vertical_layout->count(); ++j)
            {
                QWidget* widget = vertical_layout->itemAt(j)->widget();
                m_is_folded ? widget->show() : widget->hide();
            }
        }
    }

    if (!m_is_folded)
    {
        m_inner_layout->setSpacing(0);
        m_inner_layout->addWidget(m_spacer);
        m_spacer->show();
        m_fold_button->setIcon(m_fold_arrow_enabled);
    }

    // Add extra margin to shown labels when folded.
    QWidget* label = m_inner_layout->itemAt(0)->widget();
    m_is_folded ?
        label->setObjectName("unfolded_label") :
        label->setObjectName("folded_label");
    style()->unpolish(label);
    style()->polish(label);

    m_is_folded = !m_is_folded;
}

void DisneyMaterialLayerUI::slot_delete_layer()
{
    // Remove model
    string layer_rename = m_entity_editor->m_renames.get(m_layer_name.c_str());
    Dictionary& deleted_layer = m_entity_editor->m_values.dictionary(layer_rename);
    size_t deleted_layer_number = deleted_layer.get<size_t>("layer_number");
    m_entity_editor->m_values.dictionaries().remove(layer_rename);

    // Shift remaining layer numbers.
    for (const_each<DictionaryDictionary> i = m_entity_editor->m_values.dictionaries(); i; ++i)
    {
        Dictionary& layer_params = m_entity_editor->m_values.dictionary(i->name());
        size_t layer_number = layer_params.get<size_t>("layer_number");
        if (layer_number > deleted_layer_number)
            layer_params.insert("layer_number", layer_number - 1);
    }

    m_entity_editor->emit_signal_custom_applied();
    delete this;
}

void DisneyMaterialLayerUI::update_model(const int new_position, const int offset)
{
    if (new_position > 0)
    {
        string previous_layer_name, next_layer_name;
        for (const_each<DictionaryDictionary> i = m_entity_editor->m_values.dictionaries(); i; ++i)
        {
            Dictionary& layer_params = m_entity_editor->m_values.dictionary(i->name());
            size_t layer_number = layer_params.get<size_t>("layer_number");
            if (layer_number == new_position - 1 + offset)
                previous_layer_name = i->name();
            else if (layer_number == new_position + offset)
                next_layer_name = i->name();
        }
        m_entity_editor->m_values
            .dictionary(previous_layer_name)
            .insert("layer_number", new_position + offset);
        m_entity_editor->m_values
            .dictionary(next_layer_name)
            .insert("layer_number", new_position - 1 + offset);
        m_entity_editor->emit_signal_custom_applied();
    }
}

void DisneyMaterialLayerUI::slot_move_layer_up()
{
    int new_position = 0;
    // Update interface.
    for (int i=1; i<m_parent_layout->count(); ++i)
    {
        QLayoutItem* layout_item = m_parent_layout->itemAt(i);
        if (this == layout_item->widget())
        {
            if (i > 1)
            {
                m_parent_layout->takeAt(i);
                new_position = i-1;
                m_parent_layout->insertWidget(new_position, this);
            }
            break;
        }
    }

    // Update model.
    update_model(new_position, 1);
}

void DisneyMaterialLayerUI::slot_move_layer_down()
{
    int new_position = 0;
    // Update interface.
    for (int i=1; i<m_parent_layout->count(); ++i)
    {
        QLayoutItem* layout_item = m_parent_layout->itemAt(i);
        if (this == layout_item->widget())
        {
            if (i < m_parent_layout->count()-3)
            {
                m_parent_layout->takeAt(i);
                new_position = i+1;
                m_parent_layout->insertWidget(new_position, this);
            }
            break;
        }
    }

    // Update model.
    update_model(new_position, 0);
}

void DisneyMaterialLayerUI::slot_fold()
{
    fold_layer();
}

}       // namespace studio
}       // namespace appleseed
