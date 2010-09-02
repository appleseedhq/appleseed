
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
#include "entityeditorwindow.h"

// UI definition header.
#include "ui_entityeditorwindow.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// Qt headers.
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QHBoxLayout>
#include <QKeySequence>
#include <QLabel>
#include <QPushButton>
#include <QShortCut>

// Standard headers.
#include <cassert>

using namespace foundation;
using namespace std;

namespace foundation
{
    template <>
    inline QString from_string(const string& s)
    {
        return QString::fromStdString(s);
    }
}

namespace appleseed {
namespace studio {

EntityEditorWindow::EntityEditorWindow(
    QWidget*                parent,
    const string&           window_title,
    auto_ptr<IFormFactory>  form_factory,
    const QVariant&         payload)
  : QWidget(parent)
  , m_ui(new Ui::EntityEditorWindow())
  , m_form_factory(form_factory)
  , m_payload(payload)
  , m_form_layout(0)
{
    m_ui->setupUi(this);

    setAttribute(Qt::WA_DeleteOnClose);
    setWindowFlags(Qt::Tool);
    setWindowTitle(QString::fromStdString(window_title));

    resize(400, 300);

    create_form_layout();
    rebuild_form(Dictionary());

    connect(
        m_ui->buttonbox->button(QDialogButtonBox::Ok), SIGNAL(clicked()),
        this, SLOT(slot_accept()));

    connect(
        m_ui->buttonbox->button(QDialogButtonBox::Cancel), SIGNAL(clicked()),
        this, SLOT(close()));

    connect(
        new QShortcut(QKeySequence(Qt::Key_Return), this), SIGNAL(activated()),
        this, SLOT(slot_accept()));

    connect(
        new QShortcut(QKeySequence(Qt::Key_Escape), this), SIGNAL(activated()),
        this, SLOT(close()));
}

EntityEditorWindow::~EntityEditorWindow()
{
    for (const_each<ValueReaderCollection> i = m_value_readers; i; ++i)
        delete i->second;

    delete m_ui;
}

void EntityEditorWindow::create_form_layout()
{
    m_form_layout = new QFormLayout(m_ui->scrollarea_contents);
    m_ui->scrollarea_contents->setLayout(m_form_layout);

    int left, top, right, bottom;
    m_form_layout->getContentsMargins(&left, &top, &right, &bottom);
    m_form_layout->setContentsMargins(0, top, 0, bottom);
}

namespace
{
    void delete_layout_items(QLayout* layout)
    {
        for (QLayoutItem* item; item = layout->takeAt(0); )
        {
            if (item->layout())
                delete_layout_items(item->layout());
            else item->widget()->deleteLater();
            delete item;
        }
    }
}

void EntityEditorWindow::rebuild_form(const Dictionary& values)
{
    delete_layout_items(m_form_layout);

    m_form_factory->update(values, m_widget_definitions);

    for (const_each<WidgetDefinitionCollection> i = m_widget_definitions; i; ++i)
        create_input_widget(*i);
}

void EntityEditorWindow::create_input_widget(const Dictionary& definition)
{
    const string widget_type = definition.get<string>("widget");

    if (widget_type == "text_box")
    {
        create_text_box_input_widget(definition);
    }
    else if (widget_type == "entity_picker")
    {
        create_entity_picker_input_widget(definition);
    }
    else if (widget_type == "dropdown_list")
    {
        create_dropdown_list_input_widget(definition);
    }
    else
    {
        assert(!"Unknown widget type.");
    }
}

namespace
{
    QString get_label_text(const Dictionary& definition)
    {
        return definition.get<QString>("label") + ":";
    }

    bool should_be_focused(const Dictionary& definition)
    {
        return
            definition.strings().exist("focus") &&
            definition.strings().get<bool>("focus");
    }
}

void EntityEditorWindow::create_text_box_input_widget(const Dictionary& definition)
{
    QLineEdit* line_edit = new QLineEdit(m_ui->scrollarea_contents);

    if (definition.strings().exist("default"))
        line_edit->setText(definition.strings().get<QString>("default"));

    if (should_be_focused(definition))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    m_form_layout->addRow(get_label_text(definition), line_edit);

    const string name = definition.get<string>("name");
    m_value_readers[name] = new LineEditValueReader(line_edit);
}

void EntityEditorWindow::create_entity_picker_input_widget(const Dictionary& definition)
{
    QLineEdit* line_edit = new QLineEdit(m_ui->scrollarea_contents);

    if (definition.strings().exist("default"))
        line_edit->setText(definition.strings().get<QString>("default"));

    if (should_be_focused(definition))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    QWidget* button = new QPushButton("Browse", m_ui->scrollarea_contents);
    button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);

    QHBoxLayout* layout = new QHBoxLayout(m_ui->scrollarea_contents);
    layout->addWidget(line_edit);
    layout->addWidget(button);

    m_form_layout->addRow(get_label_text(definition), layout);

    const string name = definition.get<string>("name");
    m_value_readers[name] = new LineEditValueReader(line_edit);
}

void EntityEditorWindow::create_dropdown_list_input_widget(const Dictionary& definition)
{
    QComboBox* combo_box = new QComboBox(m_ui->scrollarea_contents);
    combo_box->setEditable(false);

    const StringDictionary& items = definition.dictionaries().get("dropdown_items").strings();
    for (const_each<StringDictionary> i = items; i; ++i)
        combo_box->addItem(i->name(), i->value<QString>());

    if (definition.strings().exist("default"))
    {
        const QString default_value = definition.strings().get<QString>("default");
        combo_box->setCurrentIndex(combo_box->findData(QVariant::fromValue(default_value)));
    }

    if (definition.strings().exist("on_change"))
    {
        const string on_change_value = definition.strings().get<string>("on_change");
        if (on_change_value == "rebuild_form")
            connect(combo_box, SIGNAL(currentIndexChanged(int)), this, SLOT(slot_rebuild_form()));
    }

    if (should_be_focused(definition))
        combo_box->setFocus();

    m_form_layout->addRow(get_label_text(definition), combo_box);

    const string name = definition.get<string>("name");
    m_value_readers[name] = new ComboBoxValueReader(combo_box);
}

Dictionary EntityEditorWindow::get_values() const
{
    Dictionary values;

    for (const_each<WidgetDefinitionCollection> i = m_widget_definitions; i; ++i)
    {
        const Dictionary& definition = *i;

        const string name = definition.get<string>("name");
        const string value = m_value_readers.find(name)->second->read();

        values.insert(name, value);
    }

    return values;
}

void EntityEditorWindow::slot_accept()
{
    emit accepted(m_payload, get_values());
}

void EntityEditorWindow::slot_rebuild_form()
{
    rebuild_form(get_values());
}

}   // namespace studio
}   // namespace appleseed
