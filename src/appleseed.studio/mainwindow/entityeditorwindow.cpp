
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
    QWidget*            parent,
    const string&       window_title,
    const QVariant&     payload)
  : QWidget(parent)
  , m_ui(new Ui::EntityEditorWindow())
  , m_payload(payload)
{
    m_ui->setupUi(this);

    setAttribute(Qt::WA_DeleteOnClose);
    setWindowFlags(Qt::Tool);
    setWindowTitle(QString::fromStdString(window_title));

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

void EntityEditorWindow::build_form(const InputWidgetCollection& input_widgets)
{
    m_input_widgets = input_widgets;

    QFormLayout* layout = new QFormLayout(m_ui->scrollarea_contents);

    int left, top, right, bottom;
    layout->getContentsMargins(&left, &top, &right, &bottom);
    layout->setContentsMargins(0, top, 0, bottom);

    for (const_each<InputWidgetCollection> i = input_widgets; i; ++i)
        create_input_widget(layout, *i);

    m_ui->scrollarea_contents->setLayout(layout);

    resize(400, sizeHint().height());
}

void EntityEditorWindow::create_input_widget(QFormLayout* layout, const Dictionary& widget_params)
{
    const string widget_type = widget_params.get<string>("widget");

    if (widget_type == "text_box")
    {
        create_text_box_input_widget(layout, widget_params);
    }
    else if (widget_type == "entity_picker")
    {
        create_entity_picker_input_widget(layout, widget_params);
    }
    else if (widget_type == "dropdown_list")
    {
        create_dropdown_list_input_widget(layout, widget_params);
    }
    else
    {
        assert(!"Unknown widget type.");
    }
}

namespace
{
    QString get_label_text(const Dictionary& widget_params)
    {
        return widget_params.get<QString>("label") + ":";
    }

    bool should_be_focused(const Dictionary& widget_params)
    {
        return
            widget_params.strings().exist("focus") &&
            widget_params.strings().get<bool>("focus");
    }
}

void EntityEditorWindow::create_text_box_input_widget(
    QFormLayout*        layout,
    const Dictionary&   widget_params)
{
    QLineEdit* line_edit = new QLineEdit(m_ui->scrollarea_contents);

    if (widget_params.strings().exist("default"))
        line_edit->setText(widget_params.strings().get<QString>("default"));

    if (should_be_focused(widget_params))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    const string name = widget_params.get<string>("name");
    m_value_readers[name] = new LineEditValueReader(line_edit);

    layout->addRow(get_label_text(widget_params), line_edit);
}

void EntityEditorWindow::create_entity_picker_input_widget(
    QFormLayout*        layout,
    const Dictionary&   widget_params)
{
    QLineEdit* line_edit = new QLineEdit(m_ui->scrollarea_contents);

    if (widget_params.strings().exist("default"))
        line_edit->setText(widget_params.strings().get<QString>("default"));

    if (should_be_focused(widget_params))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    QWidget* button = new QPushButton("Browse", m_ui->scrollarea_contents);
    button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);

    QHBoxLayout* sublayout = new QHBoxLayout(m_ui->scrollarea_contents);
    sublayout->addWidget(line_edit);
    sublayout->addWidget(button);

    const string name = widget_params.get<string>("name");
    m_value_readers[name] = new LineEditValueReader(line_edit);

    layout->addRow(get_label_text(widget_params), sublayout);
}

void EntityEditorWindow::create_dropdown_list_input_widget(
    QFormLayout*        layout,
    const Dictionary&   widget_params)
{
    QComboBox* combo_box = new QComboBox(m_ui->scrollarea_contents);
    combo_box->setEditable(false);

    const StringDictionary& items = widget_params.dictionaries().get("dropdown_items").strings();
    for (const_each<StringDictionary> i = items; i; ++i)
        combo_box->addItem(i->name(), i->value<QString>());

    if (widget_params.strings().exist("default"))
    {
        const QString default_value = widget_params.strings().get<QString>("default");
        combo_box->setCurrentIndex(combo_box->findData(QVariant::fromValue(default_value)));
    }

    if (should_be_focused(widget_params))
        combo_box->setFocus();

    const string name = widget_params.get<string>("name");
    m_value_readers[name] = new ComboBoxValueReader(combo_box);

    layout->addRow(get_label_text(widget_params), combo_box);
}

void EntityEditorWindow::slot_accept()
{
    Dictionary values;

    for (const_each<InputWidgetCollection> i = m_input_widgets; i; ++i)
    {
        const Dictionary& widget_params = *i;
        const string name = widget_params.get<string>("name");
        const string value = m_value_readers[name]->read();
        values.insert(name, value);
    }

    emit accepted(m_payload, values);
}

}   // namespace studio
}   // namespace appleseed
