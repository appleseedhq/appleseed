
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

// appleseed.studio headers.
#include "mainwindow/project/entitybrowserwindow.h"
#include "utility/doubleslider.h"
#include "utility/interop.h"
#include "utility/tweaks.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/string.h"

// Qt headers.
#include <QCheckBox>
#include <QColor>
#include <QColorDialog>
#include <QComboBox>
#include <QDialogButtonBox>
#include <QFileDialog>
#include <QFormLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QShortcut>
#include <QSignalMapper>
#include <QString>
#include <Qt>
#include <QToolButton>
#include <QVariant>

// boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>

using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

EntityEditorWindow::EntityEditorWindow(
    QWidget*                    parent,
    const string&               window_title,
    const Project&              project,
    auto_ptr<IFormFactory>      form_factory,
    auto_ptr<IEntityBrowser>    entity_browser,
    const Dictionary&           values)
  : QWidget(parent)
  , m_ui(new Ui::EntityEditorWindow())
  , m_project(project)
  , m_form_factory(form_factory)
  , m_entity_browser(entity_browser)
  , m_form_layout(0)
  , m_entity_picker_signal_mapper(new QSignalMapper(this))
  , m_color_picker_signal_mapper(new QSignalMapper(this))
  , m_file_picker_signal_mapper(new QSignalMapper(this))
{
    m_ui->setupUi(this);

    setAttribute(Qt::WA_DeleteOnClose);
    setWindowFlags(Qt::Tool);
    setWindowModality(Qt::NonModal);
    setWindowTitle(QString::fromStdString(window_title));

    resize(500, 400);

    create_form_layout();
    rebuild_form(values);

    m_initial_values = m_widget_proxies.get_values();

    connect(
        m_entity_picker_signal_mapper, SIGNAL(mapped(const QString&)),
        SLOT(slot_open_entity_browser(const QString&)));

    connect(
        m_color_picker_signal_mapper, SIGNAL(mapped(const QString&)),
        SLOT(slot_open_color_picker(const QString&)));

    connect(
        m_file_picker_signal_mapper, SIGNAL(mapped(const QString&)),
        SLOT(slot_open_file_picker(const QString&)));

    connect(m_ui->buttonbox, SIGNAL(accepted()), SLOT(slot_accept()));
    connect(m_ui->buttonbox, SIGNAL(rejected()), SLOT(slot_cancel()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Escape), SIGNAL(activated()),
        SLOT(slot_cancel()));
}

EntityEditorWindow::~EntityEditorWindow()
{
    delete m_ui;
}

namespace
{
    void delete_layout_items(QLayout* layout)
    {
        while (!layout->isEmpty())
        {
            QLayoutItem* item = layout->takeAt(0);

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

    m_widget_proxies.clear();

    m_form_factory->update(values, m_widget_definitions);

    for (const_each<WidgetDefinitionCollection> i = m_widget_definitions; i; ++i)
        create_input_widget(*i);
}

void EntityEditorWindow::create_form_layout()
{
    m_form_layout = new QFormLayout(m_ui->scrollarea_contents);
    m_form_layout->setLabelAlignment(Qt::AlignRight);
    m_form_layout->setSpacing(10);
    m_form_layout->setFieldGrowthPolicy(QFormLayout::AllNonFixedFieldsGrow);
}

Dictionary EntityEditorWindow::get_widget_definition(const string& name) const
{
    for (const_each<WidgetDefinitionCollection> i = m_widget_definitions; i; ++i)
    {
        const Dictionary& definition = *i;

        if (definition.get<string>("name") == name)
            return definition;
    }

    return Dictionary();
}

void EntityEditorWindow::create_input_widget(const Dictionary& definition)
{
    const string widget_type = definition.get<string>("widget");

    if (widget_type == "text_box")
    {
        create_text_box_input_widget(definition);
    }
    else if (widget_type == "numeric")
    {
        create_numeric_input_widget(definition);
    }
    else if (widget_type == "checkbox")
    {
        create_checkbox_input_widget(definition);
    }
    else if (widget_type == "dropdown_list")
    {
        create_dropdown_list_input_widget(definition);
    }
    else if (widget_type == "entity_picker")
    {
        create_entity_picker_input_widget(definition);
    }
    else if (widget_type == "color_picker")
    {
        create_color_picker_input_widget(definition);
    }
    else if (widget_type == "file_picker")
    {
        create_file_picker_input_widget(definition);
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

    const string name = definition.get<string>("name");
    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));

    if (definition.strings().exist("default"))
        widget_proxy->set(definition.strings().get<string>("default"));

    connect(widget_proxy.get(), SIGNAL(signal_changed()), SLOT(slot_apply()));

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(definition))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    m_form_layout->addRow(get_label_text(definition), line_edit);
}

namespace
{
    class LineEditDoubleSliderAdaptor
      : public QObject
    {
        Q_OBJECT

      public:
        LineEditDoubleSliderAdaptor(QLineEdit* line_edit, DoubleSlider* slider)
          : QObject(line_edit)
          , m_line_edit(line_edit)
          , m_slider(slider)
        {
        }

      public slots:
        void slot_set_line_edit_value(const double value)
        {
            // Don't block signals here, for live edit to work we want the line edit to signal changes.
            m_line_edit->setText(QString("%1").arg(value));
        }

        void slot_set_slider_value(const QString& value)
        {
            m_slider->blockSignals(true);
            m_slider->setValue(value.toDouble());
            m_slider->blockSignals(false);
        }

      private:
        QLineEdit*      m_line_edit;
        DoubleSlider*   m_slider;
    };
}

void EntityEditorWindow::create_numeric_input_widget(const Dictionary& definition)
{
    QLineEdit* line_edit = new QLineEdit(m_ui->scrollarea_contents);
    line_edit->setMaximumWidth(60);

    const double min_value = definition.get<double>("min_value");
    const double max_value = definition.get<double>("max_value");

    DoubleSlider* slider = new DoubleSlider(Qt::Horizontal, m_ui->scrollarea_contents);
    slider->setRange(min_value, max_value);
    slider->setPageStep((max_value - min_value) / 10.0);

    // Connect the line edit and the slider together.
    LineEditDoubleSliderAdaptor* adaptor =
        new LineEditDoubleSliderAdaptor(line_edit, slider);
    connect(
        slider, SIGNAL(valueChanged(const double)),
        adaptor, SLOT(slot_set_line_edit_value(const double)));
    connect(
        line_edit, SIGNAL(textChanged(const QString&)),
        adaptor, SLOT(slot_set_slider_value(const QString&)));

    const string name = definition.get<string>("name");
    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));

    if (definition.strings().exist("default"))
        widget_proxy->set(definition.strings().get<string>("default"));

    connect(line_edit, SIGNAL(returnPressed()), SLOT(slot_apply()));
    connect(slider, SIGNAL(valueChanged(int)), SLOT(slot_apply()));

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(definition))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(line_edit);
    layout->addWidget(slider);
    m_form_layout->addRow(get_label_text(definition), layout);
}

void EntityEditorWindow::create_checkbox_input_widget(const Dictionary& definition)
{
    QCheckBox* checkbox = new QCheckBox(m_ui->scrollarea_contents);

    const string name = definition.get<string>("name");
    auto_ptr<IInputWidgetProxy> widget_proxy(new CheckBoxProxy(checkbox));

    if (definition.strings().exist("default"))
        widget_proxy->set(definition.strings().get<string>("default"));

    connect(widget_proxy.get(), SIGNAL(signal_changed()), SLOT(slot_apply()));

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(definition))
        checkbox->setFocus();

    m_form_layout->addRow(get_label_text(definition), checkbox);
}

void EntityEditorWindow::create_dropdown_list_input_widget(const Dictionary& definition)
{
    QComboBox* combo_box = new QComboBox(m_ui->scrollarea_contents);
    combo_box->setEditable(false);

    const string name = definition.get<string>("name");
    auto_ptr<IInputWidgetProxy> widget_proxy(new ComboBoxProxy(combo_box));

    const StringDictionary& items = definition.dictionaries().get("dropdown_items").strings();
    for (const_each<StringDictionary> i = items; i; ++i)
        combo_box->addItem(i->name(), i->value<QString>());

    if (definition.strings().exist("default"))
    {
        const QString default_value = definition.strings().get<QString>("default");
        combo_box->setCurrentIndex(combo_box->findData(QVariant::fromValue(default_value)));
    }

    if (definition.strings().exist("on_change") &&
        definition.strings().get<string>("on_change") == "rebuild_form")
    {
        connect(combo_box, SIGNAL(currentIndexChanged(int)), SLOT(slot_rebuild_form()));
    }
    else
    {
        connect(widget_proxy.get(), SIGNAL(signal_changed()), SLOT(slot_apply()));
    }

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(definition))
        combo_box->setFocus();

    m_form_layout->addRow(get_label_text(definition), combo_box);
}

void EntityEditorWindow::create_entity_picker_input_widget(const Dictionary& definition)
{
    QLineEdit* line_edit = new QLineEdit(m_ui->scrollarea_contents);

    QWidget* browse_button = new QPushButton("Browse", m_ui->scrollarea_contents);
    browse_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    connect(browse_button, SIGNAL(clicked()), m_entity_picker_signal_mapper, SLOT(map()));

    const string name = definition.get<string>("name");
    m_entity_picker_signal_mapper->setMapping(browse_button, QString::fromStdString(name));

    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));

    if (definition.strings().exist("default"))
        widget_proxy->set(definition.strings().get<string>("default"));

    connect(widget_proxy.get(), SIGNAL(signal_changed()), SLOT(slot_apply()));

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(definition))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(line_edit);
    layout->addWidget(browse_button);
    m_form_layout->addRow(get_label_text(definition), layout);
}

void EntityEditorWindow::create_color_picker_input_widget(const Dictionary& definition)
{
    QLineEdit* line_edit = new QLineEdit(m_ui->scrollarea_contents);

    QToolButton* picker_button = new QToolButton(m_ui->scrollarea_contents);
    picker_button->setObjectName("ColorPicker");
    connect(picker_button, SIGNAL(clicked()), m_color_picker_signal_mapper, SLOT(map()));

    const string name = definition.get<string>("name");
    m_color_picker_signal_mapper->setMapping(picker_button, QString::fromStdString(name));

    auto_ptr<IInputWidgetProxy> widget_proxy(new ColorPickerProxy(line_edit, picker_button));

    if (definition.strings().exist("default"))
        widget_proxy->set(definition.strings().get<string>("default"));
    else widget_proxy->set("0.0 0.0 0.0");

    connect(widget_proxy.get(), SIGNAL(signal_changed()), SLOT(slot_apply()));

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(definition))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(line_edit);
    layout->addWidget(picker_button);
    m_form_layout->addRow(get_label_text(definition), layout);
}

void EntityEditorWindow::create_file_picker_input_widget(const Dictionary& definition)
{
    QLineEdit* line_edit = new QLineEdit(m_ui->scrollarea_contents);

    QWidget* browse_button = new QPushButton("Browse", m_ui->scrollarea_contents);
    browse_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    connect(browse_button, SIGNAL(clicked()), m_file_picker_signal_mapper, SLOT(map()));

    const string name = definition.get<string>("name");
    m_file_picker_signal_mapper->setMapping(browse_button, QString::fromStdString(name));

    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));

    if (definition.strings().exist("default"))
        widget_proxy->set(definition.strings().get<string>("default"));

    connect(widget_proxy.get(), SIGNAL(signal_changed()), SLOT(slot_apply()));

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(definition))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(line_edit);
    layout->addWidget(browse_button);
    m_form_layout->addRow(get_label_text(definition), layout);
}

void EntityEditorWindow::slot_rebuild_form()
{
    rebuild_form(m_widget_proxies.get_values());

    emit signal_applied(m_widget_proxies.get_values());
}

namespace
{
    class ForwardAcceptedSignal
      : public QObject
    {
        Q_OBJECT

      public:
        ForwardAcceptedSignal(QObject* parent, const QString& widget_name)
          : QObject(parent)
          , m_widget_name(widget_name)
        {
        }

      public slots:
        void slot_accept(QString page_name, QString entity_name)
        {
            emit signal_accepted(m_widget_name, page_name, entity_name);
        }

      signals:
        void signal_accepted(QString widget_name, QString page_name, QString entity_name);

      private:
        const QString m_widget_name;
    };
}

void EntityEditorWindow::slot_open_entity_browser(const QString& widget_name)
{
    const Dictionary widget_definition = get_widget_definition(widget_name.toStdString());

    EntityBrowserWindow* browser_window =
        new EntityBrowserWindow(
            this,
            widget_definition.get<string>("label"));

    const Dictionary& entity_types = widget_definition.dictionaries().get("entity_types");

    for (const_each<StringDictionary> i = entity_types.strings(); i; ++i)
    {
        const string entity_type = i->name();
        const string entity_label = i->value<string>();
        const StringDictionary entities = m_entity_browser->get_entities(entity_type);
        browser_window->add_items_page(entity_type, entity_label, entities);
    }

    ForwardAcceptedSignal* forward_signal =
        new ForwardAcceptedSignal(browser_window, widget_name);

    connect(
        browser_window, SIGNAL(signal_accepted(QString, QString)),
        forward_signal, SLOT(slot_accept(QString, QString)));

    connect(
        forward_signal, SIGNAL(signal_accepted(QString, QString, QString)),
        SLOT(slot_entity_browser_accept(QString, QString, QString)));

    browser_window->showNormal();
    browser_window->activateWindow();
}

void EntityEditorWindow::slot_entity_browser_accept(QString widget_name, QString page_name, QString entity_name)
{
    m_widget_proxies.get(widget_name.toStdString())->set(entity_name.toStdString());

    // Close the entity browser.
    qobject_cast<QWidget*>(sender()->parent())->close();
}

void EntityEditorWindow::slot_open_color_picker(const QString& widget_name)
{
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());

    const Color3d initial_color = ColorPickerProxy::get_color_from_string(widget_proxy->get());

    const QColor new_color =
        QColorDialog::getColor(
            color_to_qcolor(initial_color),
            this,
            "Pick Color",
            QColorDialog::DontUseNativeDialog);

    if (new_color.isValid())
        widget_proxy->set(to_string(qcolor_to_color<Color3d>(new_color)));
}

void EntityEditorWindow::slot_open_file_picker(const QString& widget_name)
{
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());

    const Dictionary widget_definition = get_widget_definition(widget_name.toStdString());

    if (widget_definition.get<string>("file_picker_mode") == "open")
    {
        const filesystem::path project_root_path = filesystem::path(m_project.get_path()).parent_path();
        const filesystem::path file_path = absolute(widget_proxy->get(), project_root_path);
        const filesystem::path file_root_path = file_path.parent_path();

        QFileDialog::Options options;
        QString selected_filter;

        QString filepath =
            QFileDialog::getOpenFileName(
                this,
                "Open...",
                QString::fromStdString(file_root_path.string()),
                widget_definition.get<QString>("file_picker_filter"),
                &selected_filter,
                options);

        if (!filepath.isEmpty())
            widget_proxy->set(QDir::toNativeSeparators(filepath).toStdString());
    }
}

void EntityEditorWindow::slot_apply()
{
    emit signal_applied(m_widget_proxies.get_values());
}

void EntityEditorWindow::slot_accept()
{
    emit signal_accepted(m_widget_proxies.get_values());

    close();
}

void EntityEditorWindow::slot_cancel()
{
    if (m_initial_values != m_widget_proxies.get_values())
        emit signal_canceled(m_initial_values);

    close();
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/project/moc_cpp_entityeditorwindow.cxx"
