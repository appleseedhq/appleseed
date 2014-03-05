
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "entityeditor.h"

// appleseed.studio headers.
#include "mainwindow/project/entitybrowserwindow.h"
#include "utility/doubleslider.h"
#include "utility/interop.h"
#include "utility/miscellaneous.h"
#include "utility/mousewheelfocuseventfilter.h"

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

EntityEditor::EntityEditor(
    QWidget*                    parent,
    const Project&              project,
    auto_ptr<IFormFactory>      form_factory,
    auto_ptr<IEntityBrowser>    entity_browser,
    const Dictionary&           values)
  : QObject(parent)
  , m_parent(parent)
  , m_project(project)
  , m_form_factory(form_factory)
  , m_entity_browser(entity_browser)
  , m_entity_picker_bind_signal_mapper(new QSignalMapper(this))
  , m_color_picker_signal_mapper(new QSignalMapper(this))
  , m_file_picker_signal_mapper(new QSignalMapper(this))
{
    create_form_layout();
    create_connections();
    rebuild_form(values);
}

Dictionary EntityEditor::get_values() const
{
    return m_widget_proxies.get_values();
}

void EntityEditor::create_form_layout()
{
    assert(m_parent->layout() == 0);

    m_form_layout = new QFormLayout(m_parent);
    m_form_layout->setLabelAlignment(Qt::AlignRight);
    m_form_layout->setSpacing(10);
    m_form_layout->setFieldGrowthPolicy(QFormLayout::AllNonFixedFieldsGrow);
}

void EntityEditor::create_connections()
{
    connect(
        m_entity_picker_bind_signal_mapper, SIGNAL(mapped(const QString&)),
        SLOT(slot_open_entity_browser(const QString&)));

    connect(
        m_color_picker_signal_mapper, SIGNAL(mapped(const QString&)),
        SLOT(slot_open_color_picker(const QString&)));

    connect(
        m_file_picker_signal_mapper, SIGNAL(mapped(const QString&)),
        SLOT(slot_open_file_picker(const QString&)));
}

void EntityEditor::rebuild_form(const Dictionary& values)
{
    clear_layout(m_form_layout);

    m_widget_proxies.clear();

    // The mappings were removed when the widgets were deleted.

    m_form_factory->update(values, m_input_metadata);

    for (const_each<InputMetadataCollection> i = m_input_metadata; i; ++i)
        create_input_widgets(*i);
}

Dictionary EntityEditor::get_input_metadata(const string& name) const
{
    for (const_each<InputMetadataCollection> i = m_input_metadata; i; ++i)
    {
        const Dictionary& metadata = *i;

        if (metadata.get<string>("name") == name)
            return metadata;
    }

    return Dictionary();
}

void EntityEditor::create_input_widgets(const Dictionary& metadata)
{
    const string type = metadata.get<string>("type");

    if (type == "text")
    {
        create_text_input_widgets(metadata);
    }
    else if (type == "numeric")
    {
        create_numeric_input_widgets(metadata);
    }
    else if (type == "colormap")
    {
        create_colormap_input_widgets(metadata);
    }
    else if (type == "boolean")
    {
        create_boolean_input_widgets(metadata);
    }
    else if (type == "enumeration")
    {
        create_enumeration_input_widgets(metadata);
    }
    else if (type == "entity")
    {
        create_entity_input_widgets(metadata);
    }
    else if (type == "color")
    {
        create_color_input_widgets(metadata);
    }
    else if (type == "file")
    {
        create_file_input_widgets(metadata);
    }
    else
    {
        assert(!"Unknown input type.");
    }
}

namespace
{
    QString get_label_text(const Dictionary& metadata)
    {
        return metadata.get<QString>("label") + ":";
    }

    bool should_be_focused(const Dictionary& metadata)
    {
        return
            metadata.strings().exist("focus") &&
            metadata.strings().get<bool>("focus");
    }
}

void EntityEditor::create_text_input_widgets(const Dictionary& metadata)
{
    QLineEdit* line_edit = new QLineEdit(m_parent);

    const string name = metadata.get<string>("name");
    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));

    if (metadata.strings().exist("default"))
        widget_proxy->set(metadata.strings().get<string>("default"));

    connect(widget_proxy.get(), SIGNAL(signal_changed()), SLOT(slot_apply()));

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    m_form_layout->addRow(get_label_text(metadata), line_edit);
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

void EntityEditor::create_numeric_input_widgets(const Dictionary& metadata)
{
    QLineEdit* line_edit = new QLineEdit(m_parent);
    line_edit->setMaximumWidth(60);

    const double min_value = metadata.get<double>("min_value");
    const double max_value = metadata.get<double>("max_value");

    DoubleSlider* slider = new DoubleSlider(Qt::Horizontal, m_parent);
    slider->setRange(min_value, max_value);
    slider->setPageStep((max_value - min_value) / 10.0);

    new MouseWheelFocusEventFilter(slider);

    // Connect the line edit and the slider together.
    LineEditDoubleSliderAdaptor* adaptor =
        new LineEditDoubleSliderAdaptor(line_edit, slider);
    connect(
        slider, SIGNAL(valueChanged(const double)),
        adaptor, SLOT(slot_set_line_edit_value(const double)));
    connect(
        line_edit, SIGNAL(textChanged(const QString&)),
        adaptor, SLOT(slot_set_slider_value(const QString&)));

    const string name = metadata.get<string>("name");
    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));

    if (metadata.strings().exist("default"))
        widget_proxy->set(metadata.strings().get<string>("default"));

    connect(line_edit, SIGNAL(returnPressed()), SLOT(slot_apply()));
    connect(slider, SIGNAL(valueChanged(int)), SLOT(slot_apply()));

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(line_edit);
    layout->addWidget(slider);
    m_form_layout->addRow(get_label_text(metadata), layout);
}

void EntityEditor::create_colormap_input_widgets(const Dictionary& metadata)
{
    QLineEdit* line_edit = new QLineEdit(m_parent);
    line_edit->setMaximumWidth(120);

    const double min_value = 0.0;
    const double max_value = 1.0;

    DoubleSlider* slider = new DoubleSlider(Qt::Horizontal, m_parent);
    slider->setRange(min_value, max_value);
    slider->setPageStep((max_value - min_value) / 10.0);

    new MouseWheelFocusEventFilter(slider);

    // Connect the line edit and the slider together.
    LineEditDoubleSliderAdaptor* adaptor =
        new LineEditDoubleSliderAdaptor(line_edit, slider);
    connect(
        slider, SIGNAL(valueChanged(const double)),
        adaptor, SLOT(slot_set_line_edit_value(const double)));
    connect(
        line_edit, SIGNAL(textChanged(const QString&)),
        adaptor, SLOT(slot_set_slider_value(const QString&)));

    const string name = metadata.get<string>("name");

    QWidget* bind_button = new QPushButton("Bind", m_parent);
    bind_button->setObjectName("bind_entity_button");
    bind_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    connect(bind_button, SIGNAL(clicked()), m_entity_picker_bind_signal_mapper, SLOT(map()));
    m_entity_picker_bind_signal_mapper->setMapping(bind_button, QString::fromStdString(name));

    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));

    if (metadata.strings().exist("default"))
        widget_proxy->set(metadata.strings().get<string>("default"));

    connect(widget_proxy.get(), SIGNAL(signal_changed()), SLOT(slot_apply()));
    connect(slider, SIGNAL(valueChanged(int)), SLOT(slot_apply()));

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(line_edit);
    layout->addWidget(slider);
    layout->addWidget(bind_button);
    m_form_layout->addRow(get_label_text(metadata), layout);
}

void EntityEditor::create_boolean_input_widgets(const Dictionary& metadata)
{
    QCheckBox* checkbox = new QCheckBox(m_parent);

    const string name = metadata.get<string>("name");
    auto_ptr<IInputWidgetProxy> widget_proxy(new CheckBoxProxy(checkbox));

    if (metadata.strings().exist("default"))
        widget_proxy->set(metadata.strings().get<string>("default"));

    connect(widget_proxy.get(), SIGNAL(signal_changed()), SLOT(slot_apply()));

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(metadata))
        checkbox->setFocus();

    m_form_layout->addRow(get_label_text(metadata), checkbox);
}

void EntityEditor::create_enumeration_input_widgets(const Dictionary& metadata)
{
    QComboBox* combo_box = new QComboBox(m_parent);
    combo_box->setEditable(false);

    new MouseWheelFocusEventFilter(combo_box);

    const string name = metadata.get<string>("name");
    auto_ptr<IInputWidgetProxy> widget_proxy(new ComboBoxProxy(combo_box));

    const StringDictionary& items = metadata.dictionaries().get("items").strings();
    for (const_each<StringDictionary> i = items; i; ++i)
        combo_box->addItem(i->name(), i->value<QString>());

    if (metadata.strings().exist("default"))
    {
        const QString default_value = metadata.strings().get<QString>("default");
        combo_box->setCurrentIndex(combo_box->findData(QVariant::fromValue(default_value)));
    }

    if (metadata.strings().exist("on_change") &&
        metadata.strings().get<string>("on_change") == "rebuild_form")
    {
        connect(combo_box, SIGNAL(currentIndexChanged(int)), SLOT(slot_rebuild_form()));
    }
    else
    {
        connect(widget_proxy.get(), SIGNAL(signal_changed()), SLOT(slot_apply()));
    }

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(metadata))
        combo_box->setFocus();

    m_form_layout->addRow(get_label_text(metadata), combo_box);
}

void EntityEditor::create_entity_input_widgets(const Dictionary& metadata)
{
    QLineEdit* line_edit = new QLineEdit(m_parent);

    const string name = metadata.get<string>("name");

    QWidget* bind_button = new QPushButton("Bind", m_parent);
    bind_button->setObjectName("bind_entity_button");
    bind_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    connect(bind_button, SIGNAL(clicked()), m_entity_picker_bind_signal_mapper, SLOT(map()));
    m_entity_picker_bind_signal_mapper->setMapping(bind_button, QString::fromStdString(name));

    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));

    if (metadata.strings().exist("default"))
        widget_proxy->set(metadata.strings().get<string>("default"));

    connect(widget_proxy.get(), SIGNAL(signal_changed()), SLOT(slot_apply()));

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(line_edit);
    layout->addWidget(bind_button);
    m_form_layout->addRow(get_label_text(metadata), layout);
}

void EntityEditor::create_color_input_widgets(const Dictionary& metadata)
{
    QLineEdit* line_edit = new QLineEdit(m_parent);

    QToolButton* picker_button = new QToolButton(m_parent);
    picker_button->setObjectName("ColorPicker");
    connect(picker_button, SIGNAL(clicked()), m_color_picker_signal_mapper, SLOT(map()));

    const string name = metadata.get<string>("name");
    m_color_picker_signal_mapper->setMapping(picker_button, QString::fromStdString(name));

    auto_ptr<IInputWidgetProxy> widget_proxy(new ColorPickerProxy(line_edit, picker_button));

    // Find the wavelength widget proxy and get its values
    // todo: Should we check the existence of such a proxy? Are we guaranteed to get a wavelength_range proxy?
    const string wavelength_range = m_widget_proxies.get("wavelength_range")->get();
    
    if (metadata.strings().exist("default"))
    {
        widget_proxy->set(
            metadata.strings().get<string>("default"),
            wavelength_range
        );
    }
    else widget_proxy->set("0.0 0.0 0.0");

    connect(widget_proxy.get(), SIGNAL(signal_changed()), SLOT(slot_apply()));

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(line_edit);
    layout->addWidget(picker_button);
    m_form_layout->addRow(get_label_text(metadata), layout);
}

void EntityEditor::create_file_input_widgets(const Dictionary& metadata)
{
    QLineEdit* line_edit = new QLineEdit(m_parent);

    QWidget* browse_button = new QPushButton("Browse", m_parent);
    browse_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    connect(browse_button, SIGNAL(clicked()), m_file_picker_signal_mapper, SLOT(map()));

    const string name = metadata.get<string>("name");
    m_file_picker_signal_mapper->setMapping(browse_button, QString::fromStdString(name));

    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));

    if (metadata.strings().exist("default"))
        widget_proxy->set(metadata.strings().get<string>("default"));

    connect(widget_proxy.get(), SIGNAL(signal_changed()), SLOT(slot_apply()));

    m_widget_proxies.insert(name, widget_proxy);

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(line_edit);
    layout->addWidget(browse_button);
    m_form_layout->addRow(get_label_text(metadata), layout);
}

void EntityEditor::slot_rebuild_form()
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
        void slot_accept(
            const QString&  page_name,
            const QString&  entity_name)
        {
            emit signal_accepted(m_widget_name, page_name, entity_name);
        }

      signals:
        void signal_accepted(
            const QString&  widget_name,
            const QString&  page_name,
            const QString&  entity_name);

      private:
        const QString m_widget_name;
    };
}

void EntityEditor::slot_open_entity_browser(const QString& widget_name)
{
    const Dictionary metadata = get_input_metadata(widget_name.toStdString());

    EntityBrowserWindow* browser_window =
        new EntityBrowserWindow(
            m_parent,
            metadata.get<string>("label"));

    const Dictionary& entity_types = metadata.dictionaries().get("entity_types");

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
        browser_window, SIGNAL(signal_accepted(const QString&, const QString&)),
        forward_signal, SLOT(slot_accept(const QString&, const QString&)));
    connect(
        forward_signal, SIGNAL(signal_accepted(const QString&, const QString&, const QString&)),
        SLOT(slot_entity_browser_accept(const QString&, const QString&, const QString&)));

    browser_window->showNormal();
    browser_window->activateWindow();
}

void EntityEditor::slot_entity_browser_accept(QString widget_name, QString page_name, QString entity_name)
{
    m_widget_proxies.get(widget_name.toStdString())->set(entity_name.toStdString());

    // Close the entity browser.
    qobject_cast<QWidget*>(sender()->parent())->close();
}

namespace
{
    class ForwardColorChangedSignal
      : public QObject
    {
        Q_OBJECT

      public:
        ForwardColorChangedSignal(QObject* parent, const QString& widget_name)
          : QObject(parent)
          , m_widget_name(widget_name)
        {
        }

      public slots:
        void slot_color_changed(const QColor& color)
        {
            emit signal_color_changed(m_widget_name, color);
        }

      signals:
        void signal_color_changed(const QString& widget_name, const QColor& color);

      private:
        const QString m_widget_name;
    };
}

void EntityEditor::slot_open_color_picker(const QString& widget_name)
{
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());

    const string wavelength_range = m_widget_proxies.get("wavelength_range")->get();

    const Color3d initial_color = ColorPickerProxy::get_color_from_string(widget_proxy->get(), wavelength_range);

    QColorDialog* dialog =
        new QColorDialog(
            color_to_qcolor(initial_color),
            m_parent);
    dialog->setWindowTitle("Pick Color");
    dialog->setOptions(QColorDialog::DontUseNativeDialog);

    ForwardColorChangedSignal* forward_signal =
        new ForwardColorChangedSignal(dialog, widget_name);
    connect(
        dialog, SIGNAL(currentColorChanged(const QColor&)),
        forward_signal, SLOT(slot_color_changed(const QColor&)));
    connect(
        forward_signal, SIGNAL(signal_color_changed(const QString&, const QColor&)),
        SLOT(slot_color_changed(const QString&, const QColor&)));

    dialog->exec();
}

void EntityEditor::slot_color_changed(const QString& widget_name, const QColor& color)
{
    m_widget_proxies.get(widget_name.toStdString())->set(to_string(qcolor_to_color<Color3d>(color)));
}

void EntityEditor::slot_open_file_picker(const QString& widget_name)
{
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());

    const Dictionary metadata = get_input_metadata(widget_name.toStdString());

    if (metadata.get<string>("file_picker_mode") == "open")
    {
        const filesystem::path project_root_path = filesystem::path(m_project.get_path()).parent_path();
        const filesystem::path file_path = absolute(widget_proxy->get(), project_root_path);
        const filesystem::path file_root_path = file_path.parent_path();

        QFileDialog::Options options;
        QString selected_filter;

        QString filepath =
            QFileDialog::getOpenFileName(
                m_parent,
                "Open...",
                QString::fromStdString(file_root_path.string()),
                metadata.get<QString>("file_picker_filter"),
                &selected_filter,
                options);

        if (!filepath.isEmpty())
            widget_proxy->set(QDir::toNativeSeparators(filepath).toStdString());
    }
}

void EntityEditor::slot_apply()
{
    emit signal_applied(m_widget_proxies.get_values());
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/project/moc_cpp_entityeditor.cxx"
