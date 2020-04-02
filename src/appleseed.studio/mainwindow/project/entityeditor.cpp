
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
#include "entityeditor.h"

// appleseed.studio headers.
#include "mainwindow/project/entitybrowserwindow.h"
#include "mainwindow/project/entityinputwidget.h"
#include "mainwindow/project/tools.h"
#include "utility/settingskeys.h"

// appleseed.qtcommon headers.
#include "utility/interop.h"
#include "utility/miscellaneous.h"
#include "widgets/doubleslider.h"
#include "widgets/mousewheelfocuseventfilter.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/scalar.h"
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/iostreamop.h"

// Qt headers.
#include <QCheckBox>
#include <QColor>
#include <QColorDialog>
#include <QComboBox>
#include <QDialogButtonBox>
#include <QDoubleValidator>
#include <QFileDialog>
#include <QFont>
#include <QFormLayout>
#include <QHBoxLayout>
#include <QIntValidator>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QShortcut>
#include <QSlider>
#include <QString>
#include <Qt>
#include <QToolButton>
#include <QVariant>
#include <QVBoxLayout>

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <cmath>
#include <limits>
#include <sstream>
#include <utility>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;
namespace bf = boost::filesystem;

namespace appleseed {
namespace studio {

EntityEditor::EntityEditor(
    QWidget*                             parent,
    const Project&                       project,
    ParamArray&                          settings,
    std::unique_ptr<IFormFactory>        form_factory,
    std::unique_ptr<IEntityBrowser>      entity_browser,
    std::unique_ptr<CustomEntityUI>      custom_ui,
    const Dictionary&                    values)
  : QObject(parent)
  , m_parent(parent)
  , m_project(project)
  , m_settings(settings)
  , m_form_factory(move(form_factory))
  , m_entity_browser(move(entity_browser))
  , m_custom_ui(move(custom_ui))
{
    if (m_parent->layout() != nullptr)
    {
        clear_layout(m_parent->layout());
        delete m_parent->layout();
    }

    m_top_layout = new QVBoxLayout(m_parent);
    m_top_layout->setMargin(7);

    create_connections();
    rebuild_form(values);
}

Dictionary EntityEditor::get_values() const
{
    Dictionary values;

    if (m_custom_ui.get())
        values.merge(m_custom_ui->get_values());

    values.merge(m_widget_proxies.get_values());

    return values;
}

void EntityEditor::rebuild_form(const Dictionary& values)
{
    // The mappings were removed when the widgets were deleted.
    clear_layout(m_top_layout);
    m_widget_proxies.clear();

    // Collect input metadata.
    m_form_factory->update(values, m_input_metadata);

    // Create corresponding input widgets.
    create_form_layout();
    for (const_each<InputMetadataCollection> i = m_input_metadata; i; ++i)
    {
        const bool input_widget_visible = is_input_widget_visible(*i, values);
        create_input_widgets(*i, input_widget_visible);
    }

    if (m_custom_ui.get())
        m_custom_ui->create_widgets(m_top_layout, values);
}

void EntityEditor::create_form_layout()
{
    m_form_layout = new QFormLayout();
    m_form_layout->setLabelAlignment(Qt::AlignRight);
    m_form_layout->setSpacing(10);
    m_form_layout->setFieldGrowthPolicy(QFormLayout::AllNonFixedFieldsGrow);
    m_top_layout->addLayout(m_form_layout);
}

void EntityEditor::create_connections()
{
    if (m_custom_ui.get())
    {
        connect(
            m_custom_ui.get(), SIGNAL(signal_apply()),
            SLOT(slot_apply()));
    }
}

const Dictionary& EntityEditor::get_input_metadata(const std::string& name) const
{
    for (const_each<InputMetadataCollection> i = m_input_metadata; i; ++i)
    {
        const Dictionary& metadata = *i;

        if (metadata.get<std::string>("name") == name)
            return metadata;
    }

    static Dictionary empty_dictionary;
    return empty_dictionary;
}

bool EntityEditor::is_input_widget_visible(const Dictionary& metadata, const Dictionary& values) const
{
    if (!metadata.dictionaries().exist("visible_if"))
        return true;

    // Conditions in a visible_if clause must be all true for the input widget to be visible
    // (in other words they are combined with AND operations).

    const StringDictionary& visible_if = metadata.dictionary("visible_if").strings();

    for (const auto& condition : visible_if)
    {
        const char* key = condition.key();
        const char* value = condition.value();

        const bool condition_met =
            values.strings().exist(key)
                ? values.strings().get<std::string>(key) == value
                : get_input_metadata(key).get<std::string>("default") == value;

        if (!condition_met)
            return false;
    }

    return true;
}

void EntityEditor::create_input_widgets(const Dictionary& metadata, const bool input_widget_visible)
{
    const std::string input_name = metadata.get<std::string>("name");
    const std::string input_type = metadata.get<std::string>("type");

    std::unique_ptr<IInputWidgetProxy> widget_proxy =
        input_type == "text" ? create_text_input_widgets(metadata, input_widget_visible) :
        input_type == "numeric" ? create_numeric_input_widgets(metadata, input_widget_visible) :
        input_type == "integer" ? create_integer_input_widgets(metadata, input_widget_visible) :
        input_type == "boolean" ? create_boolean_input_widgets(metadata, input_widget_visible) :
        input_type == "enumeration" ? create_enumeration_input_widgets(metadata, input_widget_visible) :
        input_type == "color" ? create_color_input_widgets(metadata, input_widget_visible) :
        input_type == "colormap" ? create_colormap_input_widgets(metadata, input_widget_visible) :
        input_type == "entity" ? create_entity_input_widgets(metadata, input_widget_visible) :
        input_type == "file" ? create_file_input_widgets(metadata, input_widget_visible) :
        std::unique_ptr<IInputWidgetProxy>(nullptr);

    assert(widget_proxy.get());

    const bool rebuild_form =
        metadata.strings().exist("on_change") &&
        metadata.get<std::string>("on_change") == "rebuild_form";

    connect(
        widget_proxy.get(),
        SIGNAL(signal_changed()),
        rebuild_form ? SLOT(slot_rebuild_form()) : SLOT(slot_apply()));

    m_widget_proxies.insert(input_name, move(widget_proxy));
}

namespace
{
    QLabel* create_label(const Dictionary& metadata)
    {
        QLabel* label = new QLabel(metadata.get<QString>("label") + ":");

        if (metadata.get<QString>("use") == "required")
        {
            QFont font;
            font.setBold(true);
            label->setFont(font);
        }

        return label;
    }

    bool should_be_focused(const Dictionary& metadata)
    {
        return
            metadata.strings().exist("focus") &&
            metadata.strings().get<bool>("focus");
    }
}

std::unique_ptr<IInputWidgetProxy> EntityEditor::create_text_input_widgets(const Dictionary& metadata, const bool input_widget_visible)
{
    QLineEdit* line_edit = new QLineEdit(m_parent);

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    if (input_widget_visible)
        m_form_layout->addRow(create_label(metadata), line_edit);
    else line_edit->hide();

    std::unique_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));
    widget_proxy->set(metadata.strings().get<std::string>("value"));

    return widget_proxy;
}

std::unique_ptr<IInputWidgetProxy> EntityEditor::create_numeric_input_widgets(const Dictionary& metadata, const bool input_widget_visible)
{
    const Dictionary& min = metadata.dictionary("min");
    const Dictionary& max = metadata.dictionary("max");

    const double slider_min = min.get<double>("value");
    const double slider_max = max.get<double>("value");

    const double validator_min = min.get<std::string>("type") == "hard" ? slider_min : -std::numeric_limits<double>::max();
    const double validator_max = max.get<std::string>("type") == "hard" ? slider_max : +std::numeric_limits<double>::max();

    QLineEdit* line_edit = new QLineEdit(m_parent);
    line_edit->setMaximumWidth(60);
    line_edit->setValidator(new QDoubleValidator(validator_min, validator_max, 16, line_edit));

    DoubleSlider* slider = new DoubleSlider(Qt::Horizontal, m_parent);
    slider->setRange(slider_min, slider_max);
    slider->setPageStep((slider_max - slider_min) / 10.0);
    new MouseWheelFocusEventFilter(slider);
    auto adaptor = new LineEditDoubleSliderAdaptor(line_edit, slider);
    connect(slider, SIGNAL(valueChanged(int)), SLOT(slot_apply()));

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    if (input_widget_visible)
    {
        QHBoxLayout* layout = new QHBoxLayout();
        layout->setSpacing(6);
        layout->addWidget(line_edit);
        layout->addWidget(slider);
        m_form_layout->addRow(create_label(metadata), layout);
    }
    else
    {
        line_edit->hide();
        slider->hide();
    }

    const std::string value = metadata.strings().get<std::string>("value");
    if (!value.empty())
    {
        // Keep the first value if there is more than one.
        std::istringstream istr(value);
        double val;
        istr >> val;
        adaptor->slot_set_line_edit_value(val);
    }

    std::unique_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));

    return widget_proxy;
}

std::unique_ptr<IInputWidgetProxy> EntityEditor::create_integer_input_widgets(const Dictionary& metadata, const bool input_widget_visible)
{
    const Dictionary& min = metadata.dictionary("min");
    const Dictionary& max = metadata.dictionary("max");

    const int slider_min = min.get<int>("value");
    const int slider_max = max.get<int>("value");

    const int validator_min = slider_min;
    const int validator_max = slider_max;

    QLineEdit* line_edit = new QLineEdit(m_parent);
    line_edit->setMaximumWidth(60);
    line_edit->setValidator(new QIntValidator(validator_min, validator_max, line_edit));

    QSlider* slider = new QSlider(Qt::Horizontal, m_parent);
    slider->setRange(slider_min, slider_max);
    new MouseWheelFocusEventFilter(slider);
    new LineEditSliderAdaptor(line_edit, slider);
    connect(slider, SIGNAL(valueChanged(int)), SLOT(slot_apply()));

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    if (input_widget_visible)
    {
        QHBoxLayout* layout = new QHBoxLayout();
        layout->setSpacing(6);
        layout->addWidget(line_edit);
        layout->addWidget(slider);
        m_form_layout->addRow(create_label(metadata), layout);
    }
    else
    {
        line_edit->hide();
        slider->hide();
    }

    std::unique_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));
    widget_proxy->set(metadata.strings().get<std::string>("value"));

    return widget_proxy;
}

std::unique_ptr<IInputWidgetProxy> EntityEditor::create_boolean_input_widgets(const Dictionary& metadata, const bool input_widget_visible)
{
    QCheckBox* checkbox = new QCheckBox(m_parent);

    if (should_be_focused(metadata))
        checkbox->setFocus();

    if (input_widget_visible)
        m_form_layout->addRow(create_label(metadata), checkbox);
    else checkbox->hide();

    std::unique_ptr<IInputWidgetProxy> widget_proxy(new CheckBoxProxy(checkbox));
    widget_proxy->set(metadata.strings().get<std::string>("value"));

    return widget_proxy;
}

std::unique_ptr<IInputWidgetProxy> EntityEditor::create_enumeration_input_widgets(const Dictionary& metadata, const bool input_widget_visible)
{
    QComboBox* combo_box = new QComboBox(m_parent);
    combo_box->setEditable(false);
    new MouseWheelFocusEventFilter(combo_box);

    const StringDictionary& items = metadata.dictionaries().get("items").strings();
    for (const_each<StringDictionary> i = items; i; ++i)
        combo_box->addItem(i->key(), i->value<QString>());

    const QString value = metadata.strings().get<QString>("value");
    combo_box->setCurrentIndex(combo_box->findData(QVariant::fromValue(value)));

    if (should_be_focused(metadata))
        combo_box->setFocus();

    if (input_widget_visible)
        m_form_layout->addRow(create_label(metadata), combo_box);
    else combo_box->hide();

    std::unique_ptr<IInputWidgetProxy> widget_proxy(new ComboBoxProxy(combo_box));

    return widget_proxy;
}

std::unique_ptr<IInputWidgetProxy> EntityEditor::create_color_input_widgets(const Dictionary& metadata, const bool input_widget_visible)
{
    QLineEdit* line_edit = new QLineEdit(m_parent);

    QToolButton* picker_button = new QToolButton(m_parent);
    picker_button->setObjectName("color_picker");
    const std::string name = metadata.get<std::string>("name");
    connect(picker_button, &QToolButton::clicked, [=]() { emit slot_open_color_picker(QString::fromStdString(name)); });

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    if (input_widget_visible)
    {
        QHBoxLayout* layout = new QHBoxLayout();
        layout->setSpacing(6);
        layout->addWidget(line_edit);
        layout->addWidget(picker_button);
        m_form_layout->addRow(create_label(metadata), layout);
    }
    else
    {
        line_edit->hide();
        picker_button->hide();
    }

    std::unique_ptr<ColorPickerProxy> widget_proxy(new ColorPickerProxy(line_edit, picker_button));

    if (metadata.strings().exist("value") &&
        metadata.strings().exist("wavelength_range_widget"))
    {
        const std::string value = metadata.strings().get<std::string>("value");
        const std::string wr_widget = metadata.get<std::string>("wavelength_range_widget");
        const IInputWidgetProxy* wr_widget_proxy = m_widget_proxies.get(wr_widget);
        if (wr_widget_proxy)
            widget_proxy->set(value, wr_widget_proxy->get());
        else widget_proxy->set(value);
    }
    else widget_proxy->set("0.0 0.0 0.0");

    return std::unique_ptr<IInputWidgetProxy>(move(widget_proxy));
}

std::unique_ptr<IInputWidgetProxy> EntityEditor::create_colormap_input_widgets(const Dictionary& metadata, const bool input_widget_visible)
{
    const std::string name = metadata.get<std::string>("name");

    double validator_min, validator_max;

    if (metadata.dictionaries().exist("min"))
    {
        const Dictionary& min = metadata.dictionary("min");
        validator_min = min.get<std::string>("type") == "hard" ? min.get<double>("value") : -std::numeric_limits<double>::max();
    }
    else
        validator_min = -std::numeric_limits<double>::max();

    if (metadata.dictionaries().exist("max"))
    {
        const Dictionary& max = metadata.dictionary("max");
        validator_max = max.get<std::string>("type") == "hard" ? max.get<double>("value") : +std::numeric_limits<double>::max();
    }
    else
        validator_max = +std::numeric_limits<double>::max();

    const QString default_value = metadata.strings().exist("default") ? metadata.get<QString>("default") : "";

    ColorMapInputWidget* input_widget = new ColorMapInputWidget(m_parent);
    input_widget->set_validator(new QDoubleValidatorWithDefault(validator_min, validator_max, 16, default_value));
    input_widget->set_default_value(default_value);

    connect(input_widget, &ColorMapInputWidget::signal_bind_button_clicked, [=]() { emit slot_open_entity_browser(QString::fromStdString(name)); });

    if (should_be_focused(metadata))
        input_widget->set_focus();

    if (input_widget_visible)
        m_form_layout->addRow(create_label(metadata), input_widget);
    else input_widget->hide();

    std::unique_ptr<IInputWidgetProxy> widget_proxy(new ColorMapInputProxy(input_widget));
    widget_proxy->set(metadata.strings().get<std::string>("value"));

    return widget_proxy;
}

std::unique_ptr<IInputWidgetProxy> EntityEditor::create_entity_input_widgets(const Dictionary& metadata, const bool input_widget_visible)
{
    const std::string name = metadata.get<std::string>("name");

    EntityInputWidget* input_widget = new EntityInputWidget(m_parent);
    connect(input_widget, &EntityInputWidget::signal_bind_button_clicked, [=]() { emit slot_open_entity_browser(QString::fromStdString(name)); });

    if (should_be_focused(metadata))
        input_widget->set_focus();

    if (input_widget_visible)
        m_form_layout->addRow(create_label(metadata), input_widget);
    else input_widget->hide();

    std::unique_ptr<IInputWidgetProxy> widget_proxy(new EntityInputProxy(input_widget));
    widget_proxy->set(metadata.strings().get<std::string>("value"));

    return widget_proxy;
}

std::unique_ptr<IInputWidgetProxy> EntityEditor::create_file_input_widgets(const Dictionary& metadata, const bool input_widget_visible)
{
    const std::string name = metadata.get<std::string>("name");

    QLineEdit* line_edit = new QLineEdit(m_parent);

    QPushButton* browse_button = new QPushButton("Browse", m_parent);
    browse_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    connect(browse_button, &QPushButton::clicked, [=]() { emit slot_open_file_picker(QString::fromStdString(name)); });

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    if (input_widget_visible)
    {
        QHBoxLayout* layout = new QHBoxLayout();
        layout->setSpacing(6);
        layout->addWidget(line_edit);
        layout->addWidget(browse_button);
        m_form_layout->addRow(create_label(metadata), layout);
    }
    else
    {
        line_edit->hide();
        browse_button->hide();
    }

    std::unique_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));
    widget_proxy->set(metadata.strings().get<std::string>("value"));

    return widget_proxy;
}

void EntityEditor::slot_rebuild_form()
{
    rebuild_form(get_values());

    emit signal_applied(get_values());
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
    const Dictionary& metadata = get_input_metadata(widget_name.toStdString());

    EntityBrowserWindow* browser_window =
        new EntityBrowserWindow(
            m_parent,
            metadata.get<std::string>("label"));

    const Dictionary& entity_types = metadata.dictionaries().get("entity_types");

    for (const_each<StringDictionary> i = entity_types.strings(); i; ++i)
    {
        const std::string entity_type = i->key();
        const std::string entity_label = i->value<std::string>();
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
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());
    widget_proxy->set(entity_name.toStdString());
    widget_proxy->emit_signal_changed();

    // Close the entity browser.
    qobject_cast<QWidget*>(sender()->parent())->close();
}

void EntityEditor::slot_open_color_picker(const QString& widget_name)
{
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());

    const Dictionary& metadata = get_input_metadata(widget_name.toStdString());
    const std::string wr_widget = metadata.get<std::string>("wavelength_range_widget");
    const IInputWidgetProxy* wr_widget_proxy = m_widget_proxies.get(wr_widget);
    const Color3d initial_color =
        wr_widget_proxy
            ? ColorPickerProxy::get_color_from_string(widget_proxy->get(), wr_widget_proxy->get())
            : ColorPickerProxy::get_color_from_string(widget_proxy->get());

    QColorDialog* dialog =
        new QColorDialog(
            color_to_qcolor(initial_color),
            m_parent);
    dialog->setWindowTitle("Pick Color");
    dialog->setOptions(QColorDialog::DontUseNativeDialog);

    ForwardColorChangedSignal* forward_signal =
        new ForwardColorChangedSignal(dialog, widget_name, color_to_qcolor(initial_color));
    connect(
        dialog, SIGNAL(currentColorChanged(const QColor&)),
        forward_signal, SLOT(slot_color_changed(const QColor&)));
    connect(
        forward_signal, SIGNAL(signal_color_changed(const QString&, const QColor&)),
        SLOT(slot_color_changed(const QString&, const QColor&)));
    connect(
        dialog, SIGNAL(rejected()),
        forward_signal, SLOT(slot_color_reset()));
    connect(
        forward_signal, SIGNAL(signal_color_reset(const QString&, const QColor&)),
        SLOT(slot_color_changed(const QString&, const QColor&)));

    dialog->exec();
}

void EntityEditor::slot_color_changed(const QString& widget_name, const QColor& color)
{
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());
    widget_proxy->set(to_string(qcolor_to_color<Color3d>(color)));
    widget_proxy->emit_signal_changed();
}

void EntityEditor::slot_open_file_picker(const QString& widget_name)
{
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());

    const Dictionary& metadata = get_input_metadata(widget_name.toStdString());

    if (metadata.get<std::string>("file_picker_mode") == "open")
    {
        const QString file_picker_type = metadata.get<QString>("file_picker_type");
        const QString filter =
            file_picker_type == "image" ? get_oiio_image_files_filter() :
            file_picker_type == "project" ? get_project_files_filter() :
            QString();

        const QString settings_key =
            file_picker_type == "image" ? SETTINGS_FILE_DIALOG_OIIO_TEXTURES :
            SETTINGS_FILE_DIALOG_ENTITIES;

        const bf::path project_root_path = bf::path(m_project.get_path()).parent_path();
        const bf::path file_path = absolute(widget_proxy->get(), project_root_path);
        const bf::path file_root_path = file_path.parent_path();

        QFileDialog::Options options;

        QString filepath =
            get_open_filename(
                m_parent,
                "Open...",
                filter,
                m_settings,
                settings_key,
                options);

        if (!filepath.isEmpty())
        {
            widget_proxy->set(filepath.toStdString());
            widget_proxy->emit_signal_changed();
        }
    }
}

void EntityEditor::slot_apply()
{
    emit signal_applied(get_values());
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/project/moc_cpp_entityeditor.cxx"
