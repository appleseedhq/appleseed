
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
#include "disneymaterialcustomui.h"

// appleseed.studio headers.
#include "mainwindow/project/disneymateriallayerui.h"
#include "mainwindow/project/entityeditorutils.h"
#include "mainwindow/project/expressioneditorwindow.h"
#include "utility/doubleslider.h"
#include "utility/interop.h"
#include "utility/miscellaneous.h"
#include "utility/mousewheelfocuseventfilter.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// Standard headers.
#include <algorithm>
#include <sstream>

// Qt headers.
#include <QColorDialog>
#include <QFileDialog>
#include <QFormLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QSignalMapper>
#include <QString>
#include <QVBoxLayout>
#include <Qt>
#include <QToolButton>

// boost headers.
#include "boost/algorithm/string.hpp"
#include "boost/algorithm/string/split.hpp"
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

DisneyMaterialCustomUI::DisneyMaterialCustomUI(
    const Project&          project,
    DictionaryArray         layer_metadata)
  : m_project(project)
  , m_layer_metadata(layer_metadata)
  , m_parent(0)
  , m_num_created_layers(0)
  , m_selected_layer_widget(0)
  , m_color_picker_signal_mapper(new QSignalMapper(this))
  , m_file_picker_signal_mapper(new QSignalMapper(this))
  , m_expression_editor_signal_mapper(new QSignalMapper(this))
  , m_line_edit_signal_mapper(new QSignalMapper(this))
{
}

void DisneyMaterialCustomUI::create_custom_widgets(
    QVBoxLayout*        layout,
    const Dictionary&   values)
{
    m_parent = layout->parentWidget();

    // Resize if has EntityEditorWidget ancestor.
    QWidget* parent = m_parent;
    while (parent != 0)
    {
        if (parent->objectName().toStdString() == "EntityEditorWindow")
        {
            parent->resize(500, 630);
            break;
        }
        parent = parent->parentWidget();
    }

    m_form_layout = layout;
    m_form_layout->setSpacing(5);

    // Copy values.
    m_values = values;

    // New layer button.
    // Use blanks for spacing between icon and text, as there does not appear to be a better option.
    QIcon add_icon = QIcon(":/widgets/big_add.png");
    QPushButton* add_layer_button = new QPushButton(add_icon, "   Add new layer");
    add_layer_button->setObjectName("add_material_editor_layer");
    m_form_layout->addWidget(add_layer_button);
    connect(add_layer_button, SIGNAL(clicked()), this, SLOT(slot_add_layer()));

    // Stretch at the end.
    m_form_layout->addStretch(1);

    // Build layers.
    for (size_t i = 1; i <= m_values.dictionaries().size(); ++i)
    {
        for (const_each<DictionaryDictionary> d = m_values.dictionaries(); d; ++d)
        {
            const size_t layer_number = d->value().get<size_t>("layer_number");
            if (layer_number == i)
                add_layer(false, d->value());
        }
    }

    // Recreate connections.
    create_connections();
}

Dictionary DisneyMaterialCustomUI::get_values() const
{
    return m_values;
}

void DisneyMaterialCustomUI::slot_add_layer()
{
    add_layer(true);
}

void DisneyMaterialCustomUI::slot_open_color_picker(const QString& widget_name)
{
    IInputWidgetProxy* proxy = m_widget_proxies.get(widget_name.toStdString());
    const string color_expression = proxy->get();
    const QColor initial_color = ColorExpressionProxy::expression_to_qcolor(color_expression);

    QColorDialog* dialog =
        new QColorDialog(
            initial_color,
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

void DisneyMaterialCustomUI::slot_color_changed(const QString& widget_name, const QColor& color)
{
    IInputWidgetProxy* proxy = m_widget_proxies.get(widget_name.toStdString());
    proxy->set(ColorExpressionProxy::qcolor_to_expression(color));
    proxy->emit_signal_changed();
}

void DisneyMaterialCustomUI::slot_open_file_picker(const QString& widget_name)
{
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());

    const filesystem::path project_root_path = filesystem::path(m_project.get_path()).parent_path();
    const filesystem::path file_path = absolute(widget_proxy->get(), project_root_path);
    const filesystem::path file_root_path = file_path.parent_path();

    QFileDialog::Options options;
    QString selected_filter;

    // TODO: refactor file_picker_filter extensions
    const QString file_picker_filter("OpenEXR (*.exr);;PNG (*.png)");
    QString filepath =
        QFileDialog::getOpenFileName(
            m_parent,
            "Open...",
            QString::fromStdString(file_root_path.string()),
            file_picker_filter,
            &selected_filter,
            options);

    if (!filepath.isEmpty())
    {
        const QString native_path = QDir::toNativeSeparators(filepath);
        widget_proxy->set(texture_to_expression(native_path));
    }
}

void DisneyMaterialCustomUI::slot_open_expression_editor(const QString& widget_name)
{
    IInputWidgetProxy* proxy = m_widget_proxies.get(widget_name.toStdString());
    string expression = proxy->get();

    // If widget has a double convert it to valid expression.
    double value;
    istringstream sstream(expression);
    if ((sstream >> value))
        expression = sstream.str();

    ExpressionEditorWindow* expression_editor_window = new ExpressionEditorWindow(widget_name, expression, m_parent);

    connect(
        expression_editor_window, SIGNAL(signal_expression_applied(const QString&, const QString&)),
        SLOT(slot_expression_changed(const QString&, const QString&)));

    expression_editor_window->show();
}

void DisneyMaterialCustomUI::slot_expression_changed(
    const QString& widget_name,
    const QString& expression)
{
    IInputWidgetProxy* proxy = m_widget_proxies.get(widget_name.toStdString());
    proxy->set(expression.toStdString());
    proxy->emit_signal_changed();
}

void DisneyMaterialCustomUI::slot_line_edit_changed(const QString& widget_name)
{
    IInputWidgetProxy* proxy = m_widget_proxies.get(widget_name.toStdString());

    vector<string> widget_tokens;
    tokenize(widget_name.toStdString(), ";", widget_tokens);
    const string initial_layer_name = widget_tokens[0];
    const string parameter = widget_tokens[1];
    string layer_name = m_renames.get(initial_layer_name.c_str());

    // Handle layer rename.
    if (parameter == "layer_name")
    {
        Dictionary old_layer_params = m_values.dictionary(layer_name);
        string new_layer_name = proxy->get();
        m_renames.insert(initial_layer_name, new_layer_name);
        m_values.dictionaries().remove(layer_name);
        m_values.insert(new_layer_name, old_layer_params);
        layer_name = new_layer_name;
    }

    // Handle generic line edit change.
    Dictionary& layer_params = m_values.dictionary(layer_name);
    layer_params.insert(parameter, proxy->get());

    emit_signal_custom_applied();
}

void DisneyMaterialCustomUI::create_connections()
{
    connect(
        m_color_picker_signal_mapper, SIGNAL(mapped(const QString&)),
        SLOT(slot_open_color_picker(const QString&)));

    connect(
        m_file_picker_signal_mapper, SIGNAL(mapped(const QString&)),
        SLOT(slot_open_file_picker(const QString&)));

    connect(
        m_expression_editor_signal_mapper, SIGNAL(mapped(const QString&)),
        SLOT(slot_open_expression_editor(const QString&)));

    connect(
        m_line_edit_signal_mapper, SIGNAL(mapped(const QString&)),
        SLOT(slot_line_edit_changed(const QString&)));
}

void DisneyMaterialCustomUI::create_buttons_connections(const QString& widget_name)
{
    connect(m_line_edit, SIGNAL(signal_text_changed()), m_line_edit_signal_mapper, SLOT(map()));
    m_line_edit_signal_mapper->setMapping(m_line_edit, widget_name);

    connect(m_texture_button, SIGNAL(clicked()), m_file_picker_signal_mapper, SLOT(map()));
    m_file_picker_signal_mapper->setMapping(m_texture_button, widget_name);

    connect(m_expression_button, SIGNAL(clicked()), m_expression_editor_signal_mapper, SLOT(map()));
    m_expression_editor_signal_mapper->setMapping(m_expression_button, widget_name);

    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(m_line_edit));
    m_widget_proxies.insert(widget_name.toStdString(), widget_proxy);
}

void DisneyMaterialCustomUI::create_parameters_layout()
{
    m_group_widget = new QWidget();
    m_group_widget->setObjectName("disney");
    m_group_layout = new QFormLayout(m_group_widget);

    m_form_layout->addWidget(m_group_widget);
}

void DisneyMaterialCustomUI::create_layer_layout(const string& layer_name)
{
    m_last_layer = new DisneyMaterialLayerUI(layer_name, this, m_form_layout);
    m_group_layout = m_last_layer->get_layout();
    m_group_widget = m_last_layer;

    m_layers_widgets.push_back(m_group_widget);
}

string DisneyMaterialCustomUI::unique_layer_name()
{
    return "layer" + to_string(++m_num_created_layers);
}

string DisneyMaterialCustomUI::texture_to_expression(const QString& expression)
{
    QString texture_expression = QString("texture(\"%1\", $u, $v)")
            .arg(expression);
    return texture_expression.toStdString();
}

void DisneyMaterialCustomUI::create_text_input_widgets(
    const Dictionary&   parameters,
    const string&       group_name)
{
    const string label_name = parameters.get<string>("label") + ":";
    const string parameter_name = parameters.get<string>("name");
    const string value = parameters.get<string>("default");
    m_line_edit = new LineEditForwarder(value.c_str(), m_group_widget);

    const QString name = QString::fromStdString(group_name + ";" + parameter_name);
    connect(m_line_edit, SIGNAL(signal_text_changed()), m_line_edit_signal_mapper, SLOT(map()));
    m_line_edit_signal_mapper->setMapping(m_line_edit, name);

    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(m_line_edit));
    m_widget_proxies.insert(name.toStdString(), widget_proxy);

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(m_line_edit);

    m_group_layout->addRow(QString::fromStdString(label_name), layout);
}

void DisneyMaterialCustomUI::create_color_input_widgets(
    const Dictionary&   parameters,
    const string&       group_name)
{
    const string label_name = parameters.get<string>("label") + ":";
    const string parameter_name = parameters.get<string>("name");
    const string value = parameters.get<string>("default");

    m_line_edit = new LineEditForwarder(value.c_str(), m_group_widget);

    const QString name = QString::fromStdString(group_name + ";" + parameter_name);
    QToolButton* picker_button = new QToolButton(m_group_widget);
    picker_button->setObjectName("ColorPicker");
    ColorPickerProxy picker_proxy(m_line_edit, picker_button);

    m_texture_button = new QPushButton("Texture", m_group_widget);
    m_texture_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    m_expression_button = new QPushButton("Expression", m_group_widget);
    m_expression_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);

    QString picker_name = QString::fromStdString(group_name + "_color_expression");
    connect(picker_button, SIGNAL(clicked()), m_color_picker_signal_mapper, SLOT(map()));
    m_color_picker_signal_mapper->setMapping(picker_button, picker_name);
    auto_ptr<IInputWidgetProxy> widget_proxy(new ColorExpressionProxy(m_line_edit, picker_button));
    widget_proxy->set(value);
    m_widget_proxies.insert(picker_name.toStdString(), widget_proxy);

    create_buttons_connections(name);

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(m_line_edit);
    layout->addWidget(picker_button);
    layout->addWidget(m_texture_button);
    layout->addWidget(m_expression_button);

    m_group_layout->addRow(QString::fromStdString(label_name), layout);
}

void DisneyMaterialCustomUI::create_colormap_input_widgets(
    const Dictionary&   parameters,
    const string&       group_name)
{
    const string label_name = parameters.get<string>("label") + ":";
    const string parameter_name = parameters.get<string>("name");
    const string value = parameters.get<string>("default");

    m_line_edit = new LineEditForwarder(value.c_str(), m_group_widget);

    const double min_value = 0.0;
    const double max_value = 1.0;

    DoubleSlider* slider = new DoubleSlider(Qt::Horizontal, m_group_widget);
    slider->setRange(min_value, max_value);
    slider->setPageStep((max_value - min_value) / 10.0);
    slider->setMaximumWidth(100);

    new MouseWheelFocusEventFilter(slider);

    // Connect the line edit and the slider together.
    LineEditDoubleSliderAdaptor* adaptor =
        new LineEditDoubleSliderAdaptor(m_line_edit, slider);
    connect(
        slider, SIGNAL(valueChanged(const double)),
        adaptor, SLOT(slot_set_line_edit_value(const double)));
    connect(
        m_line_edit, SIGNAL(textChanged(const QString&)),
        adaptor, SLOT(slot_set_slider_value(const QString&)));
    connect(
        m_line_edit, SIGNAL(editingFinished()),
        adaptor, SLOT(slot_apply_slider_value()));

    const QString name = QString::fromStdString(group_name + ";" + parameter_name);
    m_texture_button = new QPushButton("Texture", m_group_widget);
    m_texture_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    m_expression_button = new QPushButton("Expression", m_group_widget);
    m_expression_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);

    create_buttons_connections(name);

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(m_line_edit);
    layout->addWidget(slider);
    layout->addWidget(m_texture_button);
    layout->addWidget(m_expression_button);

    m_group_layout->addRow(QString::fromStdString(label_name), layout);
}

void DisneyMaterialCustomUI::add_layer(const bool update, const Dictionary& parameters)
{
    string layer_name = unique_layer_name();

    if (parameters.strings().exist("layer_name"))
        layer_name = parameters.strings().get<string>("layer_name");

    // Change layer name until is certainly unique.
    while (update && m_values.dictionaries().exist(layer_name))
        layer_name = unique_layer_name();

    create_layer_layout(layer_name);

    Dictionary layer_params;
    if (update)
        layer_params.insert("layer_number", m_values.dictionaries().size()+1);

    for (size_t i = 0; i < m_layer_metadata.size(); ++i)
    {
        Dictionary metadata = m_layer_metadata[i];
        const string type = metadata.get<string>("type");
        const string name = metadata.get<string>("name");

        // Change default value to existing value.
        string default_value = parameters.strings().exist(name) ?
            parameters.get(name.c_str()) : metadata.get<string>("default");

        // Change default name in metadata
        if (name == "layer_name")
            default_value = layer_name;

        metadata.insert("default", default_value);

        if (type == "colormap")
        {
            if (metadata.dictionaries().size() > 0)
            {
                const Dictionary entity_types = metadata.dictionaries().get("entity_types");
                if (entity_types.strings().exist("color"))
                    create_color_input_widgets(metadata, layer_name);
                else
                    create_colormap_input_widgets(metadata, layer_name);
            }
            else
            {
                create_colormap_input_widgets(metadata, layer_name);
            }
        }
        else if (type == "text")
            create_text_input_widgets(metadata, layer_name);

        if (update)
            layer_params.insert(name, default_value);
    }

    if (update)
        m_values.insert(layer_name, layer_params);
    m_renames.insert(layer_name, layer_name);

    // Add default folded value.
    Dictionary& current_layer = m_values.dictionaries().get(layer_name);
    if (!current_layer.strings().exist("folded"))
        current_layer.insert("folded", false);

    m_last_layer->fold_layer(false);

    emit_signal_custom_applied();
}

}       // namespace studio
}       // namespace appleseed
