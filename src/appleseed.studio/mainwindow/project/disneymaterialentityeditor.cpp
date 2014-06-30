
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
#include "disneymaterialentityeditor.h"

// appleseed.studio headers.
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

// Ui definition headers.
#include "ui_disneymaterialentityeditor.h"
#include "ui_expressioneditorwindow.h"

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

namespace
{
    class LayerWidget
      : public QFrame
    {
        Q_OBJECT

      public:
        LayerWidget(QVBoxLayout* form_layout, QWidget* parent = 0)
          : QFrame(parent)
          , m_form_layout(form_layout)
          , m_is_folded(false)
        {
            setObjectName("layer");

            m_spacer = new QWidget();
            QHBoxLayout* spacer_layout = new QHBoxLayout(m_spacer);
            spacer_layout->setSpacing(0);

            m_layout = new QVBoxLayout(this);
            m_form_layout->insertWidget(m_form_layout->count() - 2, this);

            QWidget *button_box = new QWidget(this);
            QHBoxLayout *button_box_layout = new QHBoxLayout(button_box);
            button_box_layout->setSpacing(0);
            button_box_layout->setMargin(0);
            m_layout->addWidget(button_box);

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

        void mousePressEvent(QMouseEvent* event)
        {
            for (int i=1; i<m_form_layout->count()-2; ++i)
            {
                QLayoutItem* layout_item = m_form_layout->itemAt(i);
                QWidget* widget = layout_item->widget();
                if (widget->objectName() == "selected_layer")
                {
                    widget->setObjectName("layer");
                    style()->unpolish(widget);
                    style()->polish(widget);
                    break;
                }
            }
            setObjectName("selected_layer");
            style()->unpolish(this);
            style()->polish(this);
        }

        void mouseDoubleClickEvent(QMouseEvent* event)
        {
            fold_layer();
        }

        QVBoxLayout* get_layout()
        {
            return m_layout;
        }
      
      private:
        void fold_layer()
        {
            if (m_is_folded)
            {
                m_layout->removeWidget(m_spacer);
                m_spacer->hide();
                m_fold_button->setIcon(m_fold_arrow_disabled);
            }

            for (int i=2; i<m_layout->count(); ++i)
            {
                QLayout* vertical_layout = m_layout->itemAt(i)->layout();
                for (int j=0; j<vertical_layout->count(); ++j)
                {
                    QWidget* widget = vertical_layout->itemAt(j)->widget();
                    m_is_folded ? widget->show() : widget->hide();
                }
            }
            
            if (!m_is_folded)
            {
                m_layout->addWidget(m_spacer);
                m_spacer->show();
                m_fold_button->setIcon(m_fold_arrow_enabled);
            }

            m_is_folded = !m_is_folded;
        }

      private slots:
        void slot_delete_layer() 
        {
            delete this;
        }

        void slot_move_layer_up()
        {
            for (int i=1; i<m_form_layout->count(); ++i)
            {
                QLayoutItem* layout_item = m_form_layout->itemAt(i);
                if (this == layout_item->widget())
                {
                    if (i > 1)
                    {
                        m_form_layout->takeAt(i);
                        m_form_layout->insertWidget(i-1, this);
                    }
                    break;
                }
            }
        }

        void slot_move_layer_down()
        {
            for (int i=1; i<m_form_layout->count(); ++i)
            {
                QLayoutItem* layout_item = m_form_layout->itemAt(i);
                if (this == layout_item->widget())
                {
                    if (i < m_form_layout->count()-3)
                    {
                        m_form_layout->takeAt(i);
                        m_form_layout->insertWidget(i+1, this);
                    }
                    break;
                }
            }
        }

        void slot_fold()
        {
            fold_layer();
        }

      private:
        QVBoxLayout*    m_layout;
        QVBoxLayout*    m_form_layout;
        QWidget*        m_spacer;
        QToolButton*    m_fold_button;
        bool            m_is_folded;
        QIcon           m_fold_arrow_enabled;
        QIcon           m_fold_arrow_disabled;
    };
}


DisneyMaterialEntityEditor::DisneyMaterialEntityEditor(
    QWidget*                                parent,
    const Project&                          project,
    auto_ptr<EntityEditor::IFormFactory>    form_factory,
    auto_ptr<EntityEditor::IEntityBrowser>  entity_browser,
    const Dictionary&                       values)
  : QWidget(parent)
  , m_project(project)
  , m_ui(new Ui::DisneyMaterialEntityEditor())
  , m_num_created_layers(0)
  , m_selected_layer_widget(0)
  , m_color_picker_signal_mapper(new QSignalMapper(this))
  , m_file_picker_signal_mapper(new QSignalMapper(this))
  , m_expression_editor_signal_mapper(new QSignalMapper(this))
  , m_line_edit_signal_mapper(new QSignalMapper(this))
{
    m_ui->setupUi(this);

    setWindowTitle(QString::fromStdString("Create/Edit Disney Material."));
    setWindowFlags(Qt::Tool);
    setAttribute(Qt::WA_DeleteOnClose);

    m_parent = m_ui->scrollarea_contents;
    create_form_layout();
    create_connections();
}

DisneyMaterialEntityEditor::~DisneyMaterialEntityEditor()
{
    delete m_ui;
}

void DisneyMaterialEntityEditor::slot_add_layer()
{
    add_layer();
}

void DisneyMaterialEntityEditor::slot_open_color_picker(const QString& widget_name)
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

void DisneyMaterialEntityEditor::slot_color_changed(const QString& widget_name, const QColor& color)
{
    IInputWidgetProxy* proxy = m_widget_proxies.get(widget_name.toStdString());
    proxy->set(ColorExpressionProxy::qcolor_to_expression(color));
    proxy->emit_signal_changed();
}

void DisneyMaterialEntityEditor::slot_open_file_picker(const QString& widget_name)
{
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());

    const filesystem::path project_root_path = filesystem::path(m_project.get_path()).parent_path();
    const filesystem::path file_path = absolute(widget_proxy->get(), project_root_path);
    const filesystem::path file_root_path = file_path.parent_path();

    QFileDialog::Options options;
    QString selected_filter;

    const QString file_picker_filter("PNG (*.png)");
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
        QString native_path = QDir::toNativeSeparators(filepath);
        widget_proxy->set(texture_to_expression(native_path));
    }
}

void DisneyMaterialEntityEditor::slot_open_expression_editor(const QString& widget_name)
{
    IInputWidgetProxy* proxy = m_widget_proxies.get(widget_name.toStdString());
    string expression = proxy->get();

    // If widget has a double convert it to valid expression.
    double value;
    istringstream sstream(expression);
    if ((sstream >> value))
        expression = "$value = " + sstream.str() + ";";

    ExpressionEditorWindow* expression_editor_window = new ExpressionEditorWindow(widget_name, expression, m_parent);

    connect(
        expression_editor_window, SIGNAL(signal_expression_applied(const QString&, const QString&)),
        SLOT(slot_expression_changed(const QString&, const QString&)));

    expression_editor_window->show();
}

void DisneyMaterialEntityEditor::slot_expression_changed(
    const QString& widget_name,
    const QString& expression)
{
    IInputWidgetProxy* proxy = m_widget_proxies.get(widget_name.toStdString());
    proxy->set(expression.toStdString());
    proxy->emit_signal_changed();
}

void DisneyMaterialEntityEditor::slot_line_edit_changed(const QString& widget_name)
{
    IInputWidgetProxy* proxy = m_widget_proxies.get(widget_name.toStdString());

    vector<string> widget_tokens;
    tokenize(widget_name.toStdString(), ";", widget_tokens);
    string initial_layer_name = widget_tokens[0];
    string layer_name = m_renames.get(initial_layer_name.c_str());
    string parameter = widget_tokens[1];

    // Handle layer rename.
    if (parameter == "layer_name")
    {
        Dictionary old_layer_params = m_params.dictionary(layer_name);
        string new_layer_name = proxy->get();
        m_renames.insert(initial_layer_name, new_layer_name);
        m_params.dictionaries().remove(layer_name.c_str());
        m_params.insert(new_layer_name, old_layer_params);
        layer_name = new_layer_name;

        // Debug info.
        std::cout << "layer_renames ---- " << std::endl;
        for (const_each<StringDictionary> i = m_renames.strings(); i; ++i)
        {
            std::cout << i->name() << " : " << i->value() << std::endl;
        }
        std::cout << "-------------------" << std::endl;
    }

    // Handle generic line edit change.
    Dictionary& layer_params = m_params.dictionary(layer_name);
    layer_params.insert(parameter, proxy->get());

    // Debug info.
    std::cout << "layer_name" <<  layer_name << "----" << std::endl;
    for (const_each<StringDictionary> i = layer_params.strings(); i; ++i)
    {
        std::cout << i->name() << " : " << i->value() << std::endl;
    }
    std::cout << "--------------------------" << std::endl;
}

void DisneyMaterialEntityEditor::create_connections()
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

void DisneyMaterialEntityEditor::create_buttons_connections(const QString& widget_name)
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

void DisneyMaterialEntityEditor::create_form_layout()
{
    m_form_layout = new QVBoxLayout(m_parent);
    m_form_layout->setSpacing(5);

    // Global material parameters.
    add_material_parameters();

    // New layer button.
    QIcon add_icon = QIcon(":/widgets/big_add.png");
    QPushButton *add_layer_button = new QPushButton(add_icon, "   Add new layer");
    add_layer_button->setObjectName("add_layer");
    m_form_layout->addWidget(add_layer_button);
    connect(add_layer_button, SIGNAL(clicked()), this, SLOT(slot_add_layer()));

    // Stretch at the end.
    m_form_layout->addStretch(1);

    // Default layer.
    add_layer();
}

void DisneyMaterialEntityEditor::create_parameters_layout()
{
    m_group_widget = new QWidget(this);
    m_group_widget->setObjectName("disney");
    m_group_layout = new QVBoxLayout(m_group_widget);

    m_form_layout->addWidget(m_group_widget);
}

void DisneyMaterialEntityEditor::create_layer_layout()
{
    LayerWidget* layer_widget = new LayerWidget(m_form_layout);
    m_group_layout = layer_widget->get_layout();
    m_group_widget = layer_widget;

    m_layers_widgets.push_back(m_group_widget);
}

string DisneyMaterialEntityEditor::unique_layer_name()
{
    string layer_name = "layer" + to_string(++m_num_created_layers);
    return layer_name;
}

string DisneyMaterialEntityEditor::texture_to_expression(const QString& expression)
{
    QString texture_expression = QString("texture([%1], $u, $v)")
            .arg(expression);
    return texture_expression.toStdString();
}

void DisneyMaterialEntityEditor::create_text_input_widgets(
    const Dictionary& parameters,
    const string& group_name)
{
    const string label_name = parameters.get<string>("label") + ":";
    const string parameter_name = parameters.get<string>("name");
    const string value = parameters.get<string>("default");
    QLabel* label = new QLabel(label_name.c_str(), m_group_widget);
    m_line_edit = new LineEditForwarder(value.c_str(), m_group_widget);

    string widget_name = group_name + ";" + parameter_name;
    connect(m_line_edit, SIGNAL(signal_text_changed()), m_line_edit_signal_mapper, SLOT(map()));
    m_line_edit_signal_mapper->setMapping(m_line_edit, QString::fromStdString(widget_name));

    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(m_line_edit));
    m_widget_proxies.insert(widget_name, widget_proxy);

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(label);
    layout->addWidget(m_line_edit);

    m_group_layout->addLayout(layout);
}

void DisneyMaterialEntityEditor::create_color_input_widgets(
    const Dictionary& parameters,
    const string& group_name)
{
    const string label_name = parameters.get<string>("label") + ":";
    const string parameter_name = parameters.get<string>("name");
    const string value = parameters.get<string>("default");

    QLabel* label = new QLabel(label_name.c_str(), m_group_widget);
    m_line_edit = new LineEditForwarder(value.c_str(), m_group_widget);

    QString name = QString::fromStdString(group_name + ";" + parameter_name);
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
    layout->addWidget(label);
    layout->addWidget(m_line_edit);
    layout->addWidget(picker_button);
    layout->addWidget(m_texture_button);
    layout->addWidget(m_expression_button);

    m_group_layout->addLayout(layout);
}

void DisneyMaterialEntityEditor::create_colormap_input_widgets(
    const foundation::Dictionary& parameters,
    const string& group_name)
{
    const string label_name = parameters.get<string>("label") + ":";
    const string parameter_name = parameters.get<string>("name");
    const string value = parameters.get<string>("default");

    QLabel* label = new QLabel(label_name.c_str(), m_group_widget);
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

    QString name = QString::fromStdString(group_name + ";" + parameter_name);
    m_texture_button = new QPushButton("Texture", m_group_widget);
    m_texture_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    m_expression_button = new QPushButton("Expression", m_group_widget);
    m_expression_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);

    create_buttons_connections(name);

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(label);
    layout->addWidget(m_line_edit);
    layout->addWidget(slider);
    layout->addWidget(m_texture_button);
    layout->addWidget(m_expression_button);

    m_group_layout->addLayout(layout);
}

void DisneyMaterialEntityEditor::add_material_parameters()
{
    typedef std::vector<foundation::Dictionary> InputMetadataCollection;
    InputMetadataCollection metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "name")
            .insert("label", "Name")
            .insert("type", "text")
            .insert("default", "disney_material1"));

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha_mask")
            .insert("label", "Alpha Mask")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "emission")
            .insert("label", "Emission")
            .insert("type", "colormap")
            .insert("default", "0"));

    create_parameters_layout();
    for (const_each<InputMetadataCollection> i = metadata; i; ++i)
    {
        const string type = i->get<string>("type");

        if (type == "colormap")
            create_colormap_input_widgets(*i, "base");
        else if (type == "text")
            create_text_input_widgets(*i, "base");
    }
}

void DisneyMaterialEntityEditor::add_layer()
{
    typedef std::vector<foundation::Dictionary> InputMetadataCollection;
    InputMetadataCollection metadata;
    create_layer_layout();
    const string layer_name = unique_layer_name();

    metadata.push_back(
        Dictionary()
            .insert("name", "layer_name")
            .insert("label", "Layer name")
            .insert("type", "text")
            .insert("default", layer_name));

    metadata.push_back(
        Dictionary()
            .insert("name", "mask")
            .insert("label", "Mask")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "base_color")
            .insert("label", "Base Color")
            .insert("type", "color")
            .insert("default", "[0.0, 0.0, 0.0]"));

    metadata.push_back(
        Dictionary()
            .insert("name", "subsurface")
            .insert("label", "Subsurface")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "metallic")
            .insert("label", "Metallic")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular")
            .insert("label", "Specular")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular_tint")
            .insert("label", "Specular tint")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "anisotropic")
            .insert("label", "Anisotropic")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "roughness")
            .insert("label", "Roughness")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "sheen")
            .insert("label", "Sheen")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "shin_tint")
            .insert("label", "Shin tint")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clearcoat")
            .insert("label", "Clearcoat")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clearcoat_gloss")
            .insert("label", "Clearcoat gloss")
            .insert("type", "colormap")
            .insert("default", "0"));

    Dictionary layer_params;
    layer_params.insert("layer_number", m_num_created_layers);

    for (const_each<InputMetadataCollection> i = metadata; i; ++i)
    {
        layer_params.insert(i->get<string>("name"), i->get<string>("default"));
        const string type = i->get<string>("type");

        if (type == "color")
            create_color_input_widgets(*i, layer_name);
        else if (type == "colormap")
            create_colormap_input_widgets(*i, layer_name);
        else if (type == "text")
            create_text_input_widgets(*i, layer_name);
    }
    m_params.insert(layer_name, layer_params);
    m_renames.insert(layer_name, layer_name);
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/project/moc_cpp_disneymaterialentityeditor.cxx"
