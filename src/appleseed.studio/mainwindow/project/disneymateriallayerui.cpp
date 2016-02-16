
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
#include "disneymateriallayerui.h"

// appleseed.studio headers.
#include "mainwindow/project/expressioneditorwindow.h"
#include "mainwindow/project/tools.h"
#include "utility/doubleslider.h"
#include "utility/interop.h"
#include "utility/miscellaneous.h"
#include "utility/mousewheelfocuseventfilter.h"
#include "utility/settingskeys.h"

// appleseed.renderer headers.
#include "renderer/api/material.h"
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/searchpaths.h"

// Qt headers.
#include <QColor>
#include <QColorDialog>
#include <QDir>
#include <QFileInfo>
#include <QFont>
#include <QFormLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QSignalMapper>
#include <Qt>
#include <QToolButton>
#include <QVariant>
#include <QVBoxLayout>
#include <QWidget>

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <cstddef>

using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

DisneyMaterialLayerUI::DisneyMaterialLayerUI(
    QWidget*            parent,
    const Project&      project,
    ParamArray&         settings,
    const Dictionary&   values)
  : QFrame(parent)
  , m_project(project)
  , m_settings(settings)
  , m_input_metadata(DisneyMaterialLayer::get_input_metadata())
  , m_fold_icon(":/widgets/layer_fold.png")
  , m_unfold_icon(":/widgets/layer_unfold.png")
  , m_color_picker_signal_mapper(new QSignalMapper(this))
  , m_file_picker_signal_mapper(new QSignalMapper(this))
  , m_expression_editor_signal_mapper(new QSignalMapper(this))
  , m_is_folded(values.strings().exist("layer_folded") && values.get<bool>("layer_folded"))
{
    setProperty("hasFrame", true);

    create_layer_ui();
    create_input_widgets(values);

    connect(
        m_color_picker_signal_mapper, SIGNAL(mapped(const QString&)),
        SLOT(slot_open_color_picker(const QString&)));

    connect(
        m_file_picker_signal_mapper, SIGNAL(mapped(const QString&)),
        SLOT(slot_open_file_picker(const QString&)));

    connect(
        m_expression_editor_signal_mapper, SIGNAL(mapped(const QString&)),
        SLOT(slot_open_expression_editor(const QString&)));

    if (m_is_folded)
        fold();
}

void DisneyMaterialLayerUI::create_layer_ui()
{
    // Top-level layout of the layer
    QVBoxLayout* layout = new QVBoxLayout(this);

    // Container widget for the layer buttons.
    QWidget* button_box = new QWidget(this);
    QHBoxLayout* button_box_layout = new QHBoxLayout(button_box);
    button_box_layout->setSpacing(0);
    button_box_layout->setMargin(0);
    layout->addWidget(button_box);

    // Fold button.
    m_fold_unfold_button = new QToolButton(button_box);
    m_fold_unfold_button->setObjectName("widget");
    m_fold_unfold_button->setIcon(m_fold_icon);
    button_box_layout->addWidget(m_fold_unfold_button);
    connect(m_fold_unfold_button, SIGNAL(clicked()), this, SLOT(slot_fold_unfold_layer()));

    // Layer header (only visible when the layer is folded).
    m_header_widget = new QWidget(button_box);
    m_header_widget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
    m_header_layout = new QFormLayout(m_header_widget);
    m_header_layout->setSpacing(7);
    button_box_layout->addWidget(m_header_widget);

    // Move Up button.
    QToolButton* up_button = new QToolButton(button_box);
    up_button->setObjectName("widget");
    up_button->setIcon(QIcon(":widgets/layer_move_up.png"));
    button_box_layout->addWidget(up_button);
    connect(up_button, SIGNAL(clicked()), this, SLOT(slot_move_layer_up()));

    // Move Down button.
    QToolButton* down_button = new QToolButton(button_box);
    down_button->setObjectName("widget");
    down_button->setIcon(QIcon(":widgets/layer_move_down.png"));
    button_box_layout->addWidget(down_button);
    connect(down_button, SIGNAL(clicked()), this, SLOT(slot_move_layer_down()));

    // Remove button.
    QToolButton* remove_button = new QToolButton(button_box);
    remove_button->setObjectName("widget");
    remove_button->setIcon(QIcon(":/widgets/layer_remove.png"));
    button_box_layout->addWidget(remove_button);
    connect(remove_button, SIGNAL(clicked()), this, SLOT(slot_delete_layer()));

    // Container widget for the layer contents.
    m_content_widget = new QWidget(this);
    m_content_layout = new QFormLayout(m_content_widget);
    m_content_layout->setLabelAlignment(Qt::AlignRight);
    m_content_layout->setSpacing(10);
    m_content_layout->setFieldGrowthPolicy(QFormLayout::AllNonFixedFieldsGrow);
    layout->addWidget(m_content_widget);
}

void DisneyMaterialLayerUI::create_input_widgets(const Dictionary& values)
{
    for (size_t i = 0; i < m_input_metadata.size(); ++i)
    {
        Dictionary im = m_input_metadata[i];

        const string input_name = im.get<string>("name");
        const string input_type = im.get<string>("type");

        if (values.strings().exist(input_name))
            im.insert("default", values.get(input_name.c_str()));

        auto_ptr<IInputWidgetProxy> widget_proxy =
            input_type == "colormap" ?
                im.dictionaries().exist("entity_types") &&
                im.dictionaries().get("entity_types").strings().exist("color")
                    ? create_color_input_widgets(im)
                    : create_colormap_input_widgets(im) :
            input_type == "text" ? create_text_input_widgets(im) :
            auto_ptr<IInputWidgetProxy>(0);

        assert(widget_proxy.get());

        connect(widget_proxy.get(), SIGNAL(signal_changed()), SIGNAL(signal_apply()));

        m_widget_proxies.insert(input_name, widget_proxy);
    }
}

Dictionary DisneyMaterialLayerUI::get_values() const
{
    return
        m_widget_proxies
            .get_values()
            .insert("layer_folded", m_is_folded);
}

void DisneyMaterialLayerUI::mouseDoubleClickEvent(QMouseEvent* event)
{
    slot_fold_unfold_layer();
}

void DisneyMaterialLayerUI::slot_fold_unfold_layer()
{
    m_is_folded = !m_is_folded;
    m_is_folded ? fold() : unfold();
}

void DisneyMaterialLayerUI::slot_move_layer_up()
{
    emit signal_move_layer_up(this);
}

void DisneyMaterialLayerUI::slot_move_layer_down()
{
    emit signal_move_layer_down(this);
}

void DisneyMaterialLayerUI::slot_delete_layer()
{
    emit signal_delete_layer(this);
}

void DisneyMaterialLayerUI::slot_open_color_picker(const QString& widget_name)
{
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());

    const string color_expression = widget_proxy->get();
    const QColor initial_color = ColorExpressionProxy::expression_to_qcolor(widget_proxy->get());

    QColorDialog* dialog = new QColorDialog(initial_color, m_content_widget);
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

void DisneyMaterialLayerUI::slot_color_changed(const QString& widget_name, const QColor& color)
{
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());
    widget_proxy->set(ColorExpressionProxy::qcolor_to_expression(color));
    widget_proxy->emit_signal_changed();
}

namespace
{
    bool find_path_in_dir(
        const QString&  filename,
        const QDir&     dir,
        QString&        result)
    {
        result = dir.relativeFilePath(filename);

        // Ignore paths that go up the directory hierarchy.
        if (result.startsWith(".."))
            return false;

        const QFileInfo relative_file_info(result);
        if (relative_file_info.isRelative())
            return true;

        return false;
    }

    QString find_path_in_search_paths(const SearchPaths& s, const QString& filename)
    {
        const QFileInfo file_info(filename);
        assert(file_info.isAbsolute());

        for (size_t i = 0; i < s.size(); ++i)
        {
            // Iterate in reverse order, to match search paths priorities.
            QString search_path(QString::fromStdString(s[s.size() - 1 - i]));
            const QFileInfo search_path_info(search_path);

            if (search_path_info.isRelative())
            {
                assert(s.has_root_path());

                search_path =
                    QDir::cleanPath(
                        QString::fromStdString(s.get_root_path()) +
                        QDir::separator() +
                        search_path);
            }

            const QDir search_dir(search_path);
            QString relative_path;

            if (find_path_in_dir(filename, search_dir, relative_path))
                return relative_path;
        }

        if (s.has_root_path())
        {
            const QDir root_dir(QString::fromStdString(s.get_root_path()));
            assert(root_dir.isAbsolute());

            QString relative_path;

            if (find_path_in_dir(filename, root_dir, relative_path))
                return relative_path;
        }

        return filename;
    }

    string texture_to_expression(const SearchPaths& search_paths, const QString& path)
    {
        const QString relative_path = find_path_in_search_paths(search_paths, path);
        const QString texture_expression = QString("texture(\"%1\", $u, $v)").arg(relative_path);
        return texture_expression.toStdString();
    }
}

void DisneyMaterialLayerUI::slot_open_file_picker(const QString& widget_name)
{
    QString filepath =
        get_open_filename(
            m_content_widget,
            "Pick Texture File...",
            compute_oiio_files_filter(),
            m_settings,
            SETTINGS_FILE_DIALOG_PROJECTS);

    if (!filepath.isEmpty())
    {
        IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());
        widget_proxy->set(
            texture_to_expression(
                m_project.search_paths(),
                QDir::toNativeSeparators(filepath)));
        widget_proxy->emit_signal_changed();
    }
}

void DisneyMaterialLayerUI::slot_open_expression_editor(const QString& widget_name)
{
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());

    ExpressionEditorWindow* expression_editor_window =
        new ExpressionEditorWindow(
            m_content_widget,
            m_project,
            m_settings,
            widget_name,
            widget_proxy->get());

    connect(
        expression_editor_window,
        SIGNAL(signal_expression_applied(const QString&, const QString&)),
        SLOT(slot_expression_changed(const QString&, const QString&)));

    expression_editor_window->show();
    expression_editor_window->activateWindow();
}

void DisneyMaterialLayerUI::slot_expression_changed(
    const QString& widget_name,
    const QString& expression)
{
    IInputWidgetProxy* widget_proxy = m_widget_proxies.get(widget_name.toStdString());
    widget_proxy->set(expression.toStdString());
    widget_proxy->emit_signal_changed();
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

auto_ptr<IInputWidgetProxy> DisneyMaterialLayerUI::create_text_input_widgets(const Dictionary& metadata)
{
    QLineEdit* line_edit = new QLineEdit(m_content_widget);

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    m_content_layout->addRow(create_label(metadata), line_edit);

    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));

    if (metadata.strings().exist("default"))
        widget_proxy->set(metadata.strings().get<string>("default"));

    return widget_proxy;
}

auto_ptr<IInputWidgetProxy> DisneyMaterialLayerUI::create_color_input_widgets(const Dictionary& metadata)
{
    QLineEdit* line_edit = new QLineEdit(m_content_widget);

    QToolButton* picker_button = new QToolButton(m_content_widget);
    picker_button->setObjectName("color_picker");
    connect(picker_button, SIGNAL(clicked()), m_color_picker_signal_mapper, SLOT(map()));

    const string name = metadata.get<string>("name");
    m_color_picker_signal_mapper->setMapping(picker_button, QString::fromStdString(name));

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(line_edit);
    layout->addWidget(picker_button);
    layout->addWidget(create_texture_button(name));
    layout->addWidget(create_expression_button(name));
    m_content_layout->addRow(create_label(metadata), layout);

    auto_ptr<IInputWidgetProxy> widget_proxy(new ColorExpressionProxy(line_edit, picker_button));

    if (metadata.strings().exist("default"))
        widget_proxy->set(metadata.strings().get<string>("default"));

    return widget_proxy;
}

auto_ptr<IInputWidgetProxy> DisneyMaterialLayerUI::create_colormap_input_widgets(const Dictionary& metadata)
{
    QLineEdit* line_edit = new QLineEdit(m_content_widget);
    line_edit->setMaximumWidth(120);

    const double MinValue = 0.0;
    const double MaxValue = 1.0;

    DoubleSlider* slider = new DoubleSlider(Qt::Horizontal, m_content_widget);
    slider->setRange(MinValue, MaxValue);
    slider->setPageStep((MaxValue - MinValue) / 10.0);

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
    connect(
        line_edit, SIGNAL(editingFinished()),
        adaptor, SLOT(slot_apply_slider_value()));

    connect(slider, SIGNAL(valueChanged(int)), SIGNAL(signal_apply()));

    if (should_be_focused(metadata))
    {
        line_edit->selectAll();
        line_edit->setFocus();
    }

    const string name = metadata.get<string>("name");

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(line_edit);
    layout->addWidget(slider);
    layout->addWidget(create_texture_button(name));
    layout->addWidget(create_expression_button(name));
    m_content_layout->addRow(create_label(metadata), layout);

    auto_ptr<IInputWidgetProxy> widget_proxy(new LineEditProxy(line_edit));

    if (metadata.strings().exist("default"))
        widget_proxy->set(metadata.strings().get<string>("default"));

    return widget_proxy;
}

QWidget* DisneyMaterialLayerUI::create_texture_button(const string& name)
{
    QToolButton* texture_button = new QToolButton(m_content_widget);
    texture_button->setIcon(load_icons("disney_texture"));
    texture_button->setAutoRaise(true);     // enable hover state
    texture_button->setToolTip("Bind Texture...");

    connect(texture_button, SIGNAL(clicked()), m_file_picker_signal_mapper, SLOT(map()));
    m_file_picker_signal_mapper->setMapping(texture_button, QString::fromStdString(name));

    return texture_button;
}

QWidget* DisneyMaterialLayerUI::create_expression_button(const string& name)
{
    QToolButton* expression_button = new QToolButton(m_content_widget);
    expression_button->setIcon(load_icons("disney_expression"));
    expression_button->setAutoRaise(true);  // enable hover state
    expression_button->setToolTip("Bind Expression...");

    connect(expression_button, SIGNAL(clicked()), m_expression_editor_signal_mapper, SLOT(map()));
    m_expression_editor_signal_mapper->setMapping(expression_button, QString::fromStdString(name));

    return expression_button;
}

namespace
{
    QString get_layer_name_label(const DictionaryArray& input_metadata)
    {
        for (size_t i = 0; i < input_metadata.size(); ++i)
        {
            if (input_metadata[i].get<string>("name") == "layer_name")
                return input_metadata[i].get<QString>("label") + ":";
        }

        assert(!"Could not find layer name input metadata.");
        return QString();
    }
}

void DisneyMaterialLayerUI::fold()
{
    // Hide the layer's content.
    m_content_widget->setVisible(false);

    // Use dynamic_cast<> solely to allow checking that we indeed have a LineEditProxy.
    LineEditProxy* layer_name_proxy =
        dynamic_cast<LineEditProxy*>(m_widget_proxies.get("layer_name"));
    assert(layer_name_proxy);

    // Show the layer's name in the header.
    QLineEdit* name_line_edit = new QLineEdit(m_header_widget);
    name_line_edit->setText(
        QString::fromStdString(layer_name_proxy->get()));
    connect(
        name_line_edit, SIGNAL(textChanged(const QString&)),
        layer_name_proxy->get_widget(), SLOT(setText(const QString&)));
    m_header_layout->addRow(
        get_layer_name_label(m_input_metadata),
        name_line_edit);

    // Update the icon of the fold/unfold button.
    m_fold_unfold_button->setIcon(m_unfold_icon);
}

void DisneyMaterialLayerUI::unfold()
{
    // Show the layer's content.
    m_content_widget->setVisible(true);

    // Hide the layer's name in the header.
    clear_layout(m_header_widget->layout());

    // Update the icon of the fold/unfold button.
    m_fold_unfold_button->setIcon(m_fold_icon);
}

}   // namespace studio
}   // namespace appleseed
