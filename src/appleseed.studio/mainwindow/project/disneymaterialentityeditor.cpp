
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
#include "utility/doubleslider.h"
#include "utility/miscellaneous.h"
#include "utility/mousewheelfocuseventfilter.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// Standard headers.
#include <sstream>

// Ui definition headers.
#include "ui_disneymaterialentityeditor.h"

// Qt headers.
#include <QFormLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QString>
#include <QVBoxLayout>
#include <Qt>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

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

            const double new_value = value.toDouble();

            // Adjust range max if the new value is greater than current range max.
            if (new_value > m_slider->maximum())
                adjust_slider(new_value);

            m_slider->setValue(new_value);
            m_slider->blockSignals(false);
        }

        void slot_apply_slider_value()
        {
            m_slider->blockSignals(true);

            const double new_value = m_line_edit->text().toDouble();

            // Adjust range max if the new value is greater than current range max
            // or less than a certain percentage of current range max.
            if (new_value > m_slider->maximum() ||
                new_value < lerp(m_slider->minimum(), m_slider->maximum(), 1.0 / 3))
                adjust_slider(new_value);

            m_slider->setValue(new_value);
            m_slider->blockSignals(false);
        }

      private:
        QLineEdit*      m_line_edit;
        DoubleSlider*   m_slider;

        void adjust_slider(const double new_value)
        {
            const double new_max = 2.0 * new_value;
            m_slider->setRange(0.0, new_max);
            m_slider->setPageStep(new_max / 10.0);
        }
    };

}

class DisneyMaterialEntityEditor::LayerWidget
  : public QFrame
{
    Q_OBJECT

  public:
    LayerWidget(DisneyMaterialEntityEditor* editor, QWidget* parent = 0)
      : QFrame(parent)
      , m_editor(editor)
    {
        setObjectName("layer");
    }

    void mousePressEvent(QMouseEvent* event)
    {
        if (objectName() != "selected_layer")
        {
            setObjectName("selected_layer");

            // Force stylesheet reloading for this widget.
            style()->unpolish(this);
            style()->polish(this);
            
            QWidget* selected_layer = m_editor->m_selected_layer_widget;
            if (selected_layer)
            {
                selected_layer->setObjectName("layer");
                style()->unpolish(selected_layer);
                style()->polish(selected_layer);
            }
            m_editor->m_selected_layer_widget = this;
        }
    }

    DisneyMaterialEntityEditor* m_editor;
};


DisneyMaterialEntityEditor::DisneyMaterialEntityEditor(
    QWidget*                                parent,
    const Project&                          project,
    auto_ptr<EntityEditor::IFormFactory>    form_factory,
    auto_ptr<EntityEditor::IEntityBrowser>  entity_browser,
    const Dictionary&           values)
  : QWidget(parent)
  , m_ui(new Ui::DisneyMaterialEntityEditor())
  , m_num_created_layers(0)
  , m_parent(parent)
  , m_selected_layer_widget(0)
{
    m_ui->setupUi(this);

    setWindowTitle(QString::fromStdString("Create/Edit Disney Material."));
    setWindowFlags(Qt::Tool);
    setAttribute(Qt::WA_DeleteOnClose);
    
    m_scrollarea = m_ui->scrollarea_contents;
    create_form_layout();

    // Connect slots
    connect(m_ui->add_button, SIGNAL(released()), this, SLOT(add_layer_slot()));
    connect(m_ui->delete_button, SIGNAL(released()), this, SLOT(delete_layer_slot()));
    connect(m_ui->up_button, SIGNAL(released()), this, SLOT(move_layer_up_slot()));
    connect(m_ui->down_button, SIGNAL(released()), this, SLOT(move_layer_down_slot()));
}

DisneyMaterialEntityEditor::~DisneyMaterialEntityEditor()
{
    delete m_ui;
}

void DisneyMaterialEntityEditor::add_layer_slot()
{
    add_layer();
}

void DisneyMaterialEntityEditor::delete_layer_slot()
{
    if (m_selected_layer_widget) 
    { 
        delete m_selected_layer_widget;
        m_selected_layer_widget = 0;
    }
}

void DisneyMaterialEntityEditor::move_layer_up_slot()
{
    if (m_selected_layer_widget)
    {
        for (int i=0; i<m_form_layout->count(); ++i)
        {
            QLayoutItem* layout_item = m_form_layout->itemAt(i);
            if (m_selected_layer_widget == layout_item->widget())
            {
                if (i > 0)
                {
                    m_form_layout->takeAt(i);
                    m_form_layout->insertWidget(i-1, m_selected_layer_widget);
                }
                break;
            }
            
        }
    }
}

void DisneyMaterialEntityEditor::move_layer_down_slot()
{
    if (m_selected_layer_widget)
    {
        for (int i=0; i<m_form_layout->count(); ++i)
        {
            QLayoutItem* layout_item = m_form_layout->itemAt(i);
            if (m_selected_layer_widget == layout_item->widget())
            {
                if (i < m_form_layout->count()-2)
                {
                    m_form_layout->takeAt(i);
                    m_form_layout->insertWidget(i+1, m_selected_layer_widget);
                }
                break;
            }
        }
    }
}

void DisneyMaterialEntityEditor::create_form_layout()
{
    m_form_layout = new QVBoxLayout(m_ui->scrollarea_contents);
    m_form_layout->setSpacing(5);

    // Add a default layer
    add_layer();
}

void DisneyMaterialEntityEditor::create_layer_layout()
{
    m_layer_widget = new LayerWidget(this);
    m_layers_widgets.push_back(m_layer_widget);

    m_form_layout->addWidget(m_layer_widget);
    m_layer_layout = new QVBoxLayout(m_layer_widget);
}

string DisneyMaterialEntityEditor::unique_layer_name()
{
    stringstream sstream;
    sstream << "layer" << ++m_num_created_layers;
    return sstream.str();
}

void DisneyMaterialEntityEditor::create_text_input_widgets(const string& parameter, const string& value)
{
    QLabel* label = new QLabel(parameter.c_str(), m_layer_widget);
    QLineEdit* line_edit = new QLineEdit(value.c_str(), m_layer_widget);

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(label);
    layout->addWidget(line_edit);
    
    m_layer_layout->addLayout(layout);
}

void DisneyMaterialEntityEditor::create_color_input_widgets(const string& parameter, int index)
{
    QLabel* label = new QLabel(parameter.c_str(), m_layer_widget);
    QLineEdit* line_edit = new QLineEdit("0", m_layer_widget);

    QWidget* bind_button = new QPushButton("Bind", m_layer_widget);
    bind_button->setObjectName("bind_color_button");
    bind_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(label);
    layout->addWidget(line_edit);
    layout->addWidget(bind_button);

    m_layer_layout->addLayout(layout);
}

void DisneyMaterialEntityEditor::create_colormap_input_widgets(const string& parameter, int index)
{
    QLabel* label = new QLabel(parameter.c_str(), m_layer_widget);
    QLineEdit* line_edit = new QLineEdit("0", m_layer_widget);
 
    const double min_value = 0.0;
    const double max_value = 1.0;

    DoubleSlider* slider = new DoubleSlider(Qt::Horizontal, m_layer_widget);
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
    connect(
        line_edit, SIGNAL(editingFinished()),
        adaptor, SLOT(slot_apply_slider_value()));

    QWidget* bind_button = new QPushButton("Bind", m_layer_widget);
    bind_button->setObjectName("bind_colormap_button");
    bind_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    
    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);
    layout->addWidget(label);
    layout->addWidget(line_edit);
    layout->addWidget(slider);
    layout->addWidget(bind_button);

    m_layer_layout->addLayout(layout);
}

void DisneyMaterialEntityEditor::add_layer()
{
    typedef std::vector<foundation::Dictionary> InputMetadataCollection;
    InputMetadataCollection metadata;
    int index = 0;

    metadata.push_back(
        Dictionary()
            .insert("name", "layer_name")
            .insert("label", "Layer name")
            .insert("type", "text"));

    metadata.push_back(
        Dictionary()
            .insert("name", "mask")
            .insert("label", "Mask")
            .insert("type", "colormap"));

    metadata.push_back(
        Dictionary()
            .insert("name", "base_color")
            .insert("label", "Base Color")
            .insert("type", "color"));

    metadata.push_back(
        Dictionary()
            .insert("name", "subsurface")
            .insert("label", "Subsurface")
            .insert("type", "colormap"));

    metadata.push_back(
        Dictionary()
            .insert("name", "metallic")
            .insert("label", "Metallic")
            .insert("type", "colormap"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular")
            .insert("label", "Specular")
            .insert("type", "colormap"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular_tint")
            .insert("label", "Specular tint")
            .insert("type", "colormap"));

    metadata.push_back(
        Dictionary()
            .insert("name", "anisotropic")
            .insert("label", "Anisotropic")
            .insert("type", "colormap"));

    metadata.push_back(
        Dictionary()
            .insert("name", "roughness")
            .insert("label", "Roughness")
            .insert("type", "colormap"));

    metadata.push_back(
        Dictionary()
            .insert("name", "sheen")
            .insert("label", "Sheen")
            .insert("type", "colormap"));

    metadata.push_back(
        Dictionary()
            .insert("name", "shin_tint")
            .insert("label", "Shin tint")
            .insert("type", "colormap"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clearcoat")
            .insert("label", "Clearcoat")
            .insert("type", "colormap"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clearcoat_gloss")
            .insert("label", "Clearcoat gloss")
            .insert("type", "colormap"));
    
    create_layer_layout();
    for (const_each<InputMetadataCollection> i = metadata; i; ++i)
    {
        const string name = i->get<string>("name");
        const string label = i->get<string>("label") + ":";
        const string type = i->get<string>("type");
        
        if (type == "color")
            create_color_input_widgets(label, index);
        else if (type == "colormap")
            create_colormap_input_widgets(label, index);
        else if (type == "text")
            create_text_input_widgets(label, unique_layer_name());

        index++;
    }

    if (m_form_layout->count() == 1)
    {
        // Add a stretch at the end
        m_form_layout->addStretch(1);
    }
    else
    {
        // Remove previous strech and add new one at the end
        m_form_layout->takeAt(m_form_layout->count()-2);
        m_form_layout->addStretch(1);
    }
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/project/moc_cpp_disneymaterialentityeditor.cxx"
