
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "rendersettingswindow.h"

// UI definition header.
#include "ui_rendersettingswindow.h"

// Qt headers.
#include <QCheckBox>
#include <QComboBox>
#include <QDoubleValidator>
#include <QFormLayout>
#include <QFrame>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QIntValidator>
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QPushButton>
#include <QString>
#include <Qt>
#include <QVBoxLayout>

// Standard headers.
#include <cassert>

class FoldablePanel
  : public QWidget
{
    Q_OBJECT

  public:
    FoldablePanel(const QString& title, QWidget* parent = 0)
      : QWidget(parent)
      , m_button(new QPushButton(title))
      , m_container(new QFrame())
    {
        setLayout(new QVBoxLayout());

        layout()->setSpacing(0);
        layout()->addWidget(m_button);
        layout()->addWidget(m_container);

        m_container->setProperty("hasFrame", true);

        connect(m_button, SIGNAL(clicked()), this, SLOT(slot_toggle_container_visibility()));
    }

    QFrame* container()
    {
        return m_container;
    }

  private:
    QPushButton*    m_button;
    QFrame*         m_container;

  private slots:
    void slot_toggle_container_visibility()
    {
        m_container->setVisible(!m_container->isVisible());
    }
};

namespace appleseed {
namespace studio {

//
// RenderSettingsWindow class implementation.
//

RenderSettingsWindow::RenderSettingsWindow(QWidget* parent)
  : QWidget(parent)
  , m_ui(new Ui::RenderSettingsWindow())
{
    m_ui->setupUi(this);

    setWindowFlags(Qt::Window);

    m_ui->scrollarea->setProperty("hasFrame", true);

    create_panels();
}

RenderSettingsWindow::~RenderSettingsWindow()
{
    delete m_ui;
}

void RenderSettingsWindow::create_panels() const
{
    QLayout* root = m_ui->scrollareawidget->layout();
    assert(root);

    create_image_plane_sampling_panel(root);
    create_lighting_panel(root);
    create_shading_panel(root);
}

//---------------------------------------------------------------------------------------------
// Image Plane Sampling panel.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_image_plane_sampling_panel(QLayout* parent) const
{
    FoldablePanel* panel = new FoldablePanel("Image Plane Sampling");
    parent->addWidget(panel);

    QVBoxLayout* layout = new QVBoxLayout();
    panel->container()->setLayout(layout);

    create_image_plane_sampling_general_settings(layout);
    create_image_plane_sampling_sampler_settings(layout);
}

void RenderSettingsWindow::create_image_plane_sampling_general_settings(QVBoxLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("General");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = new QVBoxLayout();
    groupbox->setLayout(layout);

    QHBoxLayout* sublayout = create_horizontal_layout();
    layout->addLayout(sublayout);

    sublayout->addLayout(create_form_layout("Filter Type:", new QComboBox()));
    sublayout->addLayout(create_form_layout("Filter Size:", create_integer_line_edit(1, 999)));

    layout->addLayout(create_form_layout("Sampler:", new QComboBox()));
}

void RenderSettingsWindow::create_image_plane_sampling_sampler_settings(QVBoxLayout* parent) const
{
    QHBoxLayout* layout = create_horizontal_layout();
    parent->addLayout(layout);

    create_image_plane_sampling_uniform_sampler_settings(layout);
    create_image_plane_sampling_adaptive_sampler_settings(layout);
}

void RenderSettingsWindow::create_image_plane_sampling_uniform_sampler_settings(QHBoxLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("Uniform Sampler");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = create_vertical_layout();
    groupbox->setLayout(layout);

    layout->addLayout(create_form_layout("Min Samples:", create_integer_line_edit(1, 999999)));
}

void RenderSettingsWindow::create_image_plane_sampling_adaptive_sampler_settings(QHBoxLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("Adaptive Sampler");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = create_vertical_layout();
    groupbox->setLayout(layout);

    QFormLayout* sublayout = create_form_layout();
    layout->addLayout(sublayout);

    sublayout->addRow("Min Samples:", create_integer_line_edit(1, 999999));
    sublayout->addRow("Max Samples:", create_integer_line_edit(1, 999999));
    sublayout->addRow("Max Error:", create_floating_point_line_edit());
}

//---------------------------------------------------------------------------------------------
// Lighting panel.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_lighting_panel(QLayout* parent) const
{
    FoldablePanel* panel = new FoldablePanel("Lighting");
    parent->addWidget(panel);

    QVBoxLayout* layout = new QVBoxLayout();
    panel->container()->setLayout(layout);

    create_lighting_general_settings(layout);
    create_lighting_distribution_ray_tracer_settings(layout);
    create_lighting_path_tracer_settings(layout);
    create_lighting_direct_lighting_settings(layout);
    create_lighting_image_based_lighting_settings(layout);
    create_lighting_caustics_settings(layout);
}

void RenderSettingsWindow::create_lighting_general_settings(QVBoxLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("General");
    parent->addWidget(groupbox);

    QFormLayout* layout = create_form_layout();
    groupbox->setLayout(layout);

    layout->addRow("Engine:", new QComboBox());
    layout->addRow("Max Bounces:", create_integer_line_edit(0, 9999));
    layout->addRow("Russian Roulette Start Bounce:", create_integer_line_edit(1, 9999));
}

void RenderSettingsWindow::create_lighting_distribution_ray_tracer_settings(QVBoxLayout* layout) const
{
}

void RenderSettingsWindow::create_lighting_path_tracer_settings(QVBoxLayout* layout) const
{
}

void RenderSettingsWindow::create_lighting_direct_lighting_settings(QLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("Direct Lighting");
    groupbox->setCheckable(true);
    parent->addWidget(groupbox);

    QVBoxLayout* layout = create_vertical_layout();
    groupbox->setLayout(layout);

    QHBoxLayout* sublayout = create_horizontal_layout();
    layout->addLayout(sublayout);

    sublayout->addLayout(create_form_layout("Light Samples:", create_integer_line_edit(1, 999999)));
    sublayout->addLayout(create_form_layout("BSDF Samples:", create_integer_line_edit(1, 999999)));
}

void RenderSettingsWindow::create_lighting_image_based_lighting_settings(QLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("Image-Based Lighting");
    groupbox->setCheckable(true);
    parent->addWidget(groupbox);

    QVBoxLayout* layout = create_vertical_layout();
    groupbox->setLayout(layout);

    QHBoxLayout* sublayout = create_horizontal_layout();
    layout->addLayout(sublayout);

    sublayout->addLayout(create_form_layout("Light Samples:", create_integer_line_edit(1, 999999)));
    sublayout->addLayout(create_form_layout("BSDF Samples:", create_integer_line_edit(1, 999999)));
}

void RenderSettingsWindow::create_lighting_caustics_settings(QLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("Caustics");
    groupbox->setCheckable(true);
    parent->addWidget(groupbox);

    QVBoxLayout* layout = create_vertical_layout();
    groupbox->setLayout(layout);
}

//---------------------------------------------------------------------------------------------
// Shading panel.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_shading_panel(QLayout* parent) const
{
    FoldablePanel* panel = new FoldablePanel("Shading");
    parent->addWidget(panel);

    QVBoxLayout* layout = new QVBoxLayout();
    panel->container()->setLayout(layout);

    create_shading_texture_cache_settings(layout);
}

void RenderSettingsWindow::create_shading_texture_cache_settings(QVBoxLayout* parent) const
{
    parent->addLayout(create_form_layout("Texture Cache Size:", create_integer_line_edit(1, 99999, "MB")));
}

//---------------------------------------------------------------------------------------------
// Base controls.
//---------------------------------------------------------------------------------------------

QHBoxLayout* RenderSettingsWindow::create_horizontal_layout() const
{
    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(20);
    return layout;
}

QVBoxLayout* RenderSettingsWindow::create_vertical_layout() const
{
    QVBoxLayout* layout = new QVBoxLayout();
    layout->setAlignment(Qt::AlignLeft | Qt::AlignTop);
    return layout;
}

QFormLayout* RenderSettingsWindow::create_form_layout() const
{
    QFormLayout* layout = new QFormLayout();
    layout->setSpacing(10);
    return layout;
}

QFormLayout* RenderSettingsWindow::create_form_layout(const QString& label, QWidget* widget) const
{
    QFormLayout* layout = create_form_layout();
    layout->addRow(label, widget);
    return layout;
}

QLineEdit* RenderSettingsWindow::create_integer_line_edit(int min, int max) const
{
    QLineEdit* lineedit = new QLineEdit();
    lineedit->setMaximumWidth(50);
    lineedit->setValidator(new QIntValidator(min, max));
    return lineedit;
}

QWidget* RenderSettingsWindow::create_integer_line_edit(int min, int max, const QString& suffix) const
{
    QWidget* group = new QWidget();

    QHBoxLayout* layout = new QHBoxLayout();
    group->setLayout(layout);

    layout->setMargin(0);
    layout->setSpacing(10);
    layout->setSizeConstraint(QLayout::SetFixedSize);

    layout->addWidget(create_integer_line_edit(min, max));
    layout->addWidget(new QLabel(suffix));

    return group;
}

QLineEdit* RenderSettingsWindow::create_floating_point_line_edit() const
{
    QLineEdit* lineedit = new QLineEdit();
    lineedit->setMaximumWidth(50);
    lineedit->setValidator(new QDoubleValidator());
    return lineedit;
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/moc_cpp_rendersettingswindow.cxx"
