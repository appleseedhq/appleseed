
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

// appleseed.studio headers.
#include "mainwindow/configurationmanagerwindow.h"
#include "utility/foldablepanelwidget.h"

// Qt headers.
#include <QCheckBox>
#include <QComboBox>
#include <QDoubleSpinBox>
#include <QFormLayout>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QLayout>
#include <QSpinBox>
#include <QString>
#include <Qt>
#include <QVBoxLayout>

// Standard headers.
#include <cassert>

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

    connect(m_ui->pushbutton_manage, SIGNAL(clicked()), this, SLOT(slot_open_configuration_manager_window()));
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
    create_advanced_panel(root);
}

//---------------------------------------------------------------------------------------------
// Image Plane Sampling panel.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_image_plane_sampling_panel(QLayout* parent) const
{
    FoldablePanelWidget* panel = new FoldablePanelWidget("Image Plane Sampling");
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
    sublayout->addLayout(create_form_layout("Filter Size:", create_integer_input(1, 64)));

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

    layout->addLayout(create_form_layout("Min Samples:", create_integer_input(1, 1000000)));
}

void RenderSettingsWindow::create_image_plane_sampling_adaptive_sampler_settings(QHBoxLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("Adaptive Sampler");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = create_vertical_layout();
    groupbox->setLayout(layout);

    QFormLayout* sublayout = create_form_layout();
    layout->addLayout(sublayout);

    sublayout->addRow("Min Samples:", create_integer_input(1, 1000000));
    sublayout->addRow("Max Samples:", create_integer_input(1, 1000000));
    sublayout->addRow("Max Error:", create_floating_point_input(0.0, 1000000.0));
}

//---------------------------------------------------------------------------------------------
// Lighting panel.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_lighting_panel(QLayout* parent) const
{
    FoldablePanelWidget* panel = new FoldablePanelWidget("Lighting");
    parent->addWidget(panel);

    QVBoxLayout* layout = new QVBoxLayout();
    panel->container()->setLayout(layout);

    create_lighting_general_settings(layout);
    create_lighting_components_settings(layout);
}

void RenderSettingsWindow::create_lighting_general_settings(QVBoxLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("General");
    parent->addWidget(groupbox);

    QFormLayout* layout = create_form_layout();
    groupbox->setLayout(layout);

    layout->addRow("Engine:", new QComboBox());
    layout->addRow("Max Bounces:", create_integer_input(0, 10000));
    layout->addRow("Russian Roulette Start Bounce:", create_integer_input(1, 10000));
}

void RenderSettingsWindow::create_lighting_components_settings(QVBoxLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("Components");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = new QVBoxLayout();
    groupbox->setLayout(layout);

    layout->addWidget(new QCheckBox("Direct Lighting"));
    layout->addWidget(new QCheckBox("Image-Based Lighting"));
    layout->addWidget(new QCheckBox("Caustics"));
}

//---------------------------------------------------------------------------------------------
// Shading panel.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_shading_panel(QLayout* parent) const
{
    FoldablePanelWidget* panel = new FoldablePanelWidget("Shading");
    parent->addWidget(panel);

    QVBoxLayout* layout = new QVBoxLayout();
    panel->container()->setLayout(layout);

    create_shading_texture_cache_settings(layout);
}

void RenderSettingsWindow::create_shading_texture_cache_settings(QVBoxLayout* parent) const
{
    parent->addLayout(create_form_layout("Texture Cache Size:", create_integer_input(1, 1024 * 1024, "MB")));
}

//---------------------------------------------------------------------------------------------
// Advanced panel.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_advanced_panel(QLayout* parent) const
{
    FoldablePanelWidget* panel = new FoldablePanelWidget("Advanced");
    parent->addWidget(panel);
    panel->fold();

    QVBoxLayout* layout = new QVBoxLayout();
    panel->container()->setLayout(layout);

    create_advanced_direct_lighting_settings(layout);
    create_advanced_image_based_lighting_settings(layout);
    create_advanced_distribution_ray_tracer_settings(layout);
    create_advanced_path_tracer_settings(layout);
}

void RenderSettingsWindow::create_advanced_direct_lighting_settings(QVBoxLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("Direct Lighting");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = create_vertical_layout();
    groupbox->setLayout(layout);

    QHBoxLayout* sublayout = create_horizontal_layout();
    layout->addLayout(sublayout);

    sublayout->addLayout(create_form_layout("Light Samples:", create_integer_input(0, 1000000)));
    sublayout->addLayout(create_form_layout("BSDF Samples:", create_integer_input(0, 1000000)));
}

void RenderSettingsWindow::create_advanced_image_based_lighting_settings(QVBoxLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("Image-Based Lighting");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = create_vertical_layout();
    groupbox->setLayout(layout);

    QHBoxLayout* sublayout = create_horizontal_layout();
    layout->addLayout(sublayout);

    sublayout->addLayout(create_form_layout("Light Samples:", create_integer_input(0, 1000000)));
    sublayout->addLayout(create_form_layout("BSDF Samples:", create_integer_input(0, 1000000)));
}

void RenderSettingsWindow::create_advanced_distribution_ray_tracer_settings(QVBoxLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("Distribution Ray Tracer");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = new QVBoxLayout();
    groupbox->setLayout(layout);
}

void RenderSettingsWindow::create_advanced_path_tracer_settings(QVBoxLayout* parent) const
{
    QGroupBox* groupbox = new QGroupBox("Path Tracer");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = new QVBoxLayout();
    groupbox->setLayout(layout);

    layout->addWidget(new QCheckBox("Next Event Estimation"));
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

QWidget* RenderSettingsWindow::create_integer_input(const int min, const int max) const
{
    QSpinBox* spinbox = new QSpinBox();
    spinbox->setMaximumWidth(60);
    spinbox->setRange(min, max);
    return spinbox;
}

QWidget* RenderSettingsWindow::create_integer_input(const int min, const int max, const QString& suffix) const
{
    QWidget* group = new QWidget();

    QHBoxLayout* layout = new QHBoxLayout();
    group->setLayout(layout);

    layout->setMargin(0);
    layout->setSpacing(10);
    layout->setSizeConstraint(QLayout::SetFixedSize);

    layout->addWidget(create_integer_input(min, max));
    layout->addWidget(new QLabel(suffix));

    return group;
}

QWidget* RenderSettingsWindow::create_floating_point_input(const double min, const double max) const
{
    QDoubleSpinBox* spinbox = new QDoubleSpinBox();
    spinbox->setMaximumWidth(60);
    spinbox->setRange(min, max);
    return spinbox;
}

void RenderSettingsWindow::slot_open_configuration_manager_window()
{
    ConfigurationManagerWindow* config_manager_window = new ConfigurationManagerWindow(this);

//    config_manager_window->center();
    config_manager_window->showNormal();
    config_manager_window->activateWindow();
}

}   // namespace studio
}   // namespace appleseed
