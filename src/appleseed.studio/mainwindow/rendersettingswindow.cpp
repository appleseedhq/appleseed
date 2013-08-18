
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
#include "rendersettingswindow.h"

// UI definition header.
#include "ui_rendersettingswindow.h"

// appleseed.studio headers.
#include "mainwindow/project/projectmanager.h"
#include "mainwindow/configurationmanagerwindow.h"
#include "utility/foldablepanelwidget.h"
#include "utility/inputwidgetproxies.h"
#include "utility/mousewheelfocuseventfilter.h"
#include "utility/tweaks.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/platform/system.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// Qt headers.
#include <QCheckBox>
#include <QComboBox>
#include <QDoubleSpinBox>
#include <QFontMetrics>
#include <QFormLayout>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QLayout>
#include <QMessageBox>
#include <QShortcut>
#include <QSpinBox>
#include <Qt>
#include <QVBoxLayout>

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

//
// RenderSettingsWindow class implementation.
//

RenderSettingsWindow::RenderSettingsWindow(ProjectManager& project_manager, QWidget* parent)
  : QWidget(parent)
  , m_ui(new Ui::RenderSettingsWindow())
  , m_project_manager(project_manager)
{
    m_ui->setupUi(this);

    setWindowFlags(Qt::Window);

    m_ui->scrollarea->setProperty("hasFrame", true);
    m_ui->scrollareawidget->hide();

    create_panels();

    create_direct_links();

    connect(m_ui->pushbutton_manage, SIGNAL(clicked()), this, SLOT(slot_open_configuration_manager_window()));

    connect(
        m_ui->combobox_configurations, SIGNAL(currentIndexChanged(const QString&)),
        this, SLOT(slot_change_active_configuration(const QString&)));

    connect(m_ui->buttonbox, SIGNAL(accepted()), this, SLOT(slot_save_configuration_and_close()));
    connect(m_ui->buttonbox, SIGNAL(rejected()), this, SLOT(slot_restore_configuration_and_close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Return), SIGNAL(activated()),
        this, SLOT(slot_save_configuration_and_close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Enter), SIGNAL(activated()),
        this, SLOT(slot_save_configuration_and_close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Escape), SIGNAL(activated()),
        this, SLOT(slot_restore_configuration_and_close()));

    reload();
}

RenderSettingsWindow::~RenderSettingsWindow()
{
    delete m_ui;
}

void RenderSettingsWindow::reload()
{
    vector<QString> configs;

    for (const_each<ConfigurationContainer> i = m_project_manager.get_project()->configurations(); i; ++i)
    {
        if (!BaseConfigurationFactory::is_base_configuration(i->get_name()))
            configs.push_back(i->get_name());
    }

    sort(configs.begin(), configs.end());

    // This will load an empty configuration.
    m_current_configuration_name.clear();

    // This will load the first configuration.
    m_ui->combobox_configurations->clear();
    for (size_t i = 0; i < configs.size(); ++i)
        m_ui->combobox_configurations->addItem(configs[i]);
}

void RenderSettingsWindow::create_panels()
{
    QLayout* root = m_ui->scrollareawidget->layout();
    assert(root);

    create_image_plane_sampling_panel(root);
    create_lighting_panel(root);
    create_drt_panel(root);
    create_pt_panel(root);
    create_system_panel(root);
}

void RenderSettingsWindow::set_panels_enabled(const bool enabled)
{
    for (const_each<PanelCollection> i = m_panels; i; ++i)
        (*i)->container()->setEnabled(enabled);
}

//---------------------------------------------------------------------------------------------
// Image Plane Sampling panel.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_image_plane_sampling_panel(QLayout* parent)
{
    FoldablePanelWidget* panel = new FoldablePanelWidget("Image Plane Sampling");
    parent->addWidget(panel);
    m_panels.push_back(panel);

    QVBoxLayout* layout = new QVBoxLayout();
    panel->container()->setLayout(layout);

    create_image_plane_sampling_general_settings(layout);
    create_image_plane_sampling_sampler_settings(layout);
}

void RenderSettingsWindow::create_image_plane_sampling_general_settings(QVBoxLayout* parent)
{
    QGroupBox* groupbox = new QGroupBox("General");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = new QVBoxLayout();
    groupbox->setLayout(layout);

    m_image_planer_sampler_combo = create_combobox("image_plane_sampling.general.sampler");
    m_image_planer_sampler_combo->addItem("Uniform", "uniform");
    m_image_planer_sampler_combo->addItem("Adaptive", "adaptive");
    m_image_planer_sampler_combo->setCurrentIndex(-1);
    layout->addLayout(create_form_layout("Sampler:", m_image_planer_sampler_combo));
    connect(
        m_image_planer_sampler_combo, SIGNAL(currentIndexChanged(int)),
        this, SLOT(slot_changed_image_plane_sampler(int)));
}

void RenderSettingsWindow::slot_changed_image_plane_sampler(int index)
{
    const QString sampler = m_image_planer_sampler_combo->itemData(index).value<QString>();

    m_uniform_image_plane_sampler->setEnabled(sampler == "uniform");
    m_adaptive_image_plane_sampler->setEnabled(sampler == "adaptive");
}

void RenderSettingsWindow::create_image_plane_sampling_sampler_settings(QVBoxLayout* parent)
{
    QHBoxLayout* layout = create_horizontal_layout();
    parent->addLayout(layout);

    create_image_plane_sampling_uniform_sampler_settings(layout);
    create_image_plane_sampling_adaptive_sampler_settings(layout);
}

void RenderSettingsWindow::create_image_plane_sampling_uniform_sampler_settings(QHBoxLayout* parent)
{
    m_uniform_image_plane_sampler = new QGroupBox("Uniform Sampler");
    parent->addWidget(m_uniform_image_plane_sampler);

    QVBoxLayout* layout = create_vertical_layout();
    m_uniform_image_plane_sampler->setLayout(layout);

    layout->addLayout(create_form_layout("Samples:", create_integer_input("image_plane_sampling.uniform_sampler.samples", 1, 1000000)));
    layout->addWidget(create_checkbox("image_plane_sampling.uniform_sampler.decorrelate_pixels", "Decorrelate Pixels"));
}

void RenderSettingsWindow::create_image_plane_sampling_adaptive_sampler_settings(QHBoxLayout* parent)
{
    m_adaptive_image_plane_sampler = new QGroupBox("Adaptive Sampler");
    parent->addWidget(m_adaptive_image_plane_sampler);

    QVBoxLayout* layout = create_vertical_layout();
    m_adaptive_image_plane_sampler->setLayout(layout);

    QFormLayout* sublayout = create_form_layout();
    layout->addLayout(sublayout);

    sublayout->addRow("Min Samples:", create_integer_input("image_plane_sampling.adaptive_sampler.min_samples", 1, 1000000));
    sublayout->addRow("Max Samples:", create_integer_input("image_plane_sampling.adaptive_sampler.max_samples", 1, 1000000));
    sublayout->addRow("Quality:", create_double_input("image_plane_sampling.adaptive_sampler.quality", -20.0, +20.0, 2, 0.5));

    layout->addWidget(create_checkbox("image_plane_sampling.adaptive_sampler.enable_diagnostics", "Enable Diagnostics"));
}

//---------------------------------------------------------------------------------------------
// Lighting panel.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_lighting_panel(QLayout* parent)
{
    FoldablePanelWidget* panel = new FoldablePanelWidget("Lighting");
    parent->addWidget(panel);
    m_panels.push_back(panel);

    QFormLayout* layout = create_form_layout();
    panel->container()->setLayout(layout);

    QComboBox* engine = create_combobox("lighting.engine");
    engine->addItem("Distribution Ray Tracer", "drt");
    engine->addItem("Unidirectional Path Tracer", "pt");
    layout->addRow("Engine:", engine);
}

//---------------------------------------------------------------------------------------------
// Distribution Ray Tracer panel.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_drt_panel(QLayout* parent)
{
    FoldablePanelWidget* panel = new FoldablePanelWidget("Distribution Ray Tracer");
    parent->addWidget(panel);
    m_panels.push_back(panel);

    panel->fold();

    QVBoxLayout* layout = new QVBoxLayout();
    panel->container()->setLayout(layout);

    QGroupBox* groupbox = new QGroupBox("Components");
    layout->addWidget(groupbox);

    QVBoxLayout* sublayout = new QVBoxLayout();
    groupbox->setLayout(sublayout);

    sublayout->addWidget(create_checkbox("drt.lighting_components.ibl", "Image-Based Lighting"));

    create_bounce_settings(layout, "drt");
    create_drt_advanced_settings(layout);
}

void RenderSettingsWindow::create_drt_advanced_settings(QVBoxLayout* parent)
{
    QGroupBox* groupbox = new QGroupBox("Advanced");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = new QVBoxLayout();
    groupbox->setLayout(layout);

    create_drt_advanced_dl_settings(layout);
    create_drt_advanced_ibl_settings(layout);
}

void RenderSettingsWindow::create_drt_advanced_dl_settings(QVBoxLayout* parent)
{
    QGroupBox* groupbox = new QGroupBox("Direct Lighting");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = create_vertical_layout();
    groupbox->setLayout(layout);

    QHBoxLayout* sublayout = create_horizontal_layout();
    layout->addLayout(sublayout);

    sublayout->addLayout(create_form_layout("Light Samples:", create_double_input("drt.advanced.dl.light_samples", 0.0, 1000000.0, 3, 1.0)));
}

void RenderSettingsWindow::create_drt_advanced_ibl_settings(QVBoxLayout* parent)
{
    QGroupBox* groupbox = new QGroupBox("Image-Based Lighting");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = create_vertical_layout();
    groupbox->setLayout(layout);

    QHBoxLayout* sublayout = create_horizontal_layout();
    layout->addLayout(sublayout);

    sublayout->addLayout(create_form_layout("Environment Samples:", create_double_input("drt.advanced.ibl.env_samples", 0.0, 1000000.0, 3, 1.0)));
}

//---------------------------------------------------------------------------------------------
// Unidirectional Path Tracer panel.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_pt_panel(QLayout* parent)
{
    FoldablePanelWidget* panel = new FoldablePanelWidget("Unidirectional Path Tracer");
    parent->addWidget(panel);
    m_panels.push_back(panel);

    panel->fold();

    QVBoxLayout* layout = new QVBoxLayout();
    panel->container()->setLayout(layout);

    QGroupBox* groupbox = new QGroupBox("Components");
    layout->addWidget(groupbox);

    QVBoxLayout* sublayout = new QVBoxLayout();
    groupbox->setLayout(sublayout);

    sublayout->addWidget(create_checkbox("pt.lighting_components.dl", "Direct Lighting"));
    sublayout->addWidget(create_checkbox("pt.lighting_components.ibl", "Image-Based Lighting"));
    sublayout->addWidget(create_checkbox("pt.lighting_components.caustics", "Caustics"));

    create_bounce_settings(layout, "pt");
    create_pt_advanced_settings(layout);
}

void RenderSettingsWindow::create_pt_advanced_settings(QVBoxLayout* parent)
{
    QGroupBox* groupbox = new QGroupBox("Advanced");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = new QVBoxLayout();
    groupbox->setLayout(layout);

    QGroupBox* nee_groupbox = create_checkable_groupbox("pt.advanced.next_event_estimation", "Next Event Estimation");
    layout->addWidget(nee_groupbox);

    QVBoxLayout* nee_layout = create_vertical_layout();
    nee_groupbox->setLayout(nee_layout);

    create_pt_advanced_dl_settings(nee_layout);
    create_pt_advanced_ibl_settings(nee_layout);
    create_pt_advanced_max_ray_intensity_settings(nee_layout);
}

void RenderSettingsWindow::create_pt_advanced_dl_settings(QVBoxLayout* parent)
{
    QGroupBox* groupbox = new QGroupBox("Direct Lighting");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = create_vertical_layout();
    groupbox->setLayout(layout);

    QHBoxLayout* sublayout = create_horizontal_layout();
    layout->addLayout(sublayout);

    sublayout->addLayout(create_form_layout("Light Samples:", create_double_input("pt.advanced.dl.light_samples", 0.0, 1000000.0, 3, 1.0)));
}

void RenderSettingsWindow::create_pt_advanced_ibl_settings(QVBoxLayout* parent)
{
    QGroupBox* groupbox = new QGroupBox("Image-Based Lighting");
    parent->addWidget(groupbox);

    QVBoxLayout* layout = create_vertical_layout();
    groupbox->setLayout(layout);

    QHBoxLayout* sublayout = create_horizontal_layout();
    layout->addLayout(sublayout);

    sublayout->addLayout(create_form_layout("Environment Samples:", create_double_input("pt.advanced.ibl.env_samples", 0.0, 1000000.0, 3, 1.0)));
}

void RenderSettingsWindow::create_pt_advanced_max_ray_intensity_settings(QVBoxLayout* parent)
{
    QDoubleSpinBox* max_ray_intensity = create_double_input("pt.advanced.max_ray_intensity", 0.0, 1.0e9, 3, 0.5);
    QCheckBox* unlimited_ray_intensity = create_checkbox("pt.advanced.unlimited_ray_intensity", "Unlimited");
    parent->addLayout(create_form_layout("Max Ray Intensity:", create_horizontal_group(max_ray_intensity, unlimited_ray_intensity)));
    connect(unlimited_ray_intensity, SIGNAL(toggled(bool)), max_ray_intensity, SLOT(setDisabled(bool)));
}

//---------------------------------------------------------------------------------------------
// System panel.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_system_panel(QLayout* parent)
{
    FoldablePanelWidget* panel = new FoldablePanelWidget("System");
    parent->addWidget(panel);
    m_panels.push_back(panel);

    panel->fold();

    QVBoxLayout* layout = new QVBoxLayout();
    panel->container()->setLayout(layout);

    create_system_override_rendering_threads_settings(layout);
    create_system_override_texture_store_max_size_settings(layout);
}

void RenderSettingsWindow::create_system_override_rendering_threads_settings(QVBoxLayout* parent)
{
    QGroupBox* groupbox = create_checkable_groupbox("system.rendering_threads.override", "Override");
    parent->addWidget(groupbox);

    QSpinBox* rendering_threads = create_integer_input("system.rendering_threads.value", 1, 65536);
    QCheckBox* auto_rendering_threads = create_checkbox("system.rendering_threads.auto", "Auto");
    groupbox->setLayout(create_form_layout("Rendering Threads:", create_horizontal_group(rendering_threads, auto_rendering_threads)));
    connect(auto_rendering_threads, SIGNAL(toggled(bool)), rendering_threads, SLOT(setDisabled(bool)));
}

void RenderSettingsWindow::create_system_override_texture_store_max_size_settings(QVBoxLayout* parent)
{
    QGroupBox* groupbox = create_checkable_groupbox("system.texture_store_max_size.override", "Override");
    parent->addWidget(groupbox);

    groupbox->setLayout(
        create_form_layout(
            "Texture Store Size:",
            create_integer_input("system.texture_store_max_size.value", 1, 1024 * 1024, "MB")));
}

//---------------------------------------------------------------------------------------------
// Reusable settings.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_bounce_settings(QVBoxLayout* parent, const string& lighting_engine)
{
    const string widget_base_key = lighting_engine + ".bounces.";

    QGroupBox* groupbox = new QGroupBox("Bounces");
    parent->addWidget(groupbox);

    QFormLayout* layout = create_form_layout();
    groupbox->setLayout(layout);

    QSpinBox* max_bounces = create_integer_input(widget_base_key + "max_bounces", 0, 10000);
    QCheckBox* unlimited_bounces = create_checkbox(widget_base_key + "unlimited_bounces", "Unlimited");
    layout->addRow("Max Bounces:", create_horizontal_group(max_bounces, unlimited_bounces));
    connect(unlimited_bounces, SIGNAL(toggled(bool)), max_bounces, SLOT(setDisabled(bool)));

    layout->addRow("Russian Roulette Start Bounce:", create_integer_input(widget_base_key + "rr_start_bounce", 1, 10000));
}

//---------------------------------------------------------------------------------------------
// Base controls.
//---------------------------------------------------------------------------------------------

QHBoxLayout* RenderSettingsWindow::create_horizontal_layout()
{
    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(20);
    return layout;
}

QVBoxLayout* RenderSettingsWindow::create_vertical_layout()
{
    QVBoxLayout* layout = new QVBoxLayout();
    layout->setAlignment(Qt::AlignLeft | Qt::AlignTop);
    return layout;
}

QFormLayout* RenderSettingsWindow::create_form_layout()
{
    QFormLayout* layout = new QFormLayout();
    layout->setLabelAlignment(Qt::AlignRight);
    layout->setSpacing(10);
    return layout;
}

QFormLayout* RenderSettingsWindow::create_form_layout(const QString& label, QWidget* widget)
{
    QFormLayout* layout = create_form_layout();
    layout->addRow(label, widget);
    return layout;
}

QWidget* RenderSettingsWindow::create_horizontal_group(QWidget* widget1, QWidget* widget2)
{
    QWidget* group = new QWidget();

    QHBoxLayout* layout = new QHBoxLayout();
    group->setLayout(layout);

    layout->setMargin(0);
    layout->setSpacing(10);
    layout->setSizeConstraint(QLayout::SetFixedSize);

    layout->addWidget(widget1);
    layout->addWidget(widget2);

    return group;
}

namespace
{
    void set_widget_width_for_text(
        QWidget*            widget,
        const QString&      text,
        const int           margin = 0,
        const int           min_width = 0)
    {
        const QFontMetrics metrics(widget->font());
        const int text_width = metrics.boundingRect(text).width();
        const int final_width = max(text_width + margin, min_width);

        widget->setMinimumWidth(final_width);
        widget->setMaximumWidth(final_width);
    }

    void set_widget_width_for_value(
        QWidget*            widget,
        const int           value,
        const int           margin = 0,
        const int           min_width = 0)
    {
        QString text;
        text.setNum(value);

        set_widget_width_for_text(widget, text, margin, min_width);
    }

    const int SpinBoxMargin = 28;
    const int SpinBoxMinWidth = 40;
}

QSpinBox* RenderSettingsWindow::create_integer_input(
    const string&           widget_key,
    const int               min,
    const int               max)
{
    QSpinBox* spinbox = new QSpinBox();
    m_widget_proxies[widget_key] = new SpinBoxProxy(spinbox);

    spinbox->setRange(min, max);
    set_widget_width_for_value(spinbox, max, SpinBoxMargin, SpinBoxMinWidth);

    new MouseWheelFocusEventFilter(spinbox);

    return spinbox;
}

QSpinBox* RenderSettingsWindow::create_integer_input(
    const string&           widget_key,
    const int               min,
    const int               max,
    const QString&          label)
{
    QSpinBox* spinbox = create_integer_input(widget_key, min, max);

    const QString suffix = " " + label;
    spinbox->setSuffix(suffix);

    QString text;
    text.setNum(max);
    text.append(suffix);

    set_widget_width_for_text(spinbox, text, SpinBoxMargin, SpinBoxMinWidth);

    new MouseWheelFocusEventFilter(spinbox);

    return spinbox;
}

QDoubleSpinBox* RenderSettingsWindow::create_double_input(
    const string&           widget_key,
    const double            min,
    const double            max,
    const int               decimals,
    const double            step)
{
    QDoubleSpinBox* spinbox = new QDoubleSpinBox();
    m_widget_proxies[widget_key] = new DoubleSpinBoxProxy(spinbox);

    spinbox->setMaximumWidth(60);
    spinbox->setRange(min, max);
    spinbox->setDecimals(decimals);
    spinbox->setSingleStep(step);

    new MouseWheelFocusEventFilter(spinbox);

    return spinbox;
}

QCheckBox* RenderSettingsWindow::create_checkbox(
    const string&           widget_key,
    const QString&          label)
{
    QCheckBox* checkbox = new QCheckBox(label);
    m_widget_proxies[widget_key] = new CheckBoxProxy(checkbox);

    return checkbox;
}

QGroupBox* RenderSettingsWindow::create_checkable_groupbox(
    const string&           widget_key,
    const QString&          label)
{
    QGroupBox* groupbox = new QGroupBox(label);
    m_widget_proxies[widget_key] = new GroupBoxProxy(groupbox);

    groupbox->setCheckable(true);

    return groupbox;
}

QComboBox* RenderSettingsWindow::create_combobox(
    const string&           widget_key)
{
    QComboBox* combobox = new QComboBox();
    m_widget_proxies[widget_key] = new ComboBoxProxy(combobox);

    new MouseWheelFocusEventFilter(combobox);

    return combobox;
}

//---------------------------------------------------------------------------------------------
// Configuration loading/saving.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::create_direct_links()
{
    // Image Plane Sampling.
    create_direct_link("image_plane_sampling.general.sampler", "pixel_renderer", "uniform");
    create_direct_link("image_plane_sampling.uniform_sampler.samples", "uniform_pixel_renderer.samples", 64);
    create_direct_link("image_plane_sampling.uniform_sampler.decorrelate_pixels", "uniform_pixel_renderer.decorrelate_pixels", true);
    create_direct_link("image_plane_sampling.adaptive_sampler.min_samples", "adaptive_pixel_renderer.min_samples", 16);
    create_direct_link("image_plane_sampling.adaptive_sampler.max_samples", "adaptive_pixel_renderer.max_samples", 64);
    create_direct_link("image_plane_sampling.adaptive_sampler.quality", "adaptive_pixel_renderer.quality", 3.0);
    create_direct_link("image_plane_sampling.adaptive_sampler.enable_diagnostics", "adaptive_pixel_renderer.enable_diagnostics", false);

    // Lighting.
    create_direct_link("lighting.engine", "lighting_engine", "pt");

    // Distribution Ray Tracer.
    create_direct_link("drt.lighting_components.ibl", "drt.enable_ibl", true);
    create_direct_link("drt.bounces.rr_start_bounce", "drt.rr_min_path_length", 3);
    create_direct_link("drt.advanced.dl.light_samples", "drt.dl_light_samples", 1);
    create_direct_link("drt.advanced.ibl.env_samples", "drt.ibl_env_samples", 1);

    // Unidirectional Path Tracer.
    create_direct_link("pt.lighting_components.dl", "pt.enable_dl", true);
    create_direct_link("pt.lighting_components.ibl", "pt.enable_ibl", true);
    create_direct_link("pt.lighting_components.caustics", "pt.enable_caustics", true);
    create_direct_link("pt.bounces.rr_start_bounce", "pt.rr_min_path_length", 3);
    create_direct_link("pt.advanced.next_event_estimation", "pt.next_event_estimation", true);
    create_direct_link("pt.advanced.dl.light_samples", "pt.dl_light_samples", 1);
    create_direct_link("pt.advanced.ibl.env_samples", "pt.ibl_env_samples", 1);
}

template <typename T>
void RenderSettingsWindow::create_direct_link(
    const string&   widget_key,
    const string&   param_path,
    const T&        default_value)
{
    DirectLink direct_link;
    direct_link.m_widget_key = widget_key;
    direct_link.m_param_path = param_path;
    direct_link.m_default_value = to_string(default_value);

    m_direct_links.push_back(direct_link);
}

void RenderSettingsWindow::load_configuration(const QString& name)
{
    assert(!name.isEmpty());

    do_load_configuration(get_configuration(name));

    m_current_configuration_name = name;
    m_initial_values = get_widget_values();

    set_panels_enabled(
        !BaseConfigurationFactory::is_base_configuration(name.toAscii().constData()));
}

void RenderSettingsWindow::save_current_configuration()
{
    if (m_current_configuration_name.isEmpty())
        return;

    if (BaseConfigurationFactory::is_base_configuration(m_current_configuration_name.toAscii().constData()))
        return;

    do_save_configuration(get_configuration(m_current_configuration_name));

    emit signal_settings_modified();
}

Configuration& RenderSettingsWindow::get_configuration(const QString& name) const
{
    Configuration* configuration =
        m_project_manager.get_project()->configurations().get_by_name(name.toAscii().constData());

    assert(configuration);

    return *configuration;
}

void RenderSettingsWindow::do_load_configuration(const Configuration& config)
{
    load_directly_linked_values(config);

    // Distribution Ray Tracer.
    {
        const size_t DefaultMaxBounces = 8;
        const size_t max_path_length = get_config<size_t>(config, "drt.max_path_length", 0);
        set_widget("drt.bounces.unlimited_bounces", max_path_length == 0);
        set_widget("drt.bounces.max_bounces", max_path_length == 0 ? DefaultMaxBounces : max_path_length - 1);
    }

    // Unidirectional Path Tracer.
    {
        const size_t DefaultMaxBounces = 3;
        const size_t max_path_length = get_config<size_t>(config, "pt.max_path_length", 0);
        set_widget("pt.bounces.unlimited_bounces", max_path_length == 0);
        set_widget("pt.bounces.max_bounces", max_path_length == 0 ? DefaultMaxBounces : max_path_length - 1);
    }
    {
        set_widget("pt.advanced.unlimited_ray_intensity", !config.get_parameters().exist_path("pt.max_ray_intensity"));
        set_widget("pt.advanced.max_ray_intensity", get_config<double>(config, "pt.max_ray_intensity", 1.0));
    }

    // System / Rendering Threads.
    {
        set_widget("system.rendering_threads.override", config.get_parameters().strings().exist("rendering_threads"));
        
        const string default_rendering_threads = to_string(System::get_logical_cpu_core_count());
        const string rendering_threads = get_config<string>(config, "rendering_threads", "auto");
        set_widget("system.rendering_threads.value", rendering_threads == "auto" ? default_rendering_threads : rendering_threads);
        set_widget("system.rendering_threads.auto", rendering_threads == "auto");
    }

    // System / Texture Store Size.
    set_widget("system.texture_store_max_size.override", config.get_inherited_parameters().exist_path("texture_store.max_size"));
    set_widget("system.texture_store_max_size.value", get_config<size_t>(config, "texture_store.max_size", 256 * 1024 * 1024) / (1024 * 1024));
}

void RenderSettingsWindow::do_save_configuration(Configuration& config)
{
    save_directly_linked_values(config);

    // Distribution Ray Tracer.
    set_config(config, "drt.max_path_length",
        get_widget<bool>("drt.bounces.unlimited_bounces") ? 0 : get_widget<size_t>("drt.bounces.max_bounces") + 1);

    // Unidirectional Path Tracer.
    set_config(config, "pt.max_path_length",
        get_widget<bool>("pt.bounces.unlimited_bounces") ? 0 : get_widget<size_t>("pt.bounces.max_bounces") + 1);
    if (get_widget<bool>("pt.advanced.unlimited_ray_intensity"))
        config.get_parameters().remove_path("pt.max_ray_intensity");
    else set_config(config, "pt.max_ray_intensity", get_widget<double>("pt.advanced.max_ray_intensity"));

    // System / Rendering Threads.
    if (get_widget<bool>("system.rendering_threads.override"))
    {
        set_config(config, "rendering_threads",
            get_widget<bool>("system.rendering_threads.auto") ? "auto" : get_widget<string>("system.rendering_threads.value"));
    }
    else config.get_parameters().strings().remove("rendering_threads");

    // System / Texture Store Size.
    if (get_widget<bool>("system.texture_store_max_size.override"))
        set_config(config, "texture_store.max_size", get_widget<size_t>("system.texture_store_max_size.value") * 1024 * 1024);
    else config.get_parameters().remove_path("texture_store.max_size");
}

void RenderSettingsWindow::load_directly_linked_values(const Configuration& config)
{
    for (const_each<DirectLinkCollection> i = m_direct_links; i; ++i)
        set_widget(i->m_widget_key, get_config<string>(config, i->m_param_path, i->m_default_value));
}

void RenderSettingsWindow::save_directly_linked_values(Configuration& config)
{
    for (const_each<DirectLinkCollection> i = m_direct_links; i; ++i)
        set_config(config, i->m_param_path, get_widget<string>(i->m_widget_key));
}

template <typename T>
void RenderSettingsWindow::set_widget(
    const string&           widget_key,
    const T&                value)
{
    assert(m_widget_proxies.find(widget_key) != m_widget_proxies.end());
    m_widget_proxies[widget_key]->set(to_string(value));
}

template <typename T>
T RenderSettingsWindow::get_widget(const string& widget_key)
{
    assert(m_widget_proxies.find(widget_key) != m_widget_proxies.end());
    return from_string<T>(m_widget_proxies[widget_key]->get());
}

template <typename T>
void RenderSettingsWindow::set_config(
    Configuration&          configuration,
    const string&           param_path,
    const T&                value)
{
    configuration.get_parameters().insert_path(param_path, value);
}

template <typename T>
T RenderSettingsWindow::get_config(
    const Configuration&    configuration,
    const string&           param_path,
    const T&                default_value)
{
    return configuration.get_inherited_parameters().
        template get_path_optional<T>(param_path.c_str(), default_value);
}

RenderSettingsWindow::WidgetValueCollection RenderSettingsWindow::get_widget_values() const
{
    map<string, string> values;

    for (const_each<WidgetProxyCollection> i = m_widget_proxies; i; ++i)
        values[i->first] = i->second->get();

    return values;
}

//---------------------------------------------------------------------------------------------
// Slots.
//---------------------------------------------------------------------------------------------

void RenderSettingsWindow::slot_open_configuration_manager_window()
{
    ConfigurationManagerWindow* config_manager_window = new ConfigurationManagerWindow(this);

    config_manager_window->showNormal();
    config_manager_window->activateWindow();
}

namespace
{
    int show_modified_configuration_message_box(QWidget* parent)
    {
        QMessageBox msgbox(parent);
        msgbox.setWindowTitle("Save Changes?");
        msgbox.setIcon(QMessageBox::Question);
        msgbox.setText("This configuration has been modified.");
        msgbox.setInformativeText("Do you want to save your changes?");
        msgbox.setStandardButtons(QMessageBox::Save | QMessageBox::Discard);
        msgbox.setDefaultButton(QMessageBox::Save);
        return msgbox.exec();
    }
}

void RenderSettingsWindow::slot_change_active_configuration(const QString& configuration_name)
{
    if (!m_current_configuration_name.isEmpty())
    {
        if (get_widget_values() != m_initial_values)
        {
            switch (show_modified_configuration_message_box(this))
            {
              case QMessageBox::Save:
                save_current_configuration();
                break;

              case QMessageBox::Discard:
                break;
            }
        }
    }

    if (!configuration_name.isEmpty())
        load_configuration(configuration_name);

    m_ui->scrollareawidget->show();
}

void RenderSettingsWindow::slot_save_configuration_and_close()
{
    save_current_configuration();
    close();
}

void RenderSettingsWindow::slot_restore_configuration_and_close()
{
    load_configuration(m_current_configuration_name);
    close();
}

}   // namespace studio
}   // namespace appleseed
