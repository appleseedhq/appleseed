
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "utility/miscellaneous.h"
#include "utility/mousewheelfocuseventfilter.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/system.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// Qt headers.
#include <QButtonGroup>
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
#include <QRadioButton>
#include <QShortcut>
#include <QSpacerItem>
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

namespace
{
    //
    // Helper functions to build the user interface.
    //

    QHBoxLayout* create_horizontal_layout()
    {
        QHBoxLayout* layout = new QHBoxLayout();
        layout->setSpacing(20);
        return layout;
    }

    QVBoxLayout* create_vertical_layout()
    {
        QVBoxLayout* layout = new QVBoxLayout();
        layout->setAlignment(Qt::AlignLeft | Qt::AlignTop);
        return layout;
    }

    QFormLayout* create_form_layout()
    {
        QFormLayout* layout = new QFormLayout();
        layout->setLabelAlignment(Qt::AlignRight);
        layout->setSpacing(10);
        return layout;
    }

    QFormLayout* create_form_layout(const QString& label, QWidget* widget)
    {
        QFormLayout* layout = create_form_layout();
        layout->addRow(label, widget);
        return layout;
    }

    QWidget* create_horizontal_group(QWidget* widget1, QWidget* widget2 = 0, QWidget* widget3 = 0)
    {
        QWidget* group = new QWidget();

        QHBoxLayout* layout = new QHBoxLayout();
        group->setLayout(layout);

        layout->setMargin(0);
        layout->setSpacing(10);
        layout->setSizeConstraint(QLayout::SetFixedSize);

        layout->addWidget(widget1);

        if (widget2)
            layout->addWidget(widget2);

        if (widget3)
            layout->addWidget(widget3);

        return group;
    }

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
        set_widget_width_for_text(widget, QString::number(value), margin, min_width);
    }

    const int SpinBoxMargin = 28;
    const int SpinBoxMinWidth = 40;
}

//
// RenderSettingsPanel base class.
//

class RenderSettingsPanel
  : public FoldablePanelWidget
{
  public:
    RenderSettingsPanel(const QString& title, QWidget* parent = 0)
      : FoldablePanelWidget(title, parent)
    {
    }

    virtual void save_config(Configuration& config) const = 0;

    map<string, string> get_widget_values() const
    {
        map<string, string> values;

        for (const_each<WidgetProxyCollection> i = m_widget_proxies; i; ++i)
            values[i->first] = i->second->get();

        return values;
    }

  protected:
    typedef map<string, IInputWidgetProxy*> WidgetProxyCollection;

    struct DirectLink
    {
        string  m_widget_key;
        string  m_param_path;
        string  m_default_value;
    };

    typedef vector<DirectLink> DirectLinkCollection;

    WidgetProxyCollection       m_widget_proxies;
    DirectLinkCollection        m_direct_links;

    QSpinBox* create_integer_input(
        const string&           widget_key,
        const int               min,
        const int               max,
        const int               step)
    {
        QSpinBox* spinbox = new QSpinBox();
        m_widget_proxies[widget_key] = new SpinBoxProxy(spinbox);

        spinbox->setRange(min, max);
        spinbox->setSingleStep(step);
        set_widget_width_for_value(spinbox, max, SpinBoxMargin, SpinBoxMinWidth);

        new MouseWheelFocusEventFilter(spinbox);

        return spinbox;
    }

    QSpinBox* create_integer_input(
        const string&           widget_key,
        const int               min,
        const int               max,
        const int               step,
        const QString&          label)
    {
        QSpinBox* spinbox = create_integer_input(widget_key, min, max, step);

        const QString suffix = " " + label;
        spinbox->setSuffix(suffix);

        QString text;
        text.setNum(max);
        text.append(suffix);
        set_widget_width_for_text(spinbox, text, SpinBoxMargin, SpinBoxMinWidth);

        return spinbox;
    }

    QDoubleSpinBox* create_double_input(
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

    QDoubleSpinBox* create_double_input(
        const string&           widget_key,
        const double            min,
        const double            max,
        const int               decimals,
        const double            step,
        const QString&          label)
    {
        QDoubleSpinBox* spinbox =
            create_double_input(
                widget_key,
                min,
                max,
                decimals,
                step);

        const QString suffix = " " + label;
        spinbox->setSuffix(suffix);

        QString text;
        text.setNum(max, 'f', decimals);
        text.append(suffix);
        set_widget_width_for_text(spinbox, text, SpinBoxMargin, SpinBoxMinWidth);

        return spinbox;
    }

    QCheckBox* create_checkbox(
        const string&           widget_key,
        const QString&          label)
    {
        QCheckBox* checkbox = new QCheckBox(label);
        m_widget_proxies[widget_key] = new CheckBoxProxy(checkbox);

        return checkbox;
    }

    QGroupBox* create_checkable_groupbox(
        const string&           widget_key,
        const QString&          label)
    {
        QGroupBox* groupbox = new QGroupBox(label);
        m_widget_proxies[widget_key] = new GroupBoxProxy(groupbox);

        groupbox->setCheckable(true);

        return groupbox;
    }

    QRadioButton* create_radio_button(
        const string&           widget_key,
        const QString&          label)
    {
        QRadioButton* radio_button = new QRadioButton(label);
        m_widget_proxies[widget_key] = new RadioButtonProxy(radio_button);

        return radio_button;
    }

    QComboBox* create_combobox(
        const string&           widget_key)
    {
        QComboBox* combobox = new QComboBox();
        m_widget_proxies[widget_key] = new ComboBoxProxy(combobox);

        new MouseWheelFocusEventFilter(combobox);

        return combobox;
    }

    template <typename T>
    void create_direct_link(
        const string&           widget_key,
        const string&           param_path,
        const T&                default_value)
    {
        DirectLink direct_link;
        direct_link.m_widget_key = widget_key;
        direct_link.m_param_path = param_path;
        direct_link.m_default_value = to_string(default_value);
        m_direct_links.push_back(direct_link);
    }

    void load_directly_linked_values(const Configuration& config)
    {
        for (const_each<DirectLinkCollection> i = m_direct_links; i; ++i)
            set_widget(i->m_widget_key, get_config<string>(config, i->m_param_path, i->m_default_value));
    }

    void save_directly_linked_values(Configuration& config) const
    {
        for (const_each<DirectLinkCollection> i = m_direct_links; i; ++i)
            set_config(config, i->m_param_path, get_widget<string>(i->m_widget_key));
    }

    template <typename T>
    T get_widget(const string& widget_key) const
    {
        const WidgetProxyCollection::const_iterator i = m_widget_proxies.find(widget_key);
        assert(i != m_widget_proxies.end());
        return from_string<T>(i->second->get());
    }

    template <typename T>
    void set_widget(
        const string&           widget_key,
        const T&                value)
    {
        assert(m_widget_proxies.find(widget_key) != m_widget_proxies.end());
        m_widget_proxies[widget_key]->set(to_string(value));
    }

    template <typename T>
    static T get_config(
        const Configuration&    configuration,
        const string&           param_path,
        const T&                default_value)
    {
        return configuration.get_inherited_parameters().
            template get_path_optional<T>(param_path.c_str(), default_value);
    }

    template <typename T>
    static void set_config(
        Configuration&          configuration,
        const string&           param_path,
        const T&                value)
    {
        configuration.get_parameters().insert_path(param_path, value);
    }
};

namespace
{
    //
    // Image Plane Sampling panel.
    //

    class ImagePlaneSamplingPanel
      : public RenderSettingsPanel
    {
        Q_OBJECT

      public:
        ImagePlaneSamplingPanel(const Configuration& config, QWidget* parent = 0)
          : RenderSettingsPanel("Image Plane Sampling", parent)
        {
            QVBoxLayout* layout = new QVBoxLayout();
            container()->setLayout(layout);

            create_image_plane_sampling_general_settings(layout);
            create_image_plane_sampling_sampler_settings(layout);

            create_direct_link("general.sampler", "pixel_renderer", "uniform");
            create_direct_link("general.passes", "generic_frame_renderer.passes", 1);
            create_direct_link("uniform_sampler.samples", "uniform_pixel_renderer.samples", 64);
            create_direct_link("uniform_sampler.force_antialiasing", "uniform_pixel_renderer.force_antialiasing", false);
            create_direct_link("uniform_sampler.decorrelate_pixels", "uniform_pixel_renderer.decorrelate_pixels", true);
            create_direct_link("adaptive_sampler.min_samples", "adaptive_pixel_renderer.min_samples", 16);
            create_direct_link("adaptive_sampler.max_samples", "adaptive_pixel_renderer.max_samples", 64);
            create_direct_link("adaptive_sampler.quality", "adaptive_pixel_renderer.quality", 2.0);
            create_direct_link("adaptive_sampler.enable_diagnostics", "adaptive_pixel_renderer.enable_diagnostics", false);

            load_directly_linked_values(config);
        }

        virtual void save_config(Configuration& config) const APPLESEED_OVERRIDE
        {
            save_directly_linked_values(config);

            set_config(
                config,
                "shading_result_framebuffer",
                get_widget<size_t>("general.passes") > 1 ? "permanent" : "ephemeral");
        }

      private:
        QComboBox*  m_image_plane_sampler_combo;
        QSpinBox*   m_image_plane_sampler_passes;
        QGroupBox*  m_uniform_image_plane_sampler;
        QGroupBox*  m_adaptive_image_plane_sampler;
        QCheckBox*  m_uniform_sampler_decorrelate_pixels;
        QCheckBox*  m_uniform_sampler_force_aa;

        void create_image_plane_sampling_general_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("General");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = new QVBoxLayout();
            groupbox->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            m_image_plane_sampler_combo = create_combobox("general.sampler");
            m_image_plane_sampler_combo->addItem("Uniform", "uniform");
            m_image_plane_sampler_combo->addItem("Adaptive", "adaptive");
            m_image_plane_sampler_combo->setCurrentIndex(-1);
            sublayout->addRow("Sampler:", m_image_plane_sampler_combo);

            connect(
                m_image_plane_sampler_combo, SIGNAL(currentIndexChanged(int)),
                SLOT(slot_changed_image_plane_sampler(const int)));

            m_image_plane_sampler_passes = create_integer_input("general.passes", 1, 1000000, 1);
            sublayout->addRow("Passes:", m_image_plane_sampler_passes);
        }

        void create_image_plane_sampling_sampler_settings(QVBoxLayout* parent)
        {
            QHBoxLayout* layout = create_horizontal_layout();
            parent->addLayout(layout);

            create_image_plane_sampling_uniform_sampler_settings(layout);
            create_image_plane_sampling_adaptive_sampler_settings(layout);
        }

        void create_image_plane_sampling_uniform_sampler_settings(QHBoxLayout* parent)
        {
            m_uniform_image_plane_sampler = new QGroupBox("Uniform Sampler");
            parent->addWidget(m_uniform_image_plane_sampler);

            QVBoxLayout* layout = create_vertical_layout();
            m_uniform_image_plane_sampler->setLayout(layout);

            QSpinBox* samples_spinbox = create_integer_input("uniform_sampler.samples", 1, 1000000, 1);
            layout->addLayout(create_form_layout("Samples:", samples_spinbox));

            m_uniform_sampler_force_aa = create_checkbox("uniform_sampler.force_antialiasing", "Force Antialiasing");
            layout->addWidget(m_uniform_sampler_force_aa);

            connect(
                samples_spinbox, SIGNAL(valueChanged(const int)),
                SLOT(slot_changed_uniform_sampler_samples(const int)));

            m_uniform_sampler_decorrelate_pixels =
                create_checkbox("uniform_sampler.decorrelate_pixels", "Decorrelate Pixels");
            layout->addWidget(m_uniform_sampler_decorrelate_pixels);

            connect(
                m_image_plane_sampler_passes, SIGNAL(valueChanged(const int)),
                SLOT(slot_changed_image_plane_sampler_passes(const int)));
        }

        void create_image_plane_sampling_adaptive_sampler_settings(QHBoxLayout* parent)
        {
            m_adaptive_image_plane_sampler = new QGroupBox("Adaptive Sampler");
            parent->addWidget(m_adaptive_image_plane_sampler);

            QVBoxLayout* layout = create_vertical_layout();
            m_adaptive_image_plane_sampler->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            sublayout->addRow("Min Samples:", create_integer_input("adaptive_sampler.min_samples", 1, 1000000, 1));
            sublayout->addRow("Max Samples:", create_integer_input("adaptive_sampler.max_samples", 1, 1000000, 1));
            sublayout->addRow("Quality:", create_double_input("adaptive_sampler.quality", -20.0, +20.0, 2, 0.5));

            layout->addWidget(create_checkbox("adaptive_sampler.enable_diagnostics", "Enable Diagnostic AOVs"));
        }

      private slots:
        void slot_changed_image_plane_sampler(const int index)
        {
            const QString sampler = m_image_plane_sampler_combo->itemData(index).value<QString>();

            m_uniform_image_plane_sampler->setEnabled(sampler == "uniform");
            m_adaptive_image_plane_sampler->setEnabled(sampler == "adaptive");
        }

        void slot_changed_image_plane_sampler_passes(const int passes)
        {
            if (passes > 1)
            {
                m_uniform_sampler_decorrelate_pixels->setChecked(true);
                m_uniform_sampler_decorrelate_pixels->setDisabled(true);
            }
            else
            {
                m_uniform_sampler_decorrelate_pixels->setDisabled(false);
            }
        }

        void slot_changed_uniform_sampler_samples(const int samples)
        {
            m_uniform_sampler_force_aa->setDisabled(samples > 1);
        }
    };

    //
    // Lighting panels.
    //

    class LightingPanel
      : public RenderSettingsPanel
    {
      public:
        LightingPanel(const Configuration& config, QWidget* parent = 0)
          : RenderSettingsPanel("Lighting", parent)
        {
        }

        virtual void save_config(Configuration& config) const APPLESEED_OVERRIDE
        {
            save_directly_linked_values(config);
        }

      protected:
        void construct(const Configuration& config, QComboBox* engine_combobox)
        {
            QFormLayout* layout = create_form_layout();
            container()->setLayout(layout);
            layout->addRow("Engine:", engine_combobox);

            create_direct_link("engine", "lighting_engine", "pt");

            load_directly_linked_values(config);
        }
    };

    class FinalConfigurationLightingPanel
      : public LightingPanel
    {
      public:
        FinalConfigurationLightingPanel(const Configuration& config, QWidget* parent = 0)
          : LightingPanel(config, parent)
        {
            QComboBox* combobox = create_combobox("engine");
            combobox->addItem("Distribution Ray Tracer", "drt");
            combobox->addItem("Unidirectional Path Tracer", "pt");
            combobox->addItem("Stochastic Progressive Photon Mapping", "sppm");
            construct(config, combobox);
        }
    };

    class InteractiveConfigurationLightingPanel
      : public LightingPanel
    {
      public:
        InteractiveConfigurationLightingPanel(const Configuration& config, QWidget* parent = 0)
          : LightingPanel(config, parent)
        {
            QComboBox* combobox = create_combobox("engine");
            combobox->addItem("Distribution Ray Tracer", "drt");
            combobox->addItem("Unidirectional Path Tracer", "pt");
            construct(config, combobox);
        }
    };

    //
    // Lighting Engine panel.
    //

    class LightingEnginePanel
      : public RenderSettingsPanel
    {
      public:
        LightingEnginePanel(const QString& title, QWidget* parent = 0)
          : RenderSettingsPanel(title, parent)
        {
        }

      protected:
        void create_bounce_settings_group(QVBoxLayout* parent, const string& prefix)
        {
            QGroupBox* groupbox = new QGroupBox("Bounces");
            parent->addWidget(groupbox);

            QFormLayout* layout = create_form_layout();
            groupbox->setLayout(layout);

            create_bounce_settings(layout, prefix);
        }

        void create_bounce_settings(QFormLayout* layout, const string& prefix)
        {
            const string widget_base_key = prefix + ".bounces.";

            QSpinBox* max_bounces = create_integer_input(widget_base_key + "max_bounces", 0, 10000, 1);
            QCheckBox* unlimited_bounces = create_checkbox(widget_base_key + "unlimited_bounces", "Unlimited");
            layout->addRow("Max Bounces:", create_horizontal_group(max_bounces, unlimited_bounces));
            connect(unlimited_bounces, SIGNAL(toggled(bool)), max_bounces, SLOT(setDisabled(bool)));

            layout->addRow("Russian Roulette Start Bounce:", create_integer_input(widget_base_key + "rr_start_bounce", 1, 10000, 1));
        }

        void load_bounce_settings(
            const Configuration&    config,
            const string&           widget_key_prefix,
            const string&           param_path)
        {
            const size_t DefaultMaxBounces = 8;

            const size_t max_path_length =
                get_config<size_t>(config, param_path, 0);

            set_widget(widget_key_prefix + ".bounces.unlimited_bounces", max_path_length == 0);
            set_widget(widget_key_prefix + ".bounces.max_bounces", max_path_length == 0 ? DefaultMaxBounces : max_path_length - 1);
        }

        void save_bounce_settings(
            Configuration&          config,
            const string&           widget_key_prefix,
            const string&           param_path) const
        {
            const size_t max_path_length =
                !get_widget<bool>(widget_key_prefix + ".bounces.unlimited_bounces")
                    ? get_widget<size_t>(widget_key_prefix + ".bounces.max_bounces") + 1
                    : 0;

            set_config(config, param_path, max_path_length);
        }
    };

    //
    // Distribution Ray Tracer panel.
    //

    class DistributionRayTracerPanel
      : public LightingEnginePanel
    {
      public:
        DistributionRayTracerPanel(const Configuration& config, QWidget* parent = 0)
          : LightingEnginePanel("Distribution Ray Tracer", parent)
        {
            fold();

            QVBoxLayout* layout = new QVBoxLayout();
            container()->setLayout(layout);

            QGroupBox* groupbox = new QGroupBox("Components");
            layout->addWidget(groupbox);

            QVBoxLayout* sublayout = new QVBoxLayout();
            groupbox->setLayout(sublayout);

            sublayout->addWidget(create_checkbox("lighting_components.ibl", "Image-Based Lighting"));

            create_bounce_settings_group(layout, "drt");
            create_drt_advanced_settings(layout);

            create_direct_link("lighting_components.ibl", "drt.enable_ibl", true);
            create_direct_link("drt.bounces.rr_start_bounce", "drt.rr_min_path_length", 3);
            create_direct_link("advanced.dl.light_samples", "drt.dl_light_samples", 1);
            create_direct_link("advanced.ibl.env_samples", "drt.ibl_env_samples", 1);

            load_directly_linked_values(config);

            load_bounce_settings(config, "drt", "drt.max_path_length");
        }

        virtual void save_config(Configuration& config) const APPLESEED_OVERRIDE
        {
            save_directly_linked_values(config);

            save_bounce_settings(config, "drt", "drt.max_path_length");
        }

      private:
        void create_drt_advanced_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Advanced");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = new QVBoxLayout();
            groupbox->setLayout(layout);

            create_drt_advanced_dl_settings(layout);
            create_drt_advanced_ibl_settings(layout);
        }

        void create_drt_advanced_dl_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Direct Lighting");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = create_vertical_layout();
            groupbox->setLayout(layout);

            QHBoxLayout* sublayout = create_horizontal_layout();
            layout->addLayout(sublayout);

            sublayout->addLayout(create_form_layout("Light Samples:", create_double_input("advanced.dl.light_samples", 0.0, 1000000.0, 3, 1.0)));
        }

        void create_drt_advanced_ibl_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Image-Based Lighting");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = create_vertical_layout();
            groupbox->setLayout(layout);

            QHBoxLayout* sublayout = create_horizontal_layout();
            layout->addLayout(sublayout);

            sublayout->addLayout(create_form_layout("Environment Samples:", create_double_input("advanced.ibl.env_samples", 0.0, 1000000.0, 3, 1.0)));
        }
    };

    //
    // Unidirectional Path Tracer panel.
    //

    class UnidirectionalPathTracerPanel
      : public LightingEnginePanel
    {
      public:
        UnidirectionalPathTracerPanel(const Configuration& config, QWidget* parent = 0)
          : LightingEnginePanel("Unidirectional Path Tracer", parent)
        {
            fold();

            QVBoxLayout* layout = new QVBoxLayout();
            container()->setLayout(layout);

            QGroupBox* groupbox = new QGroupBox("Components");
            layout->addWidget(groupbox);

            QVBoxLayout* sublayout = new QVBoxLayout();
            groupbox->setLayout(sublayout);

            sublayout->addWidget(create_checkbox("lighting_components.dl", "Direct Lighting"));
            sublayout->addWidget(create_checkbox("lighting_components.ibl", "Image-Based Lighting"));
            sublayout->addWidget(create_checkbox("lighting_components.caustics", "Caustics"));

            create_bounce_settings_group(layout, "pt");
            create_pt_advanced_settings(layout);

            create_direct_link("lighting_components.dl", "pt.enable_dl", true);
            create_direct_link("lighting_components.ibl", "pt.enable_ibl", true);
            create_direct_link("lighting_components.caustics", "pt.enable_caustics", false);
            create_direct_link("pt.bounces.rr_start_bounce", "pt.rr_min_path_length", 3);
            create_direct_link("advanced.next_event_estimation", "pt.next_event_estimation", true);
            create_direct_link("advanced.dl.light_samples", "pt.dl_light_samples", 1);
            create_direct_link("advanced.ibl.env_samples", "pt.ibl_env_samples", 1);

            load_directly_linked_values(config);

            load_bounce_settings(config, "pt", "pt.max_path_length");

            set_widget("advanced.unlimited_ray_intensity", !config.get_parameters().exist_path("pt.max_ray_intensity"));
            set_widget("advanced.max_ray_intensity", get_config<double>(config, "pt.max_ray_intensity", 1.0));
        }

        virtual void save_config(Configuration& config) const APPLESEED_OVERRIDE
        {
            save_directly_linked_values(config);

            save_bounce_settings(config, "pt", "pt.max_path_length");

            if (get_widget<bool>("advanced.unlimited_ray_intensity"))
                config.get_parameters().remove_path("pt.max_ray_intensity");
            else set_config(config, "pt.max_ray_intensity", get_widget<double>("advanced.max_ray_intensity"));
        }

      private:
        void create_pt_advanced_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Advanced");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = new QVBoxLayout();
            groupbox->setLayout(layout);

            QGroupBox* nee_groupbox = create_checkable_groupbox("advanced.next_event_estimation", "Next Event Estimation");
            layout->addWidget(nee_groupbox);

            QVBoxLayout* nee_layout = create_vertical_layout();
            nee_groupbox->setLayout(nee_layout);

            create_pt_advanced_dl_settings(nee_layout);
            create_pt_advanced_ibl_settings(nee_layout);
            create_pt_advanced_max_ray_intensity_settings(nee_layout);
        }

        void create_pt_advanced_dl_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Direct Lighting");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = create_vertical_layout();
            groupbox->setLayout(layout);

            QHBoxLayout* sublayout = create_horizontal_layout();
            layout->addLayout(sublayout);

            sublayout->addLayout(create_form_layout("Light Samples:", create_double_input("advanced.dl.light_samples", 0.0, 1000000.0, 3, 1.0)));
        }

        void create_pt_advanced_ibl_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Image-Based Lighting");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = create_vertical_layout();
            groupbox->setLayout(layout);

            QHBoxLayout* sublayout = create_horizontal_layout();
            layout->addLayout(sublayout);

            sublayout->addLayout(create_form_layout("Environment Samples:", create_double_input("advanced.ibl.env_samples", 0.0, 1000000.0, 3, 1.0)));
        }

        void create_pt_advanced_max_ray_intensity_settings(QVBoxLayout* parent)
        {
            QDoubleSpinBox* max_ray_intensity = create_double_input("advanced.max_ray_intensity", 0.0, 1.0e9, 3, 0.1);
            QCheckBox* unlimited_ray_intensity = create_checkbox("advanced.unlimited_ray_intensity", "Unlimited");
            parent->addLayout(create_form_layout("Max Ray Intensity:", create_horizontal_group(max_ray_intensity, unlimited_ray_intensity)));
            connect(unlimited_ray_intensity, SIGNAL(toggled(bool)), max_ray_intensity, SLOT(setDisabled(bool)));
        }
    };

    //
    // Stochastic Progressive Photon Mapping panel.
    //

    class SPPMPanel
      : public LightingEnginePanel
    {
      public:
        SPPMPanel(const Configuration& config, QWidget* parent = 0)
          : LightingEnginePanel("Stochastic Progressive Photon Mapping", parent)
        {
            fold();

            QVBoxLayout* layout = new QVBoxLayout();
            container()->setLayout(layout);

            create_photon_type_settings(layout);
            create_components_settings(layout);
            create_photon_tracing_settings(layout);
            create_radiance_estimation_settings(layout);

            create_direct_link("lighting_components.ibl", "sppm.enable_ibl", true);
            create_direct_link("lighting_components.caustics", "sppm.enable_caustics", true);
            create_direct_link("photon_tracing.bounces.rr_start_bounce", "sppm.photon_tracing_rr_min_path_length", 3);
            create_direct_link("photon_tracing.light_photons", "sppm.light_photons_per_pass", 1000000);
            create_direct_link("photon_tracing.env_photons", "sppm.env_photons_per_pass", 1000000);
            create_direct_link("radiance_estimation.bounces.rr_start_bounce", "sppm.path_tracing_rr_min_path_length", 3);
            create_direct_link("radiance_estimation.initial_radius", "sppm.initial_radius", 1.0);
            create_direct_link("radiance_estimation.max_photons", "sppm.max_photons_per_estimate", 100);
            create_direct_link("radiance_estimation.alpha", "sppm.alpha", 0.7);

            load_directly_linked_values(config);

            load_bounce_settings(config, "photon_tracing", "sppm.photon_tracing_max_path_length");
            load_bounce_settings(config, "radiance_estimation", "sppm.path_tracing_max_path_length");

            const string dl_mode = get_config<string>(config, "sppm.dl_mode", "rt");
            if (dl_mode == "rt")
                set_widget("lighting_components.dl.rt", true);
            else if (dl_mode == "sppm")
                set_widget("lighting_components.dl.sppm", true);
            else set_widget("lighting_components.dl.off", true);

            const string photon_type = get_config<string>(config, "sppm.photon_type", "mono");
            if (photon_type == "mono")
                set_widget("photon_type.mono", true);
            else set_widget("photon_type.poly", true);
        }

        virtual void save_config(Configuration& config) const APPLESEED_OVERRIDE
        {
            save_directly_linked_values(config);

            save_bounce_settings(config, "photon_tracing", "sppm.photon_tracing_max_path_length");
            save_bounce_settings(config, "radiance_estimation", "sppm.path_tracing_max_path_length");

            set_config(config, "sppm.dl_mode",
                get_widget<bool>("lighting_components.dl.rt") ? "rt" :
                get_widget<bool>("lighting_components.dl.sppm") ? "sppm" : "off");

            set_config(config, "sppm.photon_type",
                get_widget<bool>("photon_type.mono") ? "mono" : "poly");
        }

      private:
        void create_photon_type_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Photon Type");
            parent->addWidget(groupbox);

            QButtonGroup* buttons = new QButtonGroup(groupbox);
            QRadioButton* buttons_mono = create_radio_button("photon_type.mono", "Monochromatic Photons");
            QRadioButton* buttons_poly = create_radio_button("photon_type.poly", "Polychromatic Photons");
            buttons->addButton(buttons_mono);
            buttons->addButton(buttons_poly);

            QHBoxLayout* photontype_buttons_layout = new QHBoxLayout();
            photontype_buttons_layout->addWidget(buttons_mono);
            photontype_buttons_layout->addWidget(buttons_poly);
            groupbox->setLayout(photontype_buttons_layout);
        }

        void create_components_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Components");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = new QVBoxLayout();
            groupbox->setLayout(layout);

            QButtonGroup* buttons = new QButtonGroup(groupbox);
            QRadioButton* buttons_rt = create_radio_button("lighting_components.dl.rt", "RT Direct Lighting");
            QRadioButton* buttons_sppm = create_radio_button("lighting_components.dl.sppm", "SPPM Direct Lighting");
            QRadioButton* buttons_off = create_radio_button("lighting_components.dl.off", "No Direct Lighting");
            buttons->addButton(buttons_rt);
            buttons->addButton(buttons_sppm);
            buttons->addButton(buttons_off);

            QHBoxLayout* dl_buttons_layout = new QHBoxLayout();
            dl_buttons_layout->addWidget(buttons_rt);
            dl_buttons_layout->addWidget(buttons_sppm);
            dl_buttons_layout->addWidget(buttons_off);
            layout->addLayout(dl_buttons_layout);

            layout->addWidget(create_checkbox("lighting_components.ibl", "Image-Based Lighting"));
            layout->addWidget(create_checkbox("lighting_components.caustics", "Caustics"));
        }

        void create_photon_tracing_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Photon Tracing");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = new QVBoxLayout();
            groupbox->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            create_bounce_settings(sublayout, "photon_tracing");
            sublayout->addRow("Light Photons:", create_integer_input("photon_tracing.light_photons", 0, 1000000000, 100000));
            sublayout->addRow("Environment Photons:", create_integer_input("photon_tracing.env_photons", 0, 1000000000, 100000));
        }

        void create_radiance_estimation_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Radiance Estimation");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = new QVBoxLayout();
            groupbox->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            create_bounce_settings(sublayout, "radiance_estimation");
            sublayout->addRow("Initial Radius:", create_double_input("radiance_estimation.initial_radius", 0.001, 100.0, 3, 0.1, "%"));
            sublayout->addRow("Max Photons:", create_integer_input("radiance_estimation.max_photons", 8, 1000000000, 50));
            sublayout->addRow("Alpha:", create_double_input("radiance_estimation.alpha", 0.0, 1.0, 2, 0.1));
        }
    };

    //
    // System panel.
    //

    class SystemPanel
      : public LightingEnginePanel
    {
      public:
        SystemPanel(const Configuration& config, QWidget* parent = 0)
          : LightingEnginePanel("System", parent)
        {
            fold();

            QVBoxLayout* layout = new QVBoxLayout();
            container()->setLayout(layout);

            create_system_override_rendering_threads_settings(layout);
            create_system_override_texture_store_max_size_settings(layout);
            create_system_override_tile_ordering_settings(layout);

            load_directly_linked_values(config);

            set_widget("rendering_threads.override", config.get_parameters().strings().exist("rendering_threads"));

            const string default_rendering_threads = to_string(System::get_logical_cpu_core_count());
            const string rendering_threads = get_config<string>(config, "rendering_threads", "auto");
            set_widget("rendering_threads.value", rendering_threads == "auto" ? default_rendering_threads : rendering_threads);
            set_widget("rendering_threads.auto", rendering_threads == "auto");

            const size_t MB = 1024 * 1024;
            const size_t DefaultTextureStoreSizeMB = 1024 * MB;
            set_widget("texture_store_max_size.override", config.get_inherited_parameters().exist_path("texture_store.max_size"));
            set_widget("texture_store_max_size.value", get_config<size_t>(config, "texture_store.max_size", DefaultTextureStoreSizeMB) / MB);

            set_widget("tile_ordering.override", config.get_parameters().exist_path("generic_frame_renderer.tile_ordering"));
            set_widget("tile_ordering.value", get_config<string>(config, "generic_frame_renderer.tile_ordering", "hilbert"));
        }

        virtual void save_config(Configuration& config) const APPLESEED_OVERRIDE
        {
            save_directly_linked_values(config);

            if (get_widget<bool>("rendering_threads.override"))
            {
                set_config(config, "rendering_threads",
                    get_widget<bool>("rendering_threads.auto") ? "auto" : get_widget<string>("rendering_threads.value"));
            }
            else config.get_parameters().strings().remove("rendering_threads");

            if (get_widget<bool>("texture_store_max_size.override"))
                set_config(config, "texture_store.max_size", get_widget<size_t>("texture_store_max_size.value") * 1024 * 1024);
            else config.get_parameters().remove_path("texture_store.max_size");

            if (get_widget<bool>("tile_ordering.override"))
            {
                set_config(config, "generic_frame_renderer.tile_ordering", get_widget<string>("tile_ordering.value"));
            }
            else config.get_parameters().remove_path("generic_frame_renderer.tile_ordering");
        }

      private:
        void create_system_override_rendering_threads_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = create_checkable_groupbox("rendering_threads.override", "Override");
            parent->addWidget(groupbox);

            QSpinBox* rendering_threads = create_integer_input("rendering_threads.value", 1, 65536, 1);
            QCheckBox* auto_rendering_threads = create_checkbox("rendering_threads.auto", "Auto");
            groupbox->setLayout(create_form_layout("Rendering Threads:", create_horizontal_group(rendering_threads, auto_rendering_threads)));
            connect(auto_rendering_threads, SIGNAL(toggled(bool)), rendering_threads, SLOT(setDisabled(bool)));
        }

        void create_system_override_texture_store_max_size_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = create_checkable_groupbox("texture_store_max_size.override", "Override");
            parent->addWidget(groupbox);

            groupbox->setLayout(
                create_form_layout(
                    "Texture Store Size:",
                    create_integer_input("texture_store_max_size.value", 1, 1024 * 1024, 256, "MB")));
        }

        void create_system_override_tile_ordering_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = create_checkable_groupbox("tile_ordering.override", "Override");
            parent->addWidget(groupbox);

            QComboBox* tile_ordering = create_combobox("tile_ordering.value");
            tile_ordering->addItem("Linear", "linear");
            tile_ordering->addItem("Spiral", "spiral");
            tile_ordering->addItem("Hilbert", "hilbert");
            tile_ordering->addItem("Random", "random");
            groupbox->setLayout(create_form_layout("Tile Ordering:", tile_ordering));
        }
    };
}


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

    create_connections();

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

void RenderSettingsWindow::create_connections()
{
    connect(m_ui->pushbutton_manage, SIGNAL(clicked()), SLOT(slot_open_configuration_manager_window()));

    connect(
        m_ui->combobox_configurations, SIGNAL(currentIndexChanged(const QString&)),
        SLOT(slot_change_active_configuration(const QString&)));

    connect(m_ui->buttonbox, SIGNAL(accepted()), SLOT(slot_save_configuration_and_close()));
    connect(m_ui->buttonbox, SIGNAL(rejected()), SLOT(slot_restore_configuration_and_close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Return), SIGNAL(activated()),
        SLOT(slot_save_configuration_and_close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Enter), SIGNAL(activated()),
        SLOT(slot_save_configuration_and_close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Escape), SIGNAL(activated()),
        SLOT(slot_restore_configuration_and_close()));
}

namespace
{
    bool is_interactive_configuration(const Configuration& config)
    {
        if (BaseConfigurationFactory::is_base_interactive_configuration(config.get_name()))
            return true;

        const Configuration* base_config = config.get_base();

        if (base_config == 0)
            return false;

        return is_interactive_configuration(*base_config);
    }
}

void RenderSettingsWindow::create_panels(const Configuration& config)
{
    const bool interactive = is_interactive_configuration(config);

    m_panels.clear();

    if (!interactive)
        m_panels.push_back(new ImagePlaneSamplingPanel(config));

    if (interactive)
        m_panels.push_back(new InteractiveConfigurationLightingPanel(config));
    else m_panels.push_back(new FinalConfigurationLightingPanel(config));

    m_panels.push_back(new DistributionRayTracerPanel(config));
    m_panels.push_back(new UnidirectionalPathTracerPanel(config));

    if (!interactive)
        m_panels.push_back(new SPPMPanel(config));

    m_panels.push_back(new SystemPanel(config));
}

void RenderSettingsWindow::create_layout()
{
    QLayout* root_layout = m_ui->scrollareawidget->layout();
    assert(root_layout);

    clear_layout(root_layout);

    root_layout->addItem(new QSpacerItem(470, 0, QSizePolicy::Expanding, QSizePolicy::Minimum));

    for (const_each<PanelCollection> i = m_panels; i; ++i)
        root_layout->addWidget(*i);
}

void RenderSettingsWindow::set_panels_enabled(const bool enabled)
{
    for (const_each<PanelCollection> i = m_panels; i; ++i)
        (*i)->container()->setEnabled(enabled);
}

void RenderSettingsWindow::load_configuration(const QString& name)
{
    assert(!name.isEmpty());

    const Configuration& config = get_configuration(name);

    create_panels(config);
    create_layout();

    set_panels_enabled(
        !BaseConfigurationFactory::is_base_configuration(
            name.toAscii().constData()));

    m_current_configuration_name = name;
    m_initial_values = get_widget_values();
}

void RenderSettingsWindow::save_current_configuration()
{
    if (m_current_configuration_name.isEmpty())
        return;

    if (BaseConfigurationFactory::is_base_configuration(m_current_configuration_name.toAscii().constData()))
        return;

    Configuration& config = get_configuration(m_current_configuration_name);

    for (const_each<PanelCollection> i = m_panels; i; ++i)
        (*i)->save_config(config);

    m_initial_values = get_widget_values();

    emit signal_settings_modified();
}

Configuration& RenderSettingsWindow::get_configuration(const QString& name) const
{
    Configuration* configuration =
        m_project_manager.get_project()->configurations().get_by_name(name.toAscii().constData());

    assert(configuration);

    return *configuration;
}

map<string, string> RenderSettingsWindow::get_widget_values() const
{
    map<string, string> values;

    for (const_each<PanelCollection> i = m_panels; i; ++i)
    {
        const map<string, string> panel_values = (*i)->get_widget_values();
        values.insert(panel_values.begin(), panel_values.end());
    }

    return values;
}

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

#include "mainwindow/moc_cpp_rendersettingswindow.cxx"
