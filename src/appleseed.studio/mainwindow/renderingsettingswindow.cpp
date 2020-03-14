
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
#include "renderingsettingswindow.h"

// UI definition header.
#include "ui_renderingsettingswindow.h"

// appleseed.studio headers.
#include "mainwindow/collapsiblesectionwidget.h"
#include "mainwindow/configurationmanagerwindow.h"
#include "utility/inputwidgetproxies.h"
#include "utility/settingskeys.h"

// appleseed.qtcommon headers.
#include "project/projectmanager.h"
#include "utility/interop.h"
#include "utility/miscellaneous.h"
#include "widgets/foldablepanelwidget.h"
#include "widgets/mousewheelfocuseventfilter.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/system.h"
#include "foundation/string/string.h"

// Qt headers.
#include <QButtonGroup>
#include <QCheckBox>
#include <QComboBox>
#include <QDoubleSpinBox>
#include <QFontMetrics>
#include <QFormLayout>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QKeySequence>
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QMessageBox>
#include <QRadioButton>
#include <QShortcut>
#include <QSpacerItem>
#include <QSpinBox>
#include <Qt>
#include <QVBoxLayout>
#include <QWidget>

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <iterator>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

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

    QWidget* create_horizontal_group(QWidget* widget1, QWidget* widget2 = nullptr, QWidget* widget3 = nullptr, QWidget* widget4 = nullptr)
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

        if (widget4)
            layout->addWidget(widget4);

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
        const int final_width = std::max(text_width + margin, min_width);

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

    constexpr int SpinBoxMargin = 28;
    constexpr int SpinBoxMinWidth = 40;
}

//
// RenderSettingsPanel base class.
//

class RenderSettingsPanel
  : public FoldablePanelWidget
{
  public:
    explicit RenderSettingsPanel(const QString& title, QWidget* parent = nullptr)
      : FoldablePanelWidget(title, parent)
      , m_params_metadata(Configuration::get_metadata())
    {
    }

    virtual void save_config(Configuration& config) const = 0;

    std::map<std::string, std::string> get_widget_values() const
    {
        std::map<std::string, std::string> values;

        for (const auto& item : m_widget_proxies)
            values[item.first] = item.second->get();

        return values;
    }

  protected:
    typedef std::map<std::string, IInputWidgetProxy*> WidgetProxyCollection;

    struct DirectLink
    {
        std::string  m_widget_key;
        std::string  m_param_path;
        std::string  m_default_value;
    };

    typedef std::vector<DirectLink> DirectLinkCollection;

    const ParamArray            m_params_metadata;

    WidgetProxyCollection       m_widget_proxies;
    DirectLinkCollection        m_direct_links;

    QSpinBox* create_integer_input(
        const std::string&      widget_key,
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
        const std::string&      widget_key,
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
        const std::string&      widget_key,
        const double            min,
        const double            max,
        const int               decimals,
        const double            step)
    {
        QDoubleSpinBox* spinbox = new QDoubleSpinBox();
        m_widget_proxies[widget_key] = new DoubleSpinBoxProxy(spinbox);

        spinbox->setRange(min, max);
        spinbox->setDecimals(decimals);
        spinbox->setSingleStep(step);

        QString text;
        text.setNum(max, 'f', decimals);
        set_widget_width_for_text(spinbox, text, SpinBoxMargin, SpinBoxMinWidth);

        new MouseWheelFocusEventFilter(spinbox);

        return spinbox;
    }

    QDoubleSpinBox* create_double_input(
        const std::string&      widget_key,
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
        const std::string&      widget_key,
        const QString&          label)
    {
        QCheckBox* checkbox = new QCheckBox(label);
        m_widget_proxies[widget_key] = new CheckBoxProxy(checkbox);

        return checkbox;
    }

    QGroupBox* create_checkable_groupbox(
        const std::string&      widget_key,
        const QString&          label)
    {
        QGroupBox* groupbox = new QGroupBox(label);
        m_widget_proxies[widget_key] = new GroupBoxProxy(groupbox);

        groupbox->setCheckable(true);

        return groupbox;
    }

    QRadioButton* create_radio_button(
        const std::string&      widget_key,
        const QString&          label)
    {
        QRadioButton* radio_button = new QRadioButton(label);
        m_widget_proxies[widget_key] = new RadioButtonProxy(radio_button);

        return radio_button;
    }

    QComboBox* create_combobox(
        const std::string&      widget_key)
    {
        QComboBox* combobox = new QComboBox();
        m_widget_proxies[widget_key] = new ComboBoxProxy(combobox);

        new MouseWheelFocusEventFilter(combobox);

        return combobox;
    }

    QLineEdit* create_line_edit(
        const std::string&      widget_key)
    {
        QLineEdit* line_edit = new QLineEdit();
        m_widget_proxies[widget_key] = new LineEditProxy(line_edit);

        return line_edit;
    }

    void create_direct_link(
        const std::string&      widget_key,
        const std::string&      param_path,
        const std::string&      default_value = std::string())
    {
        DirectLink direct_link;
        direct_link.m_widget_key = widget_key;
        direct_link.m_param_path = param_path;
        direct_link.m_default_value = default_value;
        m_direct_links.push_back(direct_link);
    }

    void load_directly_linked_values(const Configuration& config)
    {
        for (const DirectLink& link : m_direct_links)
        {
            const std::string default_value_path = link.m_param_path + ".default";
            const std::string default_value =
                m_params_metadata.get_path_optional<std::string>(
                    default_value_path.c_str(),
                    link.m_default_value);
            const std::string value = get_config<std::string>(config, link.m_param_path, default_value);
            set_widget(link.m_widget_key, value);
        }
    }

    void save_directly_linked_values(Configuration& config) const
    {
        for (const DirectLink& link : m_direct_links)
            set_config(config, link.m_param_path, get_widget<std::string>(link.m_widget_key));
    }

    template <typename T>
    T get_widget(const std::string& widget_key) const
    {
        const WidgetProxyCollection::const_iterator i = m_widget_proxies.find(widget_key);
        assert(i != m_widget_proxies.end());
        return from_string<T>(i->second->get());
    }

    template <typename T>
    void set_widget(
        const std::string&      widget_key,
        const T&                value)
    {
        assert(m_widget_proxies.find(widget_key) != m_widget_proxies.end());
        m_widget_proxies[widget_key]->set(to_string(value));
    }

    template <typename T>
    static T get_config(
        const Configuration&    configuration,
        const std::string&      param_path,
        const T&                default_value)
    {
        return configuration.get_inherited_parameters().
            template get_path_optional<T>(param_path.c_str(), default_value);
    }

    template <typename T>
    static void set_config(
        Configuration&          configuration,
        const std::string&      param_path,
        const T&                value)
    {
        configuration.get_parameters().insert_path(param_path.c_str(), value);
    }
};

namespace
{
    //
    // General settings panel.
    //

    class GeneralSettingsPanel
      : public RenderSettingsPanel
    {
        Q_OBJECT

      public:
        explicit GeneralSettingsPanel(QWidget* parent = nullptr)
          : RenderSettingsPanel("General Settings", parent)
        {
        }

      protected:
        void create_devices(QFormLayout* parent)
        {
            QComboBox* device_combobox = create_combobox("device");
            device_combobox->setToolTip(m_params_metadata.get_path("device.help"));
            device_combobox->addItem("CPU", "cpu");
            parent->addRow("Device:", device_combobox);

            create_direct_link("device", "device", "cpu");
        }

        void create_color_pipeline(QFormLayout* parent)
        {
            QComboBox* color_pipeline_combobox = create_combobox("spectrum_mode");
            color_pipeline_combobox->setToolTip(m_params_metadata.get_path("spectrum_mode.help"));
            color_pipeline_combobox->addItem("RGB", "rgb");
            color_pipeline_combobox->addItem("Spectral", "spectral");
            parent->addRow("Color Pipeline:", color_pipeline_combobox);

            create_direct_link("spectrum_mode", "spectrum_mode", "rgb");
        }
    };

    class FinalGeneralSettingsPanel
      : public GeneralSettingsPanel
    {
        Q_OBJECT

      public:
        explicit FinalGeneralSettingsPanel(const Configuration& config, QWidget* parent = nullptr)
          : GeneralSettingsPanel(parent)
        {
            QFormLayout* layout = create_form_layout();
            container()->setLayout(layout);

            create_devices(layout);
            create_color_pipeline(layout);

            load_directly_linked_values(config);
        }

        void save_config(Configuration& config) const override
        {
            save_directly_linked_values(config);
        }
    };

    class InteractiveGeneralSettingsPanel
      : public GeneralSettingsPanel
    {
        Q_OBJECT

      public:
        explicit InteractiveGeneralSettingsPanel(const Configuration& config, QWidget* parent = nullptr)
          : GeneralSettingsPanel(parent)
        {
            QFormLayout* layout = create_form_layout();
            container()->setLayout(layout);

            create_devices(layout);
            create_color_pipeline(layout);
            create_time_limit(layout);

            load_directly_linked_values(config);
            load_time_limit(config);
        }

        void save_config(Configuration& config) const override
        {
            save_directly_linked_values(config);
            save_time_limit(config);
        }

      private:
        void create_time_limit(QFormLayout* parent)
        {
            QSpinBox* hours = create_integer_input("hours", 0, 24, 1, "h");
            QSpinBox* minutes = create_integer_input("minutes", 0, 60, 1, "m");
            QSpinBox* seconds = create_integer_input("seconds", 0, 60, 5, "s");
            QCheckBox* unlimited_time = create_checkbox("unlimited_time", "Unlimited");
            parent->addRow("Time Limit:", create_horizontal_group(hours, minutes, seconds, unlimited_time));
            connect(unlimited_time, SIGNAL(toggled(bool)), hours, SLOT(setDisabled(bool)));
            connect(unlimited_time, SIGNAL(toggled(bool)), minutes, SLOT(setDisabled(bool)));
            connect(unlimited_time, SIGNAL(toggled(bool)), seconds, SLOT(setDisabled(bool)));
        }

        void load_time_limit(const Configuration& config)
        {
            constexpr int DefaultSeconds = 0;
            constexpr int DefaultMinutes = 1;
            constexpr int DefaultHours = 0;

            const int time_limit = m_params_metadata.get_path_optional<int>("progressive_frame_renderer.time_limit.default", -1);
            const int hours = time_limit == -1 ? DefaultHours : time_limit / 3600;
            const int minutes = time_limit == -1 ? DefaultMinutes : (time_limit - hours * 3600) / 60;
            const int seconds = time_limit == -1 ? DefaultSeconds : time_limit - hours * 3600 - minutes * 60;

            set_widget("unlimited_time", time_limit == -1);
            set_widget("hours", hours);
            set_widget("minutes", minutes);
            set_widget("seconds", seconds);
        }

        void save_time_limit(Configuration & config) const
        {
            if (get_widget<bool>("unlimited_time"))
                config.get_parameters().remove_path("progressive_frame_renderer.time_limit");
            else
            {
                const int hours = get_widget<int>("hours");
                const int minutes = get_widget<int>("minutes");
                const int seconds = get_widget<int>("seconds");
                const int time_limit = hours * 60 * 60 + minutes * 60 + seconds;
                set_config(config, "progressive_frame_renderer.time_limit", time_limit);
            }
        }
    };


    //
    // Image Plane Sampling panels.
    //

    class ImagePlaneSamplingPanel
      : public RenderSettingsPanel
    {
      public:
        explicit ImagePlaneSamplingPanel(QWidget* parent = nullptr)
          : RenderSettingsPanel("Image Plane Sampling", parent)
        {
        }
    };

    class FinalImagePlaneSamplingPanel
      : public ImagePlaneSamplingPanel
    {
        Q_OBJECT

      public:
        explicit FinalImagePlaneSamplingPanel(const Configuration& config, QWidget* parent = nullptr)
          : ImagePlaneSamplingPanel(parent)
        {
            QVBoxLayout* layout = new QVBoxLayout();
            container()->setLayout(layout);

            create_image_plane_sampling_general_settings(layout);
            create_image_plane_sampling_sampler_settings(layout);

            create_direct_link("general.passes",                                   "passes");

            create_direct_link("texture_controlled_sampler.min_samples",           "texture_controlled_pixel_renderer.min_samples");
            create_direct_link("texture_controlled_sampler.max_samples",           "texture_controlled_pixel_renderer.max_samples");
            create_direct_link("texture_controlled_sampler.file_path",             "texture_controlled_pixel_renderer.file_path");
            create_direct_link("texture_controlled_sampler.force_antialiasing",    "texture_controlled_pixel_renderer.force_antialiasing");

            create_direct_link("uniform_sampler.samples",                          "uniform_pixel_renderer.samples");
            create_direct_link("uniform_sampler.force_antialiasing",               "uniform_pixel_renderer.force_antialiasing");

            create_direct_link("adaptive_tile_sampler.batch_size",                 "adaptive_tile_renderer.batch_size");
            create_direct_link("adaptive_tile_sampler.min_samples",                "adaptive_tile_renderer.min_samples");
            create_direct_link("adaptive_tile_sampler.max_samples",                "adaptive_tile_renderer.max_samples");
            create_direct_link("adaptive_tile_sampler.noise_threshold",            "adaptive_tile_renderer.noise_threshold");

            load_general_sampler(config);
            load_directly_linked_values(config);
        }

        void save_config(Configuration& config) const override
        {
            save_directly_linked_values(config);

            set_config(
                config,
                "shading_result_framebuffer",
                get_widget<size_t>("general.passes") > 1 ? "permanent" : "ephemeral");

            // Set the pixel and tile renderers.
            const QString sampler =
                m_image_plane_sampler_combo->itemData(
                    m_image_plane_sampler_combo->currentIndex()).value<QString>();
            if (sampler == "adaptive_tile")
            {
                set_config(config, "tile_renderer", "adaptive");
                config.get_parameters().remove_path("pixel_renderer");
            }
            else
            {
                set_config(config, "tile_renderer", "generic");
                set_config(config, "pixel_renderer", sampler);
            }
        }

      private:
        QComboBox*  m_image_plane_sampler_combo;
        QSpinBox*   m_image_plane_sampler_passes;
        QGroupBox*  m_uniform_image_plane_sampler;
        QGroupBox*  m_adaptive_tile_image_plane_sampler;
        QGroupBox*  m_texture_controlled_image_plane_sampler;
        QSpinBox*   m_texture_controlled_sampler_min_samples;
        QSpinBox*   m_texture_controlled_sampler_max_samples;
        QCheckBox*  m_uniform_sampler_force_aa;
        QCheckBox*  m_texture_controlled_sampler_force_aa;
        QLineEdit*  m_path_line_edit;
        ParamArray  m_file_open_settings;

        void create_image_plane_sampling_general_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("General");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = new QVBoxLayout();
            groupbox->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            m_image_plane_sampler_combo = create_combobox("general.sampler");
            m_image_plane_sampler_combo->setToolTip(m_params_metadata.get_path("sampling_mode.help"));
            m_image_plane_sampler_combo->addItem("Uniform", "uniform");
            m_image_plane_sampler_combo->addItem("Adaptive", "adaptive_tile");
            m_image_plane_sampler_combo->addItem("Texture", "texture");
            m_image_plane_sampler_combo->setCurrentIndex(-1);
            sublayout->addRow("Sampler:", m_image_plane_sampler_combo);

            connect(
                m_image_plane_sampler_combo, SIGNAL(currentIndexChanged(int)),
                SLOT(slot_changed_image_plane_sampler(const int)));

            m_image_plane_sampler_passes = create_integer_input("general.passes", 1, 1000000, 1);
            m_image_plane_sampler_passes->setToolTip(m_params_metadata.get_path("passes.help"));
            sublayout->addRow("Passes:", m_image_plane_sampler_passes);
        }

        void create_image_plane_sampling_sampler_settings(QVBoxLayout* parent)
        {
            QVBoxLayout* v_layout = create_vertical_layout();
            QHBoxLayout* h_layout = create_horizontal_layout();
            v_layout->addLayout(h_layout);
            parent->addLayout(v_layout);

            create_image_plane_sampling_uniform_sampler_settings(h_layout);
            create_image_plane_sampling_adaptive_tile_sampler_settings(h_layout);
            create_image_plane_sampling_texture_controlled_sampler_settings(v_layout);
        }

        void create_image_plane_sampling_uniform_sampler_settings(QHBoxLayout* parent)
        {
            m_uniform_image_plane_sampler = new QGroupBox("Uniform Sampler");
            parent->addWidget(m_uniform_image_plane_sampler);

            QVBoxLayout* layout = create_vertical_layout();
            m_uniform_image_plane_sampler->setLayout(layout);

            QSpinBox* samples_spinbox = create_integer_input("uniform_sampler.samples", 1, 1000000, 1);
            samples_spinbox->setToolTip(m_params_metadata.get_path("uniform_pixel_renderer.samples.help"));
            layout->addLayout(create_form_layout("Samples:", samples_spinbox));

            m_uniform_sampler_force_aa = create_checkbox("uniform_sampler.force_antialiasing", "Force Antialiasing");
            m_uniform_sampler_force_aa->setToolTip(m_params_metadata.get_path("uniform_pixel_renderer.force_antialiasing.help"));
            layout->addWidget(m_uniform_sampler_force_aa);

            connect(
                samples_spinbox, SIGNAL(valueChanged(const int)),
                SLOT(slot_changed_uniform_sampler_samples(const int)));
        }

        void create_image_plane_sampling_texture_controlled_sampler_settings(QVBoxLayout* parent)
        {
            m_texture_controlled_image_plane_sampler = new QGroupBox("Texture-controlled Sampler");
            parent->addWidget(m_texture_controlled_image_plane_sampler);

            QVBoxLayout* layout = create_vertical_layout();
            m_texture_controlled_image_plane_sampler->setLayout(layout);

            m_texture_controlled_sampler_min_samples = create_integer_input("texture_controlled_sampler.min_samples", 0, 1000000, 1);
            m_texture_controlled_sampler_min_samples->setToolTip(m_params_metadata.get_path("texture_controlled_pixel_renderer.min_samples.help"));
            layout->addLayout(create_form_layout("Min Samples:", m_texture_controlled_sampler_min_samples));

            m_texture_controlled_sampler_max_samples = create_integer_input("texture_controlled_sampler.max_samples", 1, 1000000, 1);
            m_texture_controlled_sampler_max_samples->setToolTip(m_params_metadata.get_path("texture_controlled_pixel_renderer.max_samples.help"));
            layout->addLayout(create_form_layout("Max Samples:", m_texture_controlled_sampler_max_samples));

            m_texture_controlled_sampler_force_aa = create_checkbox("texture_controlled_sampler.force_antialiasing", "Force Antialiasing");
            m_texture_controlled_sampler_force_aa->setToolTip(m_params_metadata.get_path("texture_controlled_pixel_renderer.force_antialiasing.help"));
            m_texture_controlled_sampler_force_aa->setDisabled(
                m_texture_controlled_sampler_min_samples->value() > 1 ||
                m_texture_controlled_sampler_max_samples->value() == 0);
            layout->addWidget(m_texture_controlled_sampler_force_aa);

            QHBoxLayout* file_layout = create_horizontal_layout();
            QLabel* label = new QLabel("File Path:");
            m_path_line_edit = create_line_edit("texture_controlled_sampler.file_path");
            QWidget* browse_button = new QPushButton("Browse");
            browse_button->setToolTip(m_params_metadata.get_path("texture_controlled_pixel_renderer.file_path.help"));
            browse_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
            file_layout->addWidget(label);
            file_layout->addWidget(m_path_line_edit);
            file_layout->addWidget(browse_button);
            layout->addLayout(file_layout);

            connect(
                browse_button, SIGNAL(pressed()),
                SLOT(slot_browse_button_pressed()));

            connect(
                m_texture_controlled_sampler_min_samples, SIGNAL(valueChanged(const int)),
                SLOT(slot_changed_texture_sampler_min_samples(const int)));

            connect(
                m_texture_controlled_sampler_max_samples, SIGNAL(valueChanged(const int)),
                SLOT(slot_changed_texture_sampler_max_samples(const int)));
        }

        void create_image_plane_sampling_adaptive_tile_sampler_settings(QHBoxLayout* parent)
        {
            m_adaptive_tile_image_plane_sampler = new QGroupBox("Adaptive Sampler");
            parent->addWidget(m_adaptive_tile_image_plane_sampler);

            QVBoxLayout* layout = create_vertical_layout();
            m_adaptive_tile_image_plane_sampler->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            QSpinBox* batch_size = create_integer_input("adaptive_tile_sampler.batch_size", 1, 1000000, 1);
            batch_size->setToolTip(m_params_metadata.get_path("adaptive_tile_renderer.batch_size.help"));
            sublayout->addRow("Batch Size:", batch_size);

            QSpinBox* min_samples = create_integer_input("adaptive_tile_sampler.min_samples", 0, 1000000, 1);
            min_samples->setToolTip(m_params_metadata.get_path("adaptive_tile_renderer.min_samples.help"));
            sublayout->addRow("Min Samples:", min_samples);

            QSpinBox* max_samples = create_integer_input("adaptive_tile_sampler.max_samples", 0, 1000000, 1);
            max_samples->setToolTip(m_params_metadata.get_path("adaptive_tile_renderer.max_samples.help"));
            sublayout->addRow("Max Samples:", max_samples);

            QDoubleSpinBox* noise_threshold = create_double_input("adaptive_tile_sampler.noise_threshold", 0.0001, 10000.0, 4, 0.02);
            noise_threshold->setToolTip(m_params_metadata.get_path("adaptive_tile_renderer.noise_threshold.help"));
            sublayout->addRow("Noise Threshold:", noise_threshold);
        }

        void load_general_sampler(const Configuration& config)
        {
            const std::string tile_renderer = get_config<std::string>(config, "tile_renderer", "generic");
            const std::string pixel_renderer = get_config<std::string>(config, "pixel_renderer", "uniform");

            m_image_plane_sampler_combo->setCurrentIndex(
                tile_renderer == "adaptive" ? 1 :
                pixel_renderer == "texture" ? 2 : 0);
        }

      private slots:
        void slot_changed_image_plane_sampler(const int index)
        {
            const QString sampler = m_image_plane_sampler_combo->itemData(index).value<QString>();

            m_uniform_image_plane_sampler->setEnabled(sampler == "uniform");
            m_adaptive_tile_image_plane_sampler->setEnabled(sampler == "adaptive_tile");
            m_texture_controlled_image_plane_sampler->setEnabled(sampler == "texture");
        }

        void slot_changed_uniform_sampler_samples(const int samples)
        {
            m_uniform_sampler_force_aa->setDisabled(samples > 1);
        }

        void slot_browse_button_pressed()
        {
            QFileDialog::Options options;

            QString filepath =
                get_open_filename(
                    this,
                    "Open...",
                    get_oiio_image_files_filter(),
                    m_file_open_settings,
                    SETTINGS_FILE_DIALOG_OIIO_TEXTURES,
                    options);

            if (!filepath.isEmpty())
            {
                m_path_line_edit->setText(filepath);
            }
        }

        void slot_changed_texture_sampler_min_samples(const int samples)
        {
            m_texture_controlled_sampler_max_samples->setMinimum(samples);
            m_texture_controlled_sampler_force_aa->setDisabled(samples > 1 || m_texture_controlled_sampler_max_samples->value() == 0);
        }

        void slot_changed_texture_sampler_max_samples(const int samples)
        {
            m_texture_controlled_sampler_min_samples->setMaximum(samples);
        }
    };

    class InteractiveImagePlaneSamplingPanel
      : public ImagePlaneSamplingPanel
    {
        Q_OBJECT

      public:
        explicit InteractiveImagePlaneSamplingPanel(const Configuration& config, QWidget* parent = nullptr)
          : ImagePlaneSamplingPanel(parent)
        {
            QVBoxLayout* layout = new QVBoxLayout();
            container()->setLayout(layout);

            create_image_plane_sampling_general_settings(layout);

            load_general_sampler(config);
        }

        void save_config(Configuration& config) const override
        {
            if (get_widget<bool>("general.unlimited_samples"))
                config.get_parameters().remove_path("progressive_frame_renderer.max_average_spp");
            else set_config(config, "progressive_frame_renderer.max_average_spp", get_widget<int>("general.max_average_spp"));
        }

      private:
        void create_image_plane_sampling_general_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("General");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = new QVBoxLayout();
            groupbox->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            QSpinBox* max_average_spp = create_integer_input("general.max_average_spp", 1, 1000000, 10);
            QCheckBox* unlimited_samples = create_checkbox("general.unlimited_samples", "Unlimited");
            sublayout->addRow("Max Average Samples Per Pixel:", create_horizontal_group(max_average_spp, unlimited_samples));
            connect(unlimited_samples, SIGNAL(toggled(bool)), max_average_spp, SLOT(setDisabled(bool)));
        }

        void load_general_sampler(const Configuration& config)
        {
            const int DefaultMaxSamples = 64;

            const int max_average_spp = get_config<int>(config, "progressive_frame_renderer.max_average_spp", -1);

            set_widget("general.unlimited_samples", max_average_spp == -1);
            set_widget("general.max_average_spp", max_average_spp == -1 ? DefaultMaxSamples : max_average_spp);
        }
    };

    //
    // Lighting panels.
    //

    class LightingPanel
      : public RenderSettingsPanel
    {
      public:
        explicit LightingPanel(const Configuration& config, QWidget* parent = nullptr)
          : RenderSettingsPanel("Lighting", parent)
        {
        }

        void save_config(Configuration& config) const override
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
        explicit FinalConfigurationLightingPanel(const Configuration& config, QWidget* parent = nullptr)
          : LightingPanel(config, parent)
        {
            QComboBox* combobox = create_combobox("engine");
            combobox->setToolTip(m_params_metadata.get_path("lighting_engine.help"));
            combobox->addItem("Unidirectional Path Tracer", "pt");
            // combobox->addItem("Bidirectional Path Tracer", "bdpt");
            combobox->addItem("Stochastic Progressive Photon Mapping", "sppm");
            construct(config, combobox);
        }
    };

    class InteractiveConfigurationLightingPanel
      : public LightingPanel
    {
      public:
        explicit InteractiveConfigurationLightingPanel(const Configuration& config, QWidget* parent = nullptr)
          : LightingPanel(config, parent)
        {
            QComboBox* combobox = create_combobox("engine");
            combobox->setToolTip(m_params_metadata.get_path("lighting_engine.help"));
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
        explicit LightingEnginePanel(const QString& title, QWidget* parent = nullptr)
          : RenderSettingsPanel(title, parent)
        {
        }

      protected:
        void create_bounce_settings_group(QVBoxLayout* parent, const std::string& prefix, const std::string& config_param_path)
        {
            QGroupBox* groupbox = new QGroupBox("Bounces");
            parent->addWidget(groupbox);

            QFormLayout* layout = create_form_layout();
            groupbox->setLayout(layout);

            create_bounce_settings(layout, prefix, config_param_path);
        }

        void create_bounce_settings(QFormLayout* layout, const std::string& prefix, const std::string& config_param_path)
        {
            const std::string widget_base_key = prefix + ".bounces.";

            QSpinBox* max_bounces = create_integer_input(widget_base_key + "max_bounces", 0, 100, 1);
            max_bounces->setToolTip(m_params_metadata.get_path((config_param_path + ".help").c_str()));

            QCheckBox* unlimited_bounces = create_checkbox(widget_base_key + "unlimited_bounces", "Unlimited");

            layout->addRow("Max Bounces:", create_horizontal_group(max_bounces, unlimited_bounces));
            connect(unlimited_bounces, SIGNAL(toggled(bool)), max_bounces, SLOT(setDisabled(bool)));

            QSpinBox* russian_roulette_start = create_integer_input(widget_base_key + "rr_start_bounce", 1, 100, 1);
            russian_roulette_start->setToolTip(m_params_metadata.get_path("pt.rr_min_path_length.help"));
            layout->addRow("Russian Roulette Start Bounce:", russian_roulette_start);
        }

        void create_separate_bounce_settings_group(QVBoxLayout* parent, const std::string& prefix, const std::string& config_param_path)
        {
            QGroupBox* groupbox = new QGroupBox("Bounces");
            parent->addWidget(groupbox);

            QFormLayout* layout = create_form_layout();
            groupbox->setLayout(layout);

            create_separate_bounce_settings(layout, prefix, config_param_path);
        }

        void create_separate_bounce_settings(QFormLayout* layout, const std::string& prefix, const std::string& config_param_path)
        {
            const std::string widget_base_key = prefix + ".bounces.";

            QSpinBox* max_bounces = create_integer_input(widget_base_key + "max_bounces", 0, 100, 1);
            QSpinBox* max_diffuse_bounces = create_integer_input(widget_base_key + "max_diffuse_bounces", 0, 100, 1);
            QSpinBox* max_glossy_bounces = create_integer_input(widget_base_key + "max_glossy_bounces", 0, 100, 1);
            QSpinBox* max_specular_bounces = create_integer_input(widget_base_key + "max_specular_bounces", 0, 100, 1);
            QSpinBox* max_volume_bounces = create_integer_input(widget_base_key + "max_volume_bounces", 0, 100, 1);
            max_bounces->setToolTip(m_params_metadata.get_path((config_param_path + ".help").c_str()));

            QCheckBox* unlimited_bounces = create_checkbox(widget_base_key + "unlimited_bounces", "Unlimited");
            QCheckBox* unlimited_diffuse_bounces = create_checkbox(widget_base_key + "unlimited_diffuse_bounces", "Unlimited");
            QCheckBox* unlimited_glossy_bounces = create_checkbox(widget_base_key + "unlimited_glossy_bounces", "Unlimited");
            QCheckBox* unlimited_specular_bounces = create_checkbox(widget_base_key + "unlimited_specular_bounces", "Unlimited");

            layout->addRow("Max Global Bounces:", create_horizontal_group(max_bounces, unlimited_bounces));
            layout->addRow("Max Diffuse Bounces:", create_horizontal_group(max_diffuse_bounces, unlimited_diffuse_bounces));
            layout->addRow("Max Glossy Bounces:", create_horizontal_group(max_glossy_bounces, unlimited_glossy_bounces));
            layout->addRow("Max Specular Bounces:", create_horizontal_group(max_specular_bounces, unlimited_specular_bounces));
            layout->addRow("Max Volume Bounces:", create_horizontal_group(max_volume_bounces));
            connect(unlimited_bounces, SIGNAL(toggled(bool)), max_bounces, SLOT(setDisabled(bool)));
            connect(unlimited_diffuse_bounces, SIGNAL(toggled(bool)), max_diffuse_bounces, SLOT(setDisabled(bool)));
            connect(unlimited_glossy_bounces, SIGNAL(toggled(bool)), max_glossy_bounces, SLOT(setDisabled(bool)));
            connect(unlimited_specular_bounces, SIGNAL(toggled(bool)), max_specular_bounces, SLOT(setDisabled(bool)));

            QSpinBox* russian_roulette_start = create_integer_input(widget_base_key + "rr_start_bounce", 1, 100, 1);
            russian_roulette_start->setToolTip(m_params_metadata.get_path("pt.rr_min_path_length.help"));
            layout->addRow("Russian Roulette Start Bounce:", russian_roulette_start);
        }

        void load_global_max_bounce_settings(
            const Configuration&    config,
            const std::string&      widget_key_prefix,
            const std::string&      param_path,
            const int               default_max_bounces)
        {
            const int DefaultMaxBounces = 8;

            const int max_bounces = get_config<int>(config, param_path, default_max_bounces);

            set_widget(widget_key_prefix + ".bounces.unlimited_bounces", max_bounces == -1);
            set_widget(widget_key_prefix + ".bounces.max_bounces", max_bounces == -1 ? DefaultMaxBounces : max_bounces);
        }

        void save_bounce_settings(
            Configuration&          config,
            const std::string&      widget_key_prefix,
            const std::string&      param_path) const
        {
            const int max_bounces =
                !get_widget<bool>(widget_key_prefix + ".bounces.unlimited_bounces")
                    ? get_widget<int>(widget_key_prefix + ".bounces.max_bounces")
                    : -1;

            set_config(config, param_path, max_bounces);
        }

        void load_separate_bounce_settings(
            const Configuration&    config,
            const std::string&      widget_key_prefix,
            const std::string&      bounce_type,
            const int               default_max_bounces,
            const bool              allow_unlimited = true)
        {
            const int DefaultMaxBounces = 8;
            const int DefaultMaxDiffuseBounces = 3;

            const int max_bounces =
                get_config<int>(config, construct_bounce_param_path(bounce_type), default_max_bounces);

            const std::string widget_max_bounce_key = widget_key_prefix + ".bounces.max_" + bounce_type + "_bounces";

            if (allow_unlimited)
                set_widget(widget_key_prefix + ".bounces.unlimited_" + bounce_type + "_bounces", max_bounces == -1);
            if (bounce_type == "diffuse")
                set_widget(widget_max_bounce_key, max_bounces == -1 ? DefaultMaxDiffuseBounces : max_bounces);
            else
                set_widget(widget_max_bounce_key, max_bounces == -1 ? DefaultMaxBounces : max_bounces);
        }

        void save_separate_bounce_settings(
            Configuration&          config,
            const std::string&      widget_key_prefix,
            const std::string&      bounce_type,
            const bool              allow_unlimited = true) const
        {
            const bool unlimited_bounces =
                allow_unlimited &&
                get_widget<bool>(widget_key_prefix + ".bounces.unlimited_" + bounce_type + "_bounces");

            const int max_bounces =
                !unlimited_bounces
                    ? get_widget<int>(widget_key_prefix + ".bounces.max_" + bounce_type + "_bounces")
                    : -1;

            set_config(config, construct_bounce_param_path(bounce_type), max_bounces);
        }

        static std::string construct_bounce_param_path(const std::string& bounce_type)
        {
            return "pt.max_" + bounce_type + "_bounces";
        }
    };

    //
    // Unidirectional Path Tracer panel.
    //

    class UnidirectionalPathTracerPanel
      : public LightingEnginePanel
    {
      public:
        explicit UnidirectionalPathTracerPanel(const Configuration& config, QWidget* parent = nullptr)
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

            create_separate_bounce_settings_group(layout, "pt", "pt.max_bounces");
            create_pt_volume_settings(layout);
            create_pt_advanced_settings(layout);

            create_direct_link("lighting_components.dl",                            "pt.enable_dl");
            create_direct_link("lighting_components.ibl",                           "pt.enable_ibl");
            create_direct_link("lighting_components.caustics",                      "pt.enable_caustics");

            create_direct_link("pt.bounces.rr_start_bounce",                        "pt.rr_min_path_length");

            create_direct_link("volume.distance_samples",                           "pt.volume_distance_samples");
            create_direct_link("volume.optimize_for_lights_outside_volumes",        "pt.optimize_for_lights_outside_volumes");

            create_direct_link("advanced.next_event_estimation",                    "pt.next_event_estimation");

            create_direct_link("advanced.dl.light_samples",                         "pt.dl_light_samples");
            create_direct_link("advanced.dl.low_light_threshold",                   "pt.dl_low_light_threshold");

            create_direct_link("advanced.ibl.env_samples",                          "pt.ibl_env_samples");

            create_direct_link("advanced.light_sampler.algorithm",                  "light_sampler.algorithm");
            create_direct_link("advanced.light_sampler.enable_importance_sampling", "light_sampler.enable_importance_sampling");

            create_direct_link("advanced.record_light_paths",                       "pt.record_light_paths");

            create_direct_link("advanced.clamp_roughness",                          "pt.clamp_roughness");

            load_directly_linked_values(config);

            load_global_max_bounce_settings(config, "pt", "pt.max_bounces", 8);
            load_separate_bounce_settings(config, "pt", "diffuse", 3);
            load_separate_bounce_settings(config, "pt", "glossy", 8);
            load_separate_bounce_settings(config, "pt", "specular", 8);
            load_separate_bounce_settings(config, "pt", "volume", 8, false);

            set_widget("advanced.unlimited_ray_intensity", !config.get_parameters().exist_path("pt.max_ray_intensity"));
            set_widget("advanced.max_ray_intensity", get_config<double>(config, "pt.max_ray_intensity", 1.0));
        }

        void save_config(Configuration& config) const override
        {
            save_directly_linked_values(config);

            save_bounce_settings(config, "pt", "pt.max_bounces");
            save_separate_bounce_settings(config, "pt", "diffuse");
            save_separate_bounce_settings(config, "pt", "glossy");
            save_separate_bounce_settings(config, "pt", "specular");
            save_separate_bounce_settings(config, "pt", "volume", false);

            if (get_widget<bool>("advanced.unlimited_ray_intensity"))
                config.get_parameters().remove_path("pt.max_ray_intensity");
            else set_config(config, "pt.max_ray_intensity", get_widget<double>("advanced.max_ray_intensity"));
        }

      private:
        void create_pt_volume_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Participating Media");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = create_vertical_layout();
            groupbox->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            QSpinBox* volume_distance_samples =
                create_integer_input("volume.distance_samples", 1, 1000, 1);
            volume_distance_samples->setToolTip(
                m_params_metadata.get_path("pt.volume_distance_samples.help"));
            sublayout->addRow("Volume Distance Samples:", volume_distance_samples);

            sublayout->addRow(
                create_checkbox("volume.optimize_for_lights_outside_volumes", "Optimize for Lights Outside Volumes"));
        }

        void create_pt_advanced_settings(QVBoxLayout* parent)
        {
            CollapsibleSectionWidget* collapsible_section = new CollapsibleSectionWidget("Advanced");
            parent->addWidget(collapsible_section);

            QVBoxLayout* layout = new QVBoxLayout();

            create_pt_advanced_nee_settings(layout);
            create_pt_advanced_optimization_settings(layout);
            create_pt_advanced_diag_settings(layout);
            collapsible_section->set_content_layout(layout);
        }

        void create_pt_advanced_nee_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = create_checkable_groupbox("advanced.next_event_estimation", "Next Event Estimation");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = create_vertical_layout();
            groupbox->setLayout(layout);

            create_pt_advanced_nee_lightsampler_settings(layout);
            create_pt_advanced_nee_dl_settings(layout);
            create_pt_advanced_nee_ibl_settings(layout);
            create_pt_advanced_nee_max_ray_intensity_settings(layout);
        }

        void create_pt_advanced_nee_lightsampler_settings(QVBoxLayout* parent)
        {
            QFormLayout* sublayout = create_form_layout();
            parent->addLayout(sublayout);

            QComboBox* light_sampler = create_combobox("advanced.light_sampler.algorithm");
            light_sampler->setToolTip(m_params_metadata.get_path("light_sampler.algorithm.help"));
            light_sampler->addItem("CDF", "cdf");
            light_sampler->addItem("Light Tree", "lighttree");
            sublayout->addRow("Light Sampler:", light_sampler);

            sublayout->addRow(create_checkbox("advanced.light_sampler.enable_importance_sampling", "Enable Importance Sampling"));
        }

        void create_pt_advanced_nee_dl_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Direct Lighting");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = create_vertical_layout();
            groupbox->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            QDoubleSpinBox* light_samples = create_double_input("advanced.dl.light_samples", 0.0, 1000000.0, 3, 1.0);
            light_samples->setToolTip(m_params_metadata.get_path("pt.dl_light_samples.help"));
            sublayout->addRow("Light Samples:", light_samples);

            QDoubleSpinBox* low_light_threshold = create_double_input("advanced.dl.low_light_threshold", 0.0, 1000.0, 3, 0.1);
            low_light_threshold->setToolTip(m_params_metadata.get_path("pt.dl_low_light_threshold.help"));
            sublayout->addRow("Low Light Threshold:", low_light_threshold);
        }

        void create_pt_advanced_nee_ibl_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Image-Based Lighting");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = create_vertical_layout();
            groupbox->setLayout(layout);

            QHBoxLayout* sublayout = create_horizontal_layout();
            layout->addLayout(sublayout);

            QDoubleSpinBox* env_samples = create_double_input("advanced.ibl.env_samples", 0.0, 1000000.0, 3, 1.0);
            env_samples->setToolTip(m_params_metadata.get_path("pt.ibl_env_samples.help"));
            sublayout->addLayout(create_form_layout("Environment Samples:", env_samples));
        }

        void create_pt_advanced_nee_max_ray_intensity_settings(QVBoxLayout* parent)
        {
            QDoubleSpinBox* max_ray_intensity = create_double_input("advanced.max_ray_intensity", 0.0, 1.0e4, 1, 0.1);
            max_ray_intensity->setToolTip(m_params_metadata.get_path("pt.max_ray_intensity.help"));

            QCheckBox* unlimited_ray_intensity = create_checkbox("advanced.unlimited_ray_intensity", "Unlimited");
            parent->addLayout(create_form_layout("Max Ray Intensity:", create_horizontal_group(max_ray_intensity, unlimited_ray_intensity)));
            connect(unlimited_ray_intensity, SIGNAL(toggled(bool)), max_ray_intensity, SLOT(setDisabled(bool)));
        }

        void create_pt_advanced_optimization_settings(QVBoxLayout* parent)
        {
            QGroupBox* diag_groupbox = new QGroupBox("Optimizations");
            parent->addWidget(diag_groupbox);

            QVBoxLayout* layout = create_vertical_layout();
            diag_groupbox->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            sublayout->addRow(create_checkbox("advanced.clamp_roughness", "Clamp Roughness"));
        }

        void create_pt_advanced_diag_settings(QVBoxLayout* parent)
        {
            QGroupBox* diag_groupbox = new QGroupBox("Diagnostics");
            parent->addWidget(diag_groupbox);

            QVBoxLayout* layout = create_vertical_layout();
            diag_groupbox->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            sublayout->addRow(create_checkbox("advanced.record_light_paths", "Record Light Paths"));
        }
    };

    //
    // Stochastic Progressive Photon Mapping panel.
    //

    class SPPMPanel
      : public LightingEnginePanel
    {
      public:
        explicit SPPMPanel(const Configuration& config, QWidget* parent = nullptr)
          : LightingEnginePanel("Stochastic Progressive Photon Mapping", parent)
        {
            fold();

            QVBoxLayout* layout = new QVBoxLayout();
            container()->setLayout(layout);

            create_photon_type_settings(layout);
            create_components_settings(layout);
            create_importon_tracing_settings(layout);
            create_photon_tracing_settings(layout);
            create_radiance_estimation_settings(layout);
            create_advanced_settings(layout);

            create_direct_link("lighting_components.ibl",                          "sppm.enable_ibl");
            create_direct_link("lighting_components.caustics",                     "sppm.enable_caustics");
            create_direct_link("photon_tracing.importons",                         "sppm.enable_importons");
            create_direct_link("photon_tracing.importon_lookup_radius",            "sppm.importon_lookup_radius");
            create_direct_link("photon_tracing.bounces.rr_start_bounce",           "sppm.photon_tracing_rr_min_path_length");
            create_direct_link("photon_tracing.light_photons",                     "sppm.light_photons_per_pass");
            create_direct_link("photon_tracing.env_photons",                       "sppm.env_photons_per_pass");
            create_direct_link("radiance_estimation.bounces.rr_start_bounce",      "sppm.path_tracing_rr_min_path_length");
            create_direct_link("radiance_estimation.initial_photon_lookup_radius", "sppm.initial_photon_lookup_radius");
            create_direct_link("radiance_estimation.max_photons",                  "sppm.max_photons_per_estimate");
            create_direct_link("radiance_estimation.alpha",                        "sppm.alpha");

            load_directly_linked_values(config);

            load_global_max_bounce_settings(config, "photon_tracing", "sppm.photon_tracing_max_bounces", -1);
            load_global_max_bounce_settings(config, "radiance_estimation", "sppm.path_tracing_max_bounces", -1);

            const std::string dl_mode = get_config<std::string>(config, "sppm.dl_mode", "rt");
            if (dl_mode == "rt")
                set_widget("lighting_components.dl.rt", true);
            else if (dl_mode == "sppm")
                set_widget("lighting_components.dl.sppm", true);
            else set_widget("lighting_components.dl.off", true);

            const std::string photon_type = get_config<std::string>(config, "sppm.photon_type", "poly");
            if (photon_type == "mono")
                set_widget("photon_type.mono", true);
            else set_widget("photon_type.poly", true);

            set_widget("advanced.unlimited_ray_intensity", !config.get_parameters().exist_path("sppm.path_tracing_max_ray_intensity"));
            set_widget("advanced.max_ray_intensity", get_config<double>(config, "sppm.path_tracing_max_ray_intensity", 1.0));
        }

        void save_config(Configuration& config) const override
        {
            save_directly_linked_values(config);

            save_bounce_settings(config, "photon_tracing", "sppm.photon_tracing_max_bounces");
            save_bounce_settings(config, "radiance_estimation", "sppm.path_tracing_max_bounces");

            set_config(config, "sppm.dl_mode",
                get_widget<bool>("lighting_components.dl.rt") ? "rt" :
                get_widget<bool>("lighting_components.dl.sppm") ? "sppm" : "off");

            set_config(config, "sppm.photon_type",
                get_widget<bool>("photon_type.mono") ? "mono" : "poly");

            if (get_widget<bool>("advanced.unlimited_ray_intensity"))
                config.get_parameters().remove_path("sppm.path_tracing_max_ray_intensity");
            else set_config(config, "sppm.path_tracing_max_ray_intensity", get_widget<double>("advanced.max_ray_intensity"));
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

            QRadioButton* button_rt = create_radio_button("lighting_components.dl.rt", "RT Direct Lighting");
            button_rt->setToolTip(m_params_metadata.get_path("sppm.dl_type.options.rt.help"));
            buttons->addButton(button_rt);

            QRadioButton* button_sppm = create_radio_button("lighting_components.dl.sppm", "SPPM Direct Lighting");
            button_sppm->setToolTip(m_params_metadata.get_path("sppm.dl_type.options.sppm.help"));
            buttons->addButton(button_sppm);

            QRadioButton* button_off = create_radio_button("lighting_components.dl.off", "No Direct Lighting");
            button_off->setToolTip(m_params_metadata.get_path("sppm.dl_type.options.off.help"));
            buttons->addButton(button_off);

            QHBoxLayout* dl_buttons_layout = new QHBoxLayout();
            dl_buttons_layout->addWidget(button_rt);
            dl_buttons_layout->addWidget(button_sppm);
            dl_buttons_layout->addWidget(button_off);
            layout->addLayout(dl_buttons_layout);

            layout->addWidget(create_checkbox("lighting_components.ibl", "Image-Based Lighting"));
            layout->addWidget(create_checkbox("lighting_components.caustics", "Caustics"));
        }

        void create_importon_tracing_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Importon Tracing");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = new QVBoxLayout();
            groupbox->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            QCheckBox* enable_importons = create_checkbox("photon_tracing.importons", "Enable Importons");
            enable_importons->setToolTip(m_params_metadata.get_path("sppm.enable_importons.help"));
            sublayout->addWidget(enable_importons);

            QDoubleSpinBox* importon_lookup_radius = create_double_input("photon_tracing.importon_lookup_radius", 0.001, 100.0, 3, 0.1, "%");
            importon_lookup_radius->setToolTip(m_params_metadata.get_path("sppm.importon_lookup_radius.help"));
            sublayout->addRow("Importon Lookup Radius:", importon_lookup_radius);
        }

        void create_photon_tracing_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Photon Tracing");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = new QVBoxLayout();
            groupbox->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            create_bounce_settings(sublayout, "photon_tracing", "sppm.photon_tracing_max_bounces");

            QSpinBox* light_photons = create_integer_input("photon_tracing.light_photons", 0, 1000000000, 100000);
            light_photons->setToolTip(m_params_metadata.get_path("sppm.light_photons_per_pass.help"));
            sublayout->addRow("Light Photons:", light_photons);

            QSpinBox* environment_photons = create_integer_input("photon_tracing.env_photons", 0, 1000000000, 100000);
            environment_photons->setToolTip(m_params_metadata.get_path("sppm.env_photons_per_pass.help"));
            sublayout->addRow("Environment Photons:", environment_photons);
        }

        void create_radiance_estimation_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = new QGroupBox("Radiance Estimation");
            parent->addWidget(groupbox);

            QVBoxLayout* layout = new QVBoxLayout();
            groupbox->setLayout(layout);

            QFormLayout* sublayout = create_form_layout();
            layout->addLayout(sublayout);

            create_bounce_settings(sublayout, "radiance_estimation", "sppm.path_tracing_max_bounces");

            QDoubleSpinBox* initial_photon_lookup_radius = create_double_input("radiance_estimation.initial_photon_lookup_radius", 0.001, 100.0, 3, 0.1, "%");
            initial_photon_lookup_radius->setToolTip(m_params_metadata.get_path("sppm.initial_photon_lookup_radius.help"));
            sublayout->addRow("Initial Photon Lookup Radius:", initial_photon_lookup_radius);

            QSpinBox* max_photons = create_integer_input("radiance_estimation.max_photons", 8, 10000, 50);
            max_photons->setToolTip(m_params_metadata.get_path("sppm.max_photons_per_estimate.help"));
            sublayout->addRow("Max Photons:", max_photons);

            QDoubleSpinBox* alpha = create_double_input("radiance_estimation.alpha", 0.0, 1.0, 2, 0.1);
            alpha->setToolTip(m_params_metadata.get_path("sppm.alpha.help"));
            sublayout->addRow("Alpha:", alpha);
        }

        void create_advanced_settings(QVBoxLayout* parent)
        {
            CollapsibleSectionWidget* collapsible_section = new CollapsibleSectionWidget("Advanced");
            parent->addWidget(collapsible_section);

            QVBoxLayout* layout = create_vertical_layout();

            create_advanced_max_ray_intensity_settings(layout);
            collapsible_section->set_content_layout(layout);
        }

        void create_advanced_max_ray_intensity_settings(QVBoxLayout* parent)
        {
            QDoubleSpinBox* max_ray_intensity = create_double_input("advanced.max_ray_intensity", 0.0, 1.0e4, 1, 0.1);
            max_ray_intensity->setToolTip(m_params_metadata.get_path("sppm.path_tracing_max_ray_intensity.help"));

            QCheckBox* unlimited_ray_intensity = create_checkbox("advanced.unlimited_ray_intensity", "Unlimited");
            connect(unlimited_ray_intensity, SIGNAL(toggled(bool)), max_ray_intensity, SLOT(setDisabled(bool)));

            parent->addLayout(create_form_layout("Max Ray Intensity:", create_horizontal_group(max_ray_intensity, unlimited_ray_intensity)));
        }
    };

    //
    // System panel.
    //

    class SystemPanel
      : public LightingEnginePanel
    {
        Q_OBJECT

      public:
        SystemPanel(const Configuration& config, const ParamArray& application_settings, QWidget* parent = nullptr)
          : LightingEnginePanel("System", parent)
          , m_application_settings(application_settings)
        {
            fold();

            QVBoxLayout* layout = new QVBoxLayout();
            container()->setLayout(layout);

#ifdef APPLESEED_WITH_EMBREE
            create_system_use_embree_settings(layout);
#endif

            create_system_override_rendering_threads_settings(layout);
            create_system_override_texture_store_max_size_settings(layout);
            create_system_override_tile_ordering_settings(layout);

            load_directly_linked_values(config);

            const bool override_rendering_threads = config.get_parameters().strings().exist("rendering_threads");
            set_widget("rendering_threads.override", override_rendering_threads);

            const std::string rendering_threads =
                override_rendering_threads
                    ? get_config<std::string>(config, "rendering_threads", "auto")
                    : application_settings.get_optional<std::string>("rendering_threads", "auto");
            set_widget("rendering_threads.auto", rendering_threads == "auto");
            set_widget("rendering_threads.value", rendering_threads == "auto" ? to_string(System::get_logical_cpu_core_count()) : rendering_threads);

            const size_t MB = 1024 * 1024;
            const size_t DefaultTextureStoreSizeMB = 1024 * MB;
            set_widget("texture_store_max_size.override", config.get_inherited_parameters().exist_path("texture_store.max_size"));
            set_widget("texture_store_max_size.value", get_config<size_t>(config, "texture_store.max_size", DefaultTextureStoreSizeMB) / MB);

            set_widget("tile_ordering.override", config.get_parameters().exist_path("generic_frame_renderer.tile_ordering"));
            set_widget("tile_ordering.value", get_config<std::string>(config, "generic_frame_renderer.tile_ordering", "spiral"));
        }

        void save_config(Configuration& config) const override
        {
            save_directly_linked_values(config);

            if (get_widget<bool>("rendering_threads.override"))
            {
                set_config(config, "rendering_threads",
                    get_widget<bool>("rendering_threads.auto") ? "auto" : get_widget<std::string>("rendering_threads.value"));
            }
            else config.get_parameters().strings().remove("rendering_threads");

            if (get_widget<bool>("texture_store_max_size.override"))
                set_config(config, "texture_store.max_size", get_widget<size_t>("texture_store_max_size.value") * 1024 * 1024);
            else config.get_parameters().remove_path("texture_store.max_size");

            if (get_widget<bool>("tile_ordering.override"))
                set_config(config, "generic_frame_renderer.tile_ordering", get_widget<std::string>("tile_ordering.value"));
            else config.get_parameters().remove_path("generic_frame_renderer.tile_ordering");
        }

      public slots:
        void slot_reload_application_settings()
        {
            if (!get_widget<bool>("rendering_threads.override"))
            {
                const std::string rendering_threads = m_application_settings.get_optional<std::string>("rendering_threads", "auto");
                set_widget("rendering_threads.auto", rendering_threads == "auto");
                set_widget("rendering_threads.value", rendering_threads == "auto" ? to_string(System::get_logical_cpu_core_count()) : rendering_threads);
            }
        }

      private:
        const ParamArray& m_application_settings;

#ifdef APPLESEED_WITH_EMBREE
        void create_system_use_embree_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = create_checkable_groupbox("use_embree", "Use Embree");
            parent->addWidget(groupbox);
            create_direct_link("use_embree", "use_embree", "false");
        }
#endif

        void create_system_override_rendering_threads_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = create_checkable_groupbox("rendering_threads.override", "Override");
            parent->addWidget(groupbox);

            QSpinBox* rendering_threads = create_integer_input("rendering_threads.value", -65535, 65536, 1);
            rendering_threads->setToolTip(m_params_metadata.get_path("rendering_threads.help"));

            QCheckBox* auto_rendering_threads = create_checkbox("rendering_threads.auto", "Auto");
            groupbox->setLayout(create_form_layout("Rendering Threads:", create_horizontal_group(rendering_threads, auto_rendering_threads)));

            connect(auto_rendering_threads, SIGNAL(toggled(bool)), rendering_threads, SLOT(setDisabled(bool)));
        }

        void create_system_override_texture_store_max_size_settings(QVBoxLayout* parent)
        {
            QGroupBox* groupbox = create_checkable_groupbox("texture_store_max_size.override", "Override");
            groupbox->setToolTip(m_params_metadata.get_path("texture_store.max_size.help"));
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
            tile_ordering->setToolTip(m_params_metadata.get_path("generic_frame_renderer.tile_ordering.help"));
            tile_ordering->addItem("Linear", "linear");
            tile_ordering->addItem("Spiral", "spiral");
            tile_ordering->addItem("Hilbert", "hilbert");
            tile_ordering->addItem("Random", "random");
            groupbox->setLayout(create_form_layout("Tile Ordering:", tile_ordering));
        }
    };
}


//
// RenderingSettingsWindow class implementation.
//

RenderingSettingsWindow::RenderingSettingsWindow(ProjectManager& project_manager, const ParamArray& application_settings, QWidget* parent)
  : WindowBase(parent, "rendering_settings_window")
  , m_ui(new Ui::RenderingSettingsWindow())
  , m_project_manager(project_manager)
  , m_application_settings(application_settings)
{
    m_ui->setupUi(this);

    setWindowFlags(Qt::Window);

    m_ui->scrollarea->setProperty("hasFrame", true);
    m_ui->scrollareawidget->hide();

    create_connections();

    WindowBase::load_settings();

    reload();
}

RenderingSettingsWindow::~RenderingSettingsWindow()
{
    delete m_ui;
}

void RenderingSettingsWindow::reload()
{
    assert(m_project_manager.get_project() != nullptr);

    // Collect configuration names.
    std::vector<QString> config_names;
    for (const Configuration& config : m_project_manager.get_project()->configurations())
    {
        if (!BaseConfigurationFactory::is_base_configuration(config.get_name()))
            config_names.emplace_back(config.get_name());
    }

    // Sort configuration names alphabetically.
    std::sort(std::begin(config_names), std::end(config_names));

    // This has the side effect of loading an empty configuration.
    m_current_configuration_name.clear();

    // This has the side effect of loading the first configuration.
    m_ui->combobox_configurations->clear();
    for (const QString& config_name : config_names)
        m_ui->combobox_configurations->addItem(config_name);
}

void RenderingSettingsWindow::slot_reload_application_settings()
{
    emit signal_application_settings_modified();
}

void RenderingSettingsWindow::create_connections()
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

        if (base_config == nullptr)
            return false;

        return is_interactive_configuration(*base_config);
    }
}

void RenderingSettingsWindow::create_panels(const Configuration& config)
{
    const bool interactive = is_interactive_configuration(config);

    m_panels.clear();

    if (interactive)
    {
        m_panels.push_back(new InteractiveGeneralSettingsPanel(config));
        m_panels.push_back(new InteractiveImagePlaneSamplingPanel(config));
        m_panels.push_back(new InteractiveConfigurationLightingPanel(config));
    }
    else
    {
        m_panels.push_back(new FinalGeneralSettingsPanel(config));
        m_panels.push_back(new FinalImagePlaneSamplingPanel(config));
        m_panels.push_back(new FinalConfigurationLightingPanel(config));
    }

    m_panels.push_back(new UnidirectionalPathTracerPanel(config));

    if (!interactive)
        m_panels.push_back(new SPPMPanel(config));

    SystemPanel* system_panel = new SystemPanel(config, m_application_settings);
    connect(
        this, SIGNAL(signal_application_settings_modified()),
        system_panel, SLOT(slot_reload_application_settings()));
    m_panels.push_back(system_panel);
}

void RenderingSettingsWindow::create_layout()
{
    QLayout* root_layout = m_ui->scrollareawidget->layout();
    assert(root_layout);

    clear_layout(root_layout);

    root_layout->addItem(new QSpacerItem(470, 0, QSizePolicy::Expanding, QSizePolicy::Minimum));

    for (RenderSettingsPanel* panel : m_panels)
        root_layout->addWidget(panel);
}

void RenderingSettingsWindow::set_panels_enabled(const bool enabled)
{
    for (RenderSettingsPanel* panel : m_panels)
        panel->container()->setEnabled(enabled);
}

void RenderingSettingsWindow::load_configuration(const QString& name)
{
    assert(!name.isEmpty());

    const Configuration& config = get_configuration(name);

    create_panels(config);
    create_layout();

    set_panels_enabled(
        !BaseConfigurationFactory::is_base_configuration(
            name.toUtf8().constData()));

    m_current_configuration_name = name;
    m_initial_values = get_widget_values();
}

void RenderingSettingsWindow::save_current_configuration()
{
    if (m_current_configuration_name.isEmpty())
        return;

    if (BaseConfigurationFactory::is_base_configuration(m_current_configuration_name.toUtf8().constData()))
        return;

    Configuration& config = get_configuration(m_current_configuration_name);

    for (const RenderSettingsPanel* panel : m_panels)
        panel->save_config(config);

    m_initial_values = get_widget_values();

    emit signal_rendering_settings_modified();
}

Configuration& RenderingSettingsWindow::get_configuration(const QString& name) const
{
    Configuration* configuration =
        m_project_manager.get_project()->configurations().get_by_name(name.toUtf8().constData());

    assert(configuration);

    return *configuration;
}

std::map<std::string, std::string> RenderingSettingsWindow::get_widget_values() const
{
    std::map<std::string, std::string> values;

    for (const RenderSettingsPanel* panel : m_panels)
    {
        const std::map<std::string, std::string> panel_values = panel->get_widget_values();
        values.insert(std::begin(panel_values), std::end(panel_values));
    }

    return values;
}

void RenderingSettingsWindow::slot_open_configuration_manager_window()
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
        msgbox.setText("This configuration has been modified.\n\nDo you want to save your changes?");
        msgbox.setStandardButtons(QMessageBox::Save | QMessageBox::Discard);
        msgbox.setDefaultButton(QMessageBox::Save);
        return msgbox.exec();
    }
}

void RenderingSettingsWindow::slot_change_active_configuration(const QString& configuration_name)
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

void RenderingSettingsWindow::slot_save_configuration_and_close()
{
    save_current_configuration();
    close();
}

void RenderingSettingsWindow::slot_restore_configuration_and_close()
{
    load_configuration(m_current_configuration_name);
    close();
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/moc_cpp_renderingsettingswindow.cxx"
