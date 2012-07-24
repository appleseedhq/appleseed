
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_RENDERSETTINGSWINDOW_H
#define APPLESEED_STUDIO_MAINWINDOW_RENDERSETTINGSWINDOW_H

// Qt headers.
#include <QObject>
#include <QString>
#include <QWidget>

// Standard headers.
#include <map>
#include <string>
#include <vector>

// Forward declarations.
namespace appleseed { namespace studio { class FoldablePanelWidget; } }
namespace appleseed { namespace studio { class IInputWidgetProxy; } }
namespace appleseed { namespace studio { class ProjectManager; } }
namespace renderer  { class Configuration; }
namespace Ui        { class RenderSettingsWindow; }
class QCheckBox;
class QComboBox;
class QDoubleSpinBox;
class QFormLayout;
class QHBoxLayout;
class QGroupBox;
class QLayout;
class QSpinBox;
class QVBoxLayout;

namespace appleseed {
namespace studio {

//
// Render settings window.
//

class RenderSettingsWindow
  : public QWidget
{
    Q_OBJECT

  public:
    // Constructor.
    RenderSettingsWindow(
        ProjectManager&                 project_manager,
        QWidget*                        parent = 0);

    // Destructor.
    ~RenderSettingsWindow();

    // Load the configurations of the currently open project.
    void reload();

  signals:
    void signal_settings_modified() const;

  private:
    typedef std::vector<FoldablePanelWidget*> PanelCollection;
    typedef std::map<std::string, IInputWidgetProxy*> WidgetProxyCollection;

    struct DirectLink
    {
        std::string     m_widget_key;
        std::string     m_param_path;
        std::string     m_default_value;
    };

    typedef std::vector<DirectLink> DirectLinkCollection;

    // Not wrapped in std::auto_ptr<> to avoid pulling in the UI definition code.
    Ui::RenderSettingsWindow*   m_ui;

    ProjectManager&             m_project_manager;

    PanelCollection             m_panels;
    WidgetProxyCollection       m_widget_proxies;
    DirectLinkCollection        m_direct_links;

    QString                     m_current_configuration_name;

    QComboBox*                  m_image_planer_sampler_combo;
    QGroupBox*                  m_uniform_image_plane_sampler;
    QGroupBox*                  m_adaptive_image_plane_sampler;

    void create_panels();
    void set_panels_enabled(const bool enabled);

    void create_image_plane_sampling_panel(QLayout* parent);
    void create_image_plane_sampling_general_settings(QVBoxLayout* parent);
    void create_image_plane_sampling_sampler_settings(QVBoxLayout* parent);
    void create_image_plane_sampling_uniform_sampler_settings(QHBoxLayout* parent);
    void create_image_plane_sampling_adaptive_sampler_settings(QHBoxLayout* parent);

    void create_lighting_panel(QLayout* parent);

    void create_drt_panel(QLayout* parent);
    void create_drt_advanced_settings(QVBoxLayout* parent);
    void create_drt_advanced_dl_settings(QVBoxLayout* parent);
    void create_drt_advanced_ibl_settings(QVBoxLayout* parent);

    void create_pt_panel(QLayout* parent);
    void create_pt_advanced_settings(QVBoxLayout* parent);
    void create_pt_advanced_dl_settings(QVBoxLayout* parent);
    void create_pt_advanced_ibl_settings(QVBoxLayout* parent);

    void create_system_panel(QLayout* parent);
    void create_system_override_rendering_threads_settings(QVBoxLayout* parent);
    void create_system_override_texture_cache_size_settings(QVBoxLayout* parent);

    void create_bounce_settings(QVBoxLayout* parent, const std::string& lighting_engine);

    static QHBoxLayout* create_horizontal_layout();
    static QVBoxLayout* create_vertical_layout();
    static QFormLayout* create_form_layout();
    static QFormLayout* create_form_layout(const QString& label, QWidget* widget);
    static QWidget* create_horizontal_group(QWidget* widget1, QWidget* widget2);

    QSpinBox* create_integer_input(
        const std::string&              widget_key,
        const int                       min,
        const int                       max);

    QSpinBox* create_integer_input(
        const std::string&              widget_key,
        const int                       min,
        const int                       max,
        const QString&                  label);

    QDoubleSpinBox* create_double_input(
        const std::string&              widget_key,
        const double                    min,
        const double                    max,
        const int                       decimals,
        const double                    step);

    QCheckBox* create_checkbox(
        const std::string&              widget_key,
        const QString&                  label);

    QGroupBox* create_checkable_groupbox(
        const std::string&              widget_key,
        const QString&                  label);

    QComboBox* create_combobox(
        const std::string&              widget_key);

    void create_direct_links();

    template <typename T>
    void create_direct_link(
        const std::string&              widget_key,
        const std::string&              param_path,
        const T&                        default_value);

    void load_configuration(const QString& name);
    void save_current_configuration();

    void load_configuration(const renderer::Configuration& config);
    void save_configuration(renderer::Configuration& config);
    renderer::Configuration& get_configuration(const QString& name) const;

    void load_directly_linked_values(const renderer::Configuration& config);
    void save_directly_linked_values(renderer::Configuration& config);

    template <typename T>
    void set_widget(
        const std::string&              widget_key,
        const T&                        value);

    template <typename T>
    T get_widget(const std::string& widget_key);

    template <typename T>
    void set_config(
        renderer::Configuration&        configuration,
        const std::string&              param_path,
        const T&                        value);

    template <typename T>
    T get_config(
        const renderer::Configuration&  configuration,
        const std::string&              param_path,
        const T&                        default_value);

  private slots:
    void slot_open_configuration_manager_window();
    void slot_change_active_configuration(const QString& configuration_name);
    void slot_save_configuration_and_close();

    void slot_changed_image_plane_sampler(int index);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_RENDERSETTINGSWINDOW_H
