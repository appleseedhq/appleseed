
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

#pragma once

// appleseed.studio headers.
#include "utility/windowbase.h"

// Qt headers.
#include <QObject>
#include <QString>

// Standard headers.
#include <map>
#include <string>
#include <vector>

// Forward declarations.
namespace appleseed { namespace qtcommon { class ProjectManager; } }
namespace appleseed { namespace studio { class RenderSettingsPanel; } }
namespace renderer  { class Configuration; }
namespace renderer  { class ParamArray; }
namespace Ui        { class RenderingSettingsWindow; }
class QWidget;

namespace appleseed {
namespace studio {

//
// Rendering settings window.
//

class RenderingSettingsWindow
  : public WindowBase
{
    Q_OBJECT

  public:
    // Constructor.
    RenderingSettingsWindow(
        qtcommon::ProjectManager&       project_manager,
        const renderer::ParamArray&     application_settings,
        QWidget*                        parent = nullptr);

    // Destructor.
    ~RenderingSettingsWindow() override;

    // Load the configurations of the currently open project.
    void reload();

  signals:
    void signal_rendering_settings_modified() const;
    void signal_application_settings_modified() const;

  public slots:
    void slot_reload_application_settings();

  private:
    typedef std::vector<RenderSettingsPanel*> PanelCollection;

    // Not wrapped in std::unique_ptr<> to avoid pulling in the UI definition code.
    Ui::RenderingSettingsWindow*        m_ui;

    qtcommon::ProjectManager&           m_project_manager;
    const renderer::ParamArray&         m_application_settings;

    PanelCollection                     m_panels;
    QString                             m_current_configuration_name;
    std::map<std::string, std::string>  m_initial_values;

    void create_connections();

    void create_panels(const renderer::Configuration& config);
    void create_layout();
    void set_panels_enabled(const bool enabled);

    void load_configuration(const QString& name);
    void save_current_configuration();
    renderer::Configuration& get_configuration(const QString& name) const;

    std::map<std::string, std::string> get_widget_values() const;

  private slots:
    void slot_open_configuration_manager_window();
    void slot_change_active_configuration(const QString& configuration_name);
    void slot_save_configuration_and_close();
    void slot_restore_configuration_and_close();
};

}   // namespace studio
}   // namespace appleseed
