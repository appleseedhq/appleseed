
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_SETTINGSWINDOW_H
#define APPLESEED_STUDIO_MAINWINDOW_SETTINGSWINDOW_H

// Qt headers.
#include <QObject>
#include <QWidget>

// Forward declarations.
namespace renderer  { class ParamArray; }
namespace Ui        { class SettingsWindow; }

namespace appleseed {
namespace studio {

//
// Settings window.
//

class SettingsWindow
  : public QWidget
{
    Q_OBJECT

  public:
    // Constructor.
    SettingsWindow(
        renderer::ParamArray&   settings,
        QWidget*                parent = nullptr);

    // Destructor.
    ~SettingsWindow() override;

  signals:
    void signal_settings_modified() const;

  private:
    // Not wrapped in std::unique_ptr<> to avoid pulling in the UI definition code.
    Ui::SettingsWindow*     m_ui;

    renderer::ParamArray&   m_settings;

    void build_connections();

    void load_settings();
    void save_settings();

  private slots:
    void slot_save_configuration_and_close();
    void slot_restore_configuration_and_close();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_SETTINGSWINDOW_H
