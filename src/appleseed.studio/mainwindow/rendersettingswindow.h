
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
#include <QWidget>

// Forward declarations.
namespace Ui    { class RenderSettingsWindow; }
class QFormLayout;
class QHBoxLayout;
class QLayout;
class QString;
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
    explicit RenderSettingsWindow(QWidget* parent = 0);

    // Destructor.
    ~RenderSettingsWindow();

  private:
    // Not wrapped in std::auto_ptr<> to avoid pulling in the UI definition code.
    Ui::RenderSettingsWindow* m_ui;

    void create_panels() const;

    void create_image_plane_sampling_panel(QLayout* parent) const;
    void create_image_plane_sampling_general_settings(QVBoxLayout* parent) const;
    void create_image_plane_sampling_sampler_settings(QVBoxLayout* parent) const;
    void create_image_plane_sampling_uniform_sampler_settings(QHBoxLayout* parent) const;
    void create_image_plane_sampling_adaptive_sampler_settings(QHBoxLayout* parent) const;

    void create_lighting_panel(QLayout* parent) const;
    void create_lighting_general_settings(QVBoxLayout* parent) const;
    void create_lighting_components_settings(QVBoxLayout* parent) const;

    void create_shading_panel(QLayout* parent) const;
    void create_shading_texture_cache_settings(QVBoxLayout* parent) const;

    void create_advanced_panel(QLayout* parent) const;
    void create_advanced_direct_lighting_settings(QVBoxLayout* parent) const;
    void create_advanced_image_based_lighting_settings(QVBoxLayout* parent) const;
    void create_advanced_distribution_ray_tracer_settings(QVBoxLayout* parent) const;
    void create_advanced_path_tracer_settings(QVBoxLayout* parent) const;

    QHBoxLayout* create_horizontal_layout() const;
    QVBoxLayout* create_vertical_layout() const;
    QFormLayout* create_form_layout() const;
    QFormLayout* create_form_layout(const QString& label, QWidget* widget) const;
    QWidget* create_integer_input(const int min, const int max) const;
    QWidget* create_integer_input(const int min, const int max, const QString& suffix) const;
    QWidget* create_floating_point_input(const double min, const double max) const;

  private slots:
    void slot_open_configuration_manager_window();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_RENDERSETTINGSWINDOW_H
