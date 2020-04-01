
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Kevin Masson, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

// appleseed.renderer headers.
#include "renderer/api/lighting.h"
#include "renderer/utility/paramarray.h"

// Qt headers.
#include <QObject>

// Forward declarations.
namespace appleseed { namespace studio { class LightPathsLayer; } }
namespace appleseed { namespace studio { class ViewportCanvas; } }
namespace renderer  { class Project; }
class QEvent;
class QPoint;
class QWidget;

namespace appleseed {
namespace studio {

//
// Keep track of selected light paths.
//

class LightPathsManager
  : public QObject
{
    Q_OBJECT

  public:
    LightPathsManager(
        const renderer::Project&    project,
        renderer::ParamArray&       application_settings);

    void set_light_paths(
        const renderer::LightPathArray& light_paths);

    void clear_light_paths();

    void display_light_paths(const bool on);
    bool should_display_light_paths() const;

    void save_all_light_paths(QWidget* source) const;

    const renderer::LightPathArray& get_light_paths() const;

    int get_selected_light_path_index() const;

  signals:
    void signal_light_path_selection_changed(
        const bool display_light_paths,
        const int selected_light_path_index,
        const int total_light_paths) const;

  public slots:
    void slot_select_all_light_paths();
    void slot_select_previous_light_path();
    void slot_select_next_light_path();
    void slot_clear_light_paths();

  private:
    const renderer::Project&    m_project;
    renderer::ParamArray&       m_application_settings;

    renderer::LightPathArray    m_light_paths;
    int                         m_selected_light_path_index;  // The index of the shown path. All paths are displayed when using -1.
    bool                        m_display_light_paths;

    void set_selected_light_path_index(int index);
    void print_selected_light_paths() const;
};

}   // namespace studio
}   // namespace appleseed
