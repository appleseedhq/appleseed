
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

// Interface header.
#include "lightpathsmanager.h"

// appleseed.studio headers.
#include "mainwindow/rendering/lightpathslayer.h"
#include "mainwindow/rendering/viewportcanvas.h"
#include "utility/settingskeys.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/lighting.h"
#include "renderer/api/log.h"
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/vector.h"
#include "foundation/string/string.h"

// Qt headers.
#include <QEvent>
#include <QMouseEvent>
#include <QString>
#include <Qt>
#include <QWidget>

// Standard headers.
#include <algorithm>
#include <cmath>
#include <array>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

LightPathsManager::LightPathsManager(
    const renderer::Project&    project,
    renderer::ParamArray&       application_settings)
  : m_project(project)
  , m_application_settings(application_settings)
  , m_selected_light_path_index(-1)
  , m_display_light_paths(false)
{
}

void LightPathsManager::set_light_paths(const LightPathArray& light_paths)
{
    m_light_paths = light_paths;

    if (m_light_paths.size() > 1)
    {
        // Sort paths by descending radiance at the camera.
        const auto& light_path_recorder = m_project.get_light_path_recorder();
        std::sort(
            &m_light_paths[0],
            &m_light_paths[0] + m_light_paths.size(),
            [&light_path_recorder](const LightPath& lhs, const LightPath& rhs)
            {
                LightPathVertex lhs_v;
                light_path_recorder.get_light_path_vertex(lhs.m_vertex_end_index - 1, lhs_v);

                LightPathVertex rhs_v;
                light_path_recorder.get_light_path_vertex(rhs.m_vertex_end_index - 1, rhs_v);

                return
                    sum_value(Color3f::from_array(lhs_v.m_radiance)) >
                    sum_value(Color3f::from_array(rhs_v.m_radiance));
            });
    }

    set_selected_light_path_index(-1);
}

void LightPathsManager::clear_light_paths()
{
    m_light_paths.clear();
    set_selected_light_path_index(-1);
}

void LightPathsManager::display_light_paths(const bool on)
{
    m_display_light_paths = on;
    set_selected_light_path_index(m_selected_light_path_index);
}

bool LightPathsManager::should_display_light_paths() const
{
    return m_display_light_paths;
}

void LightPathsManager::save_all_light_paths(QWidget* source) const
{
    QString filepath =
        get_save_filename(
            source,
            "Save Light Paths As...",
            "Light Paths Files (*.aspaths);;All Files (*.*)",
            m_application_settings,
            SETTINGS_FILE_DIALOG_LIGHT_PATHS);

    if (filepath.isEmpty())
        return;

    if (QFileInfo(filepath).suffix().isEmpty())
        filepath += ".aspaths";

    // Write light paths to disk.
    m_project.get_light_path_recorder().write(filepath.toUtf8().constData());
}

const renderer::LightPathArray& LightPathsManager::get_light_paths() const
{
    return m_light_paths;
}

int LightPathsManager::get_selected_light_path_index() const
{
    return m_selected_light_path_index;
}

void LightPathsManager::slot_select_all_light_paths()
{
    set_selected_light_path_index(-1);
}

void LightPathsManager::slot_select_previous_light_path()
{
    if (m_selected_light_path_index > -1)
        set_selected_light_path_index(m_selected_light_path_index - 1);
}

void LightPathsManager::slot_select_next_light_path()
{
    if (m_selected_light_path_index < static_cast<int>(m_light_paths.size()) - 1)
        set_selected_light_path_index(m_selected_light_path_index + 1);
}

void LightPathsManager::slot_clear_light_paths()
{
    m_light_paths.clear();
    set_selected_light_path_index(-1);

    m_project.get_light_path_recorder().clear();
}

void LightPathsManager::set_selected_light_path_index(int index)
{
    m_selected_light_path_index = index;

    if (m_display_light_paths)
        print_selected_light_paths();
    
    emit signal_light_path_selection_changed(
        m_display_light_paths,
        m_selected_light_path_index,
        static_cast<int>(m_light_paths.size()));
}

void LightPathsManager::print_selected_light_paths() const
{
    if (m_selected_light_path_index == -1)
    {
        if (m_light_paths.empty())
            RENDERER_LOG_INFO("no light path to display.");
        else
        {
            RENDERER_LOG_INFO("displaying %s light path%s.",
                pretty_uint(m_light_paths.size()).c_str(),
                m_light_paths.size() > 1 ? "s" : "");
        }
    }
    else
    {
        RENDERER_LOG_INFO("displaying light path %s/%s:",
            pretty_int(m_selected_light_path_index + 1).c_str(),
            pretty_int(m_light_paths.size()).c_str());

        const auto& light_path_recorder = m_project.get_light_path_recorder();
        const auto& path = m_light_paths[m_selected_light_path_index];

        for (std::size_t i = path.m_vertex_begin_index; i < path.m_vertex_end_index; ++i)
        {
            LightPathVertex v;
            light_path_recorder.get_light_path_vertex(i, v);

            const std::string entity_name =
                v.m_entity != nullptr
                    ? foundation::format("\"{0}\"", v.m_entity->get_path().c_str())
                    : "n/a";

            RENDERER_LOG_INFO("  vertex " FMT_SIZE_T ": entity: %s - position: (%f, %f, %f) - radiance: (%f, %f, %f) - total radiance: %f",
                i - path.m_vertex_begin_index + 1,
                entity_name.c_str(),
                v.m_position[0], v.m_position[1], v.m_position[2],
                v.m_radiance[0], v.m_radiance[1], v.m_radiance[2],
                v.m_radiance[0] + v.m_radiance[1] + v.m_radiance[2]);
        }
    }
}

}   // namespace studio
}   // namespace appleseed
