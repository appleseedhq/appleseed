
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

// Interface header.
#include "lightpathspickinghandler.h"

// appleseed.studio headers.
#include "mainwindow/rendering/lightpathslayer.h"
#include "mainwindow/rendering/viewportwidget.h"
#include "utility/mousecoordinatestracker.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/lighting.h"
#include "renderer/api/log.h"
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/utility/string.h"

// Qt headers.
#include <QEvent>
#include <QMouseEvent>
#include <Qt>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

LightPathsPickingHandler::LightPathsPickingHandler(
    ViewportWidget*                 viewport_widget,
    const MouseCoordinatesTracker&  mouse_tracker,
    const Project&                  project)
  : m_project(project)
  , m_enabled(true)
  , m_viewport_widget(viewport_widget)
{
}

void LightPathsPickingHandler::set_enabled(const bool enabled)
{
    m_enabled = enabled;
}

void LightPathsPickingHandler::pick(const Vector2i& pixel, QString* lpe)
{
    const Image& image = m_project.get_frame()->image();
    const CanvasProperties& props = image.properties();

    if (pixel.x >= 0 &&
        pixel.y >= 0 &&
        pixel.x < static_cast<int>(props.m_canvas_width) &&
        pixel.y < static_cast<int>(props.m_canvas_height))
    {
        m_prev_query_x_min = static_cast<size_t>(pixel.x);
        m_prev_query_y_min = static_cast<size_t>(pixel.y);
        m_prev_query_x_max = static_cast<size_t>(pixel.x);
        m_prev_query_y_max = static_cast<size_t>(pixel.y);
        m_prev_query_valid = true;
        LightPathArray light_paths;
        m_project.get_light_path_recorder().query(
            m_prev_query_x_min,
            m_prev_query_y_min,
            m_prev_query_x_max,
            m_prev_query_y_max,
            lpe == NULL ? NULL : lpe->toUtf8().data(),
            light_paths);

        if (light_paths.empty())
            RENDERER_LOG_INFO("no light path found at pixel (%d, %d) with light path expression `%s`.",
                pixel.x,
                pixel.y,
                lpe ? lpe->toUtf8().data() : "");
        else
        {
            RENDERER_LOG_INFO(
                "%s light path%s found at pixel (%d, %d) with light path expression `%s`.",
                pretty_uint(light_paths.size()).c_str(),
                light_paths.size() > 1 ? "s" : "",
                pixel.x,
                pixel.y,
                lpe ? lpe->toUtf8().data() : "");
        }

        m_viewport_widget->get_light_paths_layer()->set_light_paths(light_paths);
        m_viewport_widget->update();
    }
    else
    {
        m_prev_query_valid = false;
    }
}

void LightPathsPickingHandler::pick(const AABB2i& rect, QString* lpe)
{
    const Image& image = m_project.get_frame()->image();
    const CanvasProperties& props = image.properties();

    const AABB2i image_rect(
        Vector2i(0, 0),
        Vector2i(
            static_cast<int>(props.m_canvas_width - 1),
            static_cast<int>(props.m_canvas_height - 1)));

    const auto final_rect = AABB2i::intersect(rect, image_rect);

    if (final_rect.is_valid())
    {
        m_prev_query_x_min = static_cast<size_t>(final_rect.min.x);
        m_prev_query_y_min = static_cast<size_t>(final_rect.min.y);
        m_prev_query_x_max = static_cast<size_t>(final_rect.max.x);
        m_prev_query_y_max = static_cast<size_t>(final_rect.max.y);
        m_prev_query_valid = true;
        LightPathArray light_paths;
        m_project.get_light_path_recorder().query(
            m_prev_query_x_min,
            m_prev_query_y_min,
            m_prev_query_x_max,
            m_prev_query_y_max,
            lpe == NULL ? NULL : lpe->toUtf8().data(),
            light_paths);

        if (light_paths.empty())
        {
            RENDERER_LOG_INFO("no light path found in rectangle (%d, %d)-(%d, %d) with light path expression `%s`.",
                final_rect.min.x,
                final_rect.min.y,
                final_rect.max.x,
                final_rect.max.y,
                lpe ? lpe->toUtf8().data() : "");
        }
        else
        {
            RENDERER_LOG_INFO(
                "%s light path%s found in rectangle (%d, %d)-(%d, %d) with light path expression `%s`.",
                pretty_uint(light_paths.size()).c_str(),
                light_paths.size() > 1 ? "s" : "",
                final_rect.min.x,
                final_rect.min.y,
                final_rect.max.x,
                final_rect.max.y,
                lpe ? lpe->toUtf8().data() : "");
        }

        m_viewport_widget->get_light_paths_layer()->set_light_paths(light_paths);
        m_viewport_widget->update();
    }
    else
    {
        m_prev_query_valid = false;
    }
}

void LightPathsPickingHandler::pick(QString* lpe)
{
    if (m_prev_query_valid)
    {
        LightPathArray light_paths;
        m_project.get_light_path_recorder().query(
            m_prev_query_x_min,
            m_prev_query_y_min,
            m_prev_query_x_max,
            m_prev_query_y_max,
            lpe == NULL ? NULL : lpe->toUtf8().data(),
            light_paths);

        if (light_paths.empty())
        {
            RENDERER_LOG_INFO("no light path found in rectangle (%d, %d)-(%d, %d) with light path expression `%s`.",
                m_prev_query_x_min,
                m_prev_query_y_min,
                m_prev_query_x_max,
                m_prev_query_y_max,
                lpe ? lpe->toUtf8().data() : "");
        }
        else
        {
            RENDERER_LOG_INFO(
                "%s light path%s found in rectangle (%d, %d)-(%d, %d) with light path expression `%s`.",
                pretty_uint(light_paths.size()).c_str(),
                light_paths.size() > 1 ? "s" : "",
                m_prev_query_x_min,
                m_prev_query_y_min,
                m_prev_query_x_max,
                m_prev_query_y_max,
                lpe ? lpe->toUtf8().data() : "");
        }

        m_viewport_widget->get_light_paths_layer()->set_light_paths(light_paths);
        m_viewport_widget->update();
    }
}

}   // namespace studio
}   // namespace appleseed
