
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
#include "mainwindow/rendering/lightpathswidget.h"

// appleseed.qtcommon headers.
#include "widgets/mousecoordinatestracker.h"

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
#include <Qt>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

LightPathsPickingHandler::LightPathsPickingHandler(
    LightPathsWidget*               light_paths_widget,
    const MouseCoordinatesTracker&  mouse_tracker,
    const Project&                  project)
  : m_light_paths_widget(light_paths_widget)
  , m_mouse_tracker(mouse_tracker)
  , m_project(project)
  , m_enabled(true)
{
    m_light_paths_widget->installEventFilter(this);
}

LightPathsPickingHandler::~LightPathsPickingHandler()
{
    m_light_paths_widget->removeEventFilter(this);
}

void LightPathsPickingHandler::set_enabled(const bool enabled)
{
    m_enabled = enabled;
}

void LightPathsPickingHandler::pick(const Vector2i& pixel) const
{
    const Image& image = m_project.get_frame()->image();
    const CanvasProperties& props = image.properties();

    if (pixel.x >= 0 &&
        pixel.y >= 0 &&
        pixel.x < static_cast<int>(props.m_canvas_width) &&
        pixel.y < static_cast<int>(props.m_canvas_height))
    {
        LightPathArray light_paths;
        m_project.get_light_path_recorder().query(
            static_cast<size_t>(pixel.x),
            static_cast<size_t>(pixel.y),
            static_cast<size_t>(pixel.x),
            static_cast<size_t>(pixel.y),
            light_paths);

        if (light_paths.empty())
            RENDERER_LOG_INFO("no light path found at pixel (%d, %d).", pixel.x, pixel.y);
        else
        {
            RENDERER_LOG_INFO(
                "%s light path%s found at pixel (%d, %d).",
                pretty_uint(light_paths.size()).c_str(),
                light_paths.size() > 1 ? "s" : "",
                pixel.x,
                pixel.y);
        }

        m_light_paths_widget->set_light_paths(light_paths);
        m_light_paths_widget->update();
    }
}

void LightPathsPickingHandler::pick(const AABB2i& rect) const
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
        LightPathArray light_paths;
        m_project.get_light_path_recorder().query(
            static_cast<size_t>(final_rect.min.x),
            static_cast<size_t>(final_rect.min.y),
            static_cast<size_t>(final_rect.max.x),
            static_cast<size_t>(final_rect.max.y),
            light_paths);

        if (light_paths.empty())
        {
            RENDERER_LOG_INFO("no light path found in rectangle (%d, %d)-(%d, %d).",
                final_rect.min.x,
                final_rect.min.y,
                final_rect.max.x,
                final_rect.max.y);
        }
        else
        {
            RENDERER_LOG_INFO(
                "%s light path%s found in rectangle (%d, %d)-(%d, %d).",
                pretty_uint(light_paths.size()).c_str(),
                light_paths.size() > 1 ? "s" : "",
                final_rect.min.x,
                final_rect.min.y,
                final_rect.max.x,
                final_rect.max.y);
        }

        m_light_paths_widget->set_light_paths(light_paths);
        m_light_paths_widget->update();
    }
}

bool LightPathsPickingHandler::eventFilter(QObject* object, QEvent* event)
{
    if (m_enabled)
    {
        if (event->type() == QEvent::MouseButtonPress)
        {
            const QMouseEvent* mouse_event = static_cast<QMouseEvent*>(event);
            if (!(mouse_event->modifiers() & (Qt::AltModifier | Qt::ShiftModifier | Qt::ControlModifier)))
            {
                if (mouse_event->button() == Qt::LeftButton)
                {
                    pick(m_mouse_tracker.widget_to_pixel(mouse_event->pos()));
                    return true;
                }
            }
        }
    }

    return QObject::eventFilter(object, event);
}

}   // namespace studio
}   // namespace appleseed
