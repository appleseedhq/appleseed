
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "scenepickinghandler.h"

// appleseed.studio headers.
#include "utility/mousecoordinatestracker.h"

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"
#include "renderer/api/camera.h"
#include "renderer/api/edf.h"
#include "renderer/api/entity.h"
#include "renderer/api/log.h"
#include "renderer/api/material.h"
#include "renderer/api/object.h"
#include "renderer/api/project.h"
#include "renderer/api/rendering.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Qt headers.
#include <QEvent>
#include <QMouseEvent>
#include <Qt>
#include <QWidget>

// Standard headers.
#include <sstream>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

ScenePickingHandler::ScenePickingHandler(
    const MouseCoordinatesTracker&  mouse_tracker,
    const Project&                  project)
  : m_mouse_tracker(mouse_tracker)
  , m_project(project)
{
    m_mouse_tracker.get_widget()->installEventFilter(this);
}

ScenePickingHandler::~ScenePickingHandler()
{
    m_mouse_tracker.get_widget()->removeEventFilter(this);
}

bool ScenePickingHandler::eventFilter(QObject* object, QEvent* event)
{
    if (event->type() == QEvent::MouseButtonPress)
    {
        const QMouseEvent* mouse_event = static_cast<QMouseEvent*>(event);

        if (mouse_event->button() == Qt::LeftButton &&
            !(mouse_event->modifiers() & (Qt::AltModifier | Qt::ShiftModifier | Qt::ControlModifier)))
        {
            pick(mouse_event->pos());
            return true;
        }
    }

    return QObject::eventFilter(object, event);
}

namespace
{
    void print_entity(stringstream& sstr, const char* label, const Entity* entity)
    {
        sstr << label;

        if (entity)
            sstr << entity->get_name() << " (#" << entity->get_uid() << ")";
        else sstr << "n/a";

        sstr << endl;
    }
}

void ScenePickingHandler::pick(const QPoint& point)
{
    if (!m_project.has_trace_context())
    {
        RENDERER_LOG_INFO("the scene must be rendering or must have been rendered at least once for picking to be available.");
        return;
    }

    const Vector2i pix = m_mouse_tracker.widget_to_pixel(point);
    const Vector2d ndc = m_mouse_tracker.widget_to_ndc(point);

    const ScenePicker scene_picker(m_project.get_trace_context());
    const ScenePicker::PickingResult result = scene_picker.pick(ndc);

    stringstream sstr;

    sstr << "picking details:" << endl;
    sstr << "  pixel coords     " << pix.x << ", " << pix.y << endl;
    sstr << "  ndc coords       " << ndc.x << ", " << ndc.y << endl;

    print_entity(sstr, "  camera           ", result.m_camera);
    print_entity(sstr, "  assembly inst.   ", result.m_assembly_instance);
    print_entity(sstr, "  assembly         ", result.m_assembly);
    print_entity(sstr, "  object inst.     ", result.m_object_instance);
    print_entity(sstr, "  object           ", result.m_object);
    print_entity(sstr, "  material         ", result.m_material);
    print_entity(sstr, "  surface shader   ", result.m_surface_shader);
    print_entity(sstr, "  bsdf             ", result.m_bsdf);
    print_entity(sstr, "  edf              ", result.m_edf);

    RENDERER_LOG_INFO("%s", sstr.str().c_str());
}

}   // namespace studio
}   // namespace appleseed
