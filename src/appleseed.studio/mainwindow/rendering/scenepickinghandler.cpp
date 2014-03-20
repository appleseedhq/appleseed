
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "mainwindow/project/projectexplorer.h"
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
#include <QComboBox>
#include <QEvent>
#include <QMouseEvent>
#include <QString>
#include <Qt>
#include <QWidget>

// Standard headers.
#include <cassert>
#include <sstream>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

ScenePickingHandler::ScenePickingHandler(
    QWidget*                        widget,
    QComboBox*                      picking_mode_combo,
    const MouseCoordinatesTracker&  mouse_tracker,
    const ProjectExplorer&          project_explorer,
    const Project&                  project)
  : m_widget(widget)
  , m_picking_mode_combo(picking_mode_combo)
  , m_mouse_tracker(mouse_tracker)
  , m_project_explorer(project_explorer)
  , m_project(project)
  , m_enabled(true)
{
    m_widget->installEventFilter(this);

    m_picking_mode_combo->clear();
    m_picking_mode_combo->addItem("Assembly", "assembly");
    m_picking_mode_combo->addItem("Assembly Instance", "assembly_instance");
    m_picking_mode_combo->addItem("Object", "object");
    m_picking_mode_combo->addItem("Object Instance", "object_instance");
    m_picking_mode_combo->addItem("Material", "material");
    m_picking_mode_combo->addItem("Surface Shader", "surface_shader");
    m_picking_mode_combo->addItem("BSDF", "bsdf");
    m_picking_mode_combo->addItem("EDF", "edf");
    m_picking_mode_combo->setCurrentIndex(3);
}

ScenePickingHandler::~ScenePickingHandler()
{
    m_widget->removeEventFilter(this);
}

void ScenePickingHandler::set_enabled(const bool enabled)
{
    m_enabled = enabled;
}

bool ScenePickingHandler::eventFilter(QObject* object, QEvent* event)
{
    if (!m_enabled)
        return QObject::eventFilter(object, event);

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
            sstr << "\"" << entity->get_name() << "\" (#" << entity->get_uid() << ")";
        else sstr << "n/a";

        sstr << endl;
    }

    const Entity* get_picked_entity(
        const ScenePicker::PickingResult&   picking_result,
        const QString&                      picking_mode)
    {
        if (picking_mode == "assembly")
            return picking_result.m_assembly;
        else if (picking_mode == "assembly_instance")
            return picking_result.m_assembly_instance;
        else if (picking_mode == "object")
            return picking_result.m_object;
        else if (picking_mode == "object_instance")
            return picking_result.m_object_instance;
        else if (picking_mode == "material")
            return picking_result.m_material;
        else if (picking_mode == "surface_shader")
            return picking_result.m_surface_shader;
        else if (picking_mode == "bsdf")
            return picking_result.m_bsdf;
        else if (picking_mode == "edf")
            return picking_result.m_edf;
        else
        {
            assert(!"Invalid picking mode.");
            return 0;
        }
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

    const QString picking_mode =
        m_picking_mode_combo->itemData(m_picking_mode_combo->currentIndex()).value<QString>();

    const Entity* picked_entity = get_picked_entity(result, picking_mode);

    if (picked_entity)
        m_project_explorer.highlight_entity(picked_entity->get_uid());
    else m_project_explorer.clear_highlighting();
}

}   // namespace studio
}   // namespace appleseed
