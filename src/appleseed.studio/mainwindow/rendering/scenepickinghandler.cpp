
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "mainwindow/project/itembase.h"
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
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/math/fp.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/string.h"

// Qt headers.
#include <QComboBox>
#include <QEvent>
#include <QMenu>
#include <QMouseEvent>
#include <QString>
#include <Qt>
#include <QWidget>

// Standard headers.
#include <cassert>
#include <cstddef>
#include <ostream>
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
    m_picking_mode_combo->addItem("BSSRDF", "bssrdf");
    m_picking_mode_combo->addItem("EDF", "edf");

    m_picking_mode_combo->setCurrentIndex(3);   // default to Object Instance mode
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
    if (m_enabled)
    {
        switch (event->type())
        {
          case QEvent::MouseButtonPress:
            {
                const QMouseEvent* mouse_event = static_cast<QMouseEvent*>(event);
                if (!(mouse_event->modifiers() & (Qt::AltModifier | Qt::ShiftModifier | Qt::ControlModifier)))
                {
                    if (mouse_event->button() == Qt::LeftButton)
                    {
                        pick(mouse_event->pos());
                        return true;
                    }
                    else if (mouse_event->button() == Qt::RightButton)
                    {
                        ItemBase* item = pick(mouse_event->pos());
                        if (item)
                        {
                            QMenu* menu = item->get_single_item_context_menu();
                            menu->exec(mouse_event->globalPos());
                        }
                        return true;
                    }
                }
            }
            break;
        }
    }

    return QObject::eventFilter(object, event);
}

namespace
{
    const char* get_primitive_type_name(const ShadingPoint::PrimitiveType primitive_type)
    {
        switch (primitive_type)
        {
          case ShadingPoint::PrimitiveNone: return "none";
          case ShadingPoint::PrimitiveTriangle: return "triangle";
          case ShadingPoint::PrimitiveCurve1: return "curve";
          case ShadingPoint::PrimitiveCurve3: return "curve";
          default: return "unknown";
        }
    }

    const char* get_side_name(const ObjectInstance::Side side)
    {
        switch (side)
        {
          case ObjectInstance::FrontSide: return "front";
          case ObjectInstance::BackSide: return "back";
          case ObjectInstance::BothSides: return "front+back";
          default: return "unknown";
        }
    }

    template <typename T, size_t N>
    Vector<T, N> filter_neg_zero(const Vector<T, N>& v)
    {
        Vector<T, N> result;

        for (size_t i = 0; i < N; ++i)
            result[i] = FP<T>::is_neg_zero(v[i]) ? T(0.0) : v[i];

        return result;
    }

    string print_entity(const char* prefix, const Entity* entity)
    {
        return
            entity
                ? format("{0}\"{1}\" (#{2})", prefix, entity->get_path(), entity->get_uid())
                : format("{0}n/a", prefix);
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
        else if (picking_mode == "bssrdf")
            return picking_result.m_bssrdf;
        else if (picking_mode == "edf")
            return picking_result.m_edf;
        else
        {
            assert(!"Invalid picking mode.");
            return 0;
        }
    }
}

ItemBase* ScenePickingHandler::pick(const QPoint& point)
{
    if (!m_project.has_trace_context())
    {
        RENDERER_LOG_INFO("the scene must be rendering or must have been rendered at least once for picking to be available.");
        return 0;
    }

    const Vector2i pix = m_mouse_tracker.widget_to_pixel(point);
    const Vector2d ndc = m_mouse_tracker.widget_to_ndc(point);

    const ScenePicker scene_picker(m_project);
    const ScenePicker::PickingResult result = scene_picker.pick(ndc);

    stringstream sstr;

    sstr << "picking details:" << endl;

    sstr << "  pixel coordinates             " << pix << endl;
    sstr << "  ndc coordinates               " << ndc << endl;
    sstr << "  primitive type                " << get_primitive_type_name(result.m_primitive_type) << endl;
    sstr << "  distance                      " << result.m_distance << endl;

    sstr << "  barycentric coordinates       " << filter_neg_zero(result.m_bary) << endl;
    sstr << "  uv coordinates                " << filter_neg_zero(result.m_uv) << endl;
    sstr << "  duvdx                         " << filter_neg_zero(result.m_duvdx) << endl;
    sstr << "  duvdy                         " << filter_neg_zero(result.m_duvdy) << endl;
    sstr << "  point                         " << filter_neg_zero(result.m_point) << endl;
    sstr << "  dpdu                          " << filter_neg_zero(result.m_dpdu) << endl;
    sstr << "  dpdv                          " << filter_neg_zero(result.m_dpdv) << endl;
    sstr << "  dndu                          " << filter_neg_zero(result.m_dndu) << endl;
    sstr << "  dndv                          " << filter_neg_zero(result.m_dndv) << endl;
    sstr << "  dpdx                          " << filter_neg_zero(result.m_dpdx) << endl;
    sstr << "  dpdy                          " << filter_neg_zero(result.m_dpdy) << endl;
    sstr << "  geometric normal              " << filter_neg_zero(result.m_geometric_normal) << endl;
    sstr << "  shading normal                " << filter_neg_zero(result.m_original_shading_normal) << endl;
    sstr << "  side                          " << get_side_name(result.m_side) << endl;

    sstr << print_entity("  camera                        ", result.m_camera) << endl;
    sstr << print_entity("  assembly instance             ", result.m_assembly_instance) << endl;
    sstr << print_entity("  assembly                      ", result.m_assembly) << endl;
    sstr << print_entity("  object instance               ", result.m_object_instance) << endl;
    sstr << print_entity("  object                        ", result.m_object) << endl;
    sstr << print_entity("  material                      ", result.m_material) << endl;
    sstr << print_entity("  surface shader                ", result.m_surface_shader) << endl;
    sstr << print_entity("  bsdf                          ", result.m_bsdf) << endl;
    sstr << print_entity("  bssrdf                        ", result.m_bssrdf) << endl;
    sstr << print_entity("  edf                           ", result.m_edf);

    RENDERER_LOG_INFO("%s", sstr.str().c_str());

    emit signal_entity_picked(result);

    const QString picking_mode =
        m_picking_mode_combo->itemData(m_picking_mode_combo->currentIndex()).value<QString>();
    const Entity* picked_entity = get_picked_entity(result, picking_mode);

    ItemBase* item;

    if (picked_entity)
    {
        item = m_project_explorer.select_entity(picked_entity->get_uid());
    }
    else
    {
        m_project_explorer.clear_selection();
        item = 0;
    }

    m_widget->setFocus();

    return item;
}

}   // namespace studio
}   // namespace appleseed
