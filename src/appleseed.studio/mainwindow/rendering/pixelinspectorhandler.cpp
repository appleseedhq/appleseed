
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

#include "pixelinspectorhandler.h"
#include "renderwidget.h"

// appleseed.studio headers.
#include "mainwindow/project/projectexplorer.h"
#include "utility/mousecoordinatestracker.h"

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"
#include "renderer/api/camera.h"
#include "renderer/api/edf.h"
#include "renderer/api/entity.h"
#include "renderer/api/frame.h"
#include "renderer/api/log.h"
#include "renderer/api/material.h"
#include "renderer/api/object.h"
#include "renderer/api/project.h"
#include "renderer/api/rendering.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/math/vector.h"

// Qt headers.
#include <QComboBox>
#include <QEvent>
#include <QFont>
#include <QLabel>
#include <QMouseEvent>
#include <QString>
#include <Qt>
#include <QToolTip>
#include <QWidget>

// Standard headers.
#include <cassert>
#include <sstream>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

PixelInspectorHandler::PixelInspectorHandler(
        QWidget*                        widget,
        const MouseCoordinatesTracker&  mouse_tracker,
        const ProjectExplorer&          project_explorer,
        const Project&                  project)
    : m_widget(widget)
    , m_mouse_tracker(mouse_tracker)
    , m_project_explorer(project_explorer)
    , m_project(project)
    , m_enabled(true)
{
    m_widget->installEventFilter(this);
}

PixelInspectorHandler::~PixelInspectorHandler()
{
    m_widget->removeEventFilter(this);
}

void PixelInspectorHandler::set_enabled(const bool enabled)
{
    m_enabled = enabled;
}

bool PixelInspectorHandler::eventFilter(QObject* object, QEvent* event)
{
    if (!m_enabled)
        return QObject::eventFilter(object, event);

    const QMouseEvent* mouse_event = static_cast<QMouseEvent*>(event);

    switch (event->type())
    {
        case QEvent::MouseMove:
        show_tooltip(mouse_event->pos(),mouse_event->globalPos());
        return false;
        break;
    }

    return QObject::eventFilter(object, event);
}

void PixelInspectorHandler::show_tooltip(const QPoint& point, const QPoint& global_point)
{
    if (!m_project.has_trace_context())
    {
        RENDERER_LOG_INFO("the scene must be rendering or must have been rendered at least once for picking to be available.");
        return;
    }

    Color4f linear_rgba;
    const Vector2d pix = m_mouse_tracker.widget_to_pixel(point);
    const Vector2d ndc = m_mouse_tracker.widget_to_ndc(point);


    stringstream sstr;

    sstr << "picking details tooltip:" << endl;

    sstr << "  ndc coords       " << pix.x << ", " << pix.y << endl;

    RENDERER_LOG_INFO("%s", sstr.str().c_str());

    m_project.get_frame()->image().get_pixel(static_cast<size_t>(pix.x), static_cast<size_t>(pix.y) ,linear_rgba);
    QToolTip::showText
     (
         global_point,
         QString("Pixel Inspector \n\n") +
         QString("Pixel : \n") +
                        "X: " + QString::number( pix.x) + "\n" +
                        "Y: " + QString::number( pix.y) + "\n" + "\n" +
                 ("NDC :  \n") +
                        "X: " + QString::number (ndc.x,'f',3) + "\n" +
                        "Y: " + QString::number (ndc.y,'f',3) + "\n" + "\n" +
         QString("Color : \n") +
                        "R: " + QString::number( linear_rgba.r,'f',4) + "\n" +
                        "G: " + QString::number( linear_rgba.g,'f',4) + "\n" +
                        "B: " + QString::number( linear_rgba.b,'f',4) + "\n" +
                        "A: " + QString::number( linear_rgba.a,'f',4),
         dynamic_cast<QWidget*>(this), QRect()
     );
}

}   // namespace studio
}   // namespace appleseed
