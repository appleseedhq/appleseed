
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
#include "renderclipboardhandler.h"

// appleseed.studio headers.
#include "mainwindow/rendering/renderwidget.h"

// appleseed.renderer headers.
#include "renderer/api/log.h"

// Qt headers.
#include <QApplication>
#include <QClipboard>
#include <QEvent>
#include <QKeyEvent>
#include <Qt>

namespace appleseed {
namespace studio {

RenderClipboardHandler::RenderClipboardHandler(RenderWidget* widget)
  : m_widget(widget)
{
    m_widget->installEventFilter(this);
}

RenderClipboardHandler::~RenderClipboardHandler()
{
    m_widget->removeEventFilter(this);
}

bool RenderClipboardHandler::eventFilter(QObject* object, QEvent* event)
{
    if (event->type() == QEvent::KeyPress)
    {
        const QKeyEvent* key_event = static_cast<QKeyEvent*>(event);

        if (key_event->modifiers() == Qt::ControlModifier && key_event->key() == Qt::Key_C)
        {
            QApplication::clipboard()->setImage(m_widget->get_image_copy());
            RENDERER_LOG_INFO("copied render to clipboard.");
            return true;
        }
    }

    return QObject::eventFilter(object, event);
}

}   // namespace studio
}   // namespace appleseed
