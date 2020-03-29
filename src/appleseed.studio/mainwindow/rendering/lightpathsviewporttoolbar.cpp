
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
#include "lightpathsviewporttoolbar.h"

// appleseed.studio headers.
#include "mainwindow/rendering/lightpathspickinghandler.h"
#include "mainwindow/rendering/lightpathslayer.h"
#include "mainwindow/rendering/viewporttab.h"
#include "utility/settingskeys.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/lighting.h"
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

// Qt headers.
#include <QAction>
#include <QApplication>
#include <QDir>
#include <QEvent>
#include <QFileInfo>
#include <QGridLayout>
#include <QKeyEvent>
#include <QLabel>
#include <QMenu>
#include <QScrollArea>
#include <QSize>
#include <QString>
#include <Qt>
#include <QToolBar>
#include <QToolButton>

// Standard headers.
#include <cassert>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

//
// LightPathsViewportToolbar class implementation.
//

LightPathsViewportToolbar::LightPathsViewportToolbar(
    ViewportTab*                    viewport_tab,
    Project*                        project,
    LightPathsManager&              light_paths_manager)
  : m_enabled(false)
  , m_project(project)
  , m_light_paths_manager(light_paths_manager)
  , m_viewport_canvas(viewport_tab->get_viewport_canvas())
{
    LightPathsLayer* light_paths_layer = m_viewport_canvas->get_light_paths_layer();

    connect(
        &m_light_paths_manager, &LightPathsManager::signal_light_path_selection_changed,
        this, &LightPathsViewportToolbar::slot_light_path_selection_changed);
    connect(
        this, &LightPathsViewportToolbar::signal_display_next_light_path,
        &m_light_paths_manager, &LightPathsManager::slot_select_next_light_path);
    connect(
        this, &LightPathsViewportToolbar::signal_display_previous_light_path,
        &m_light_paths_manager, &LightPathsManager::slot_select_previous_light_path);

    create_toolbar();

    m_viewport_canvas->installEventFilter(this);
}

void LightPathsViewportToolbar::reset(renderer::Project* project)
{
    m_project = project;
}

void LightPathsViewportToolbar::set_enabled(const bool enabled)
{
    m_enabled = enabled;

    if (enabled)
    {
        refresh_toolbar();
        m_toolbar->show();
    }
    else
    {
        m_toolbar->hide();
    }

    m_toolbar->setDisabled(!enabled);
}

QToolBar* LightPathsViewportToolbar::toolbar() const
{
    return m_toolbar;
}

void LightPathsViewportToolbar::slot_light_path_selection_changed(
    const bool      display_light_paths,
    const int       selected_light_path_index,
    const int       total_light_paths)
{
    if (total_light_paths > 0)
    {
        m_prev_path_button->setEnabled(selected_light_path_index > -1);
        m_next_path_button->setEnabled(selected_light_path_index < total_light_paths - 1);
    }
    else
    {
        m_prev_path_button->setEnabled(false);
        m_next_path_button->setEnabled(false);
    }
}

void LightPathsViewportToolbar::slot_save_light_paths()
{
    m_light_paths_manager.save_all_light_paths(m_toolbar);
}

void LightPathsViewportToolbar::create_toolbar()
{
    // Create the render toolbar.
    m_toolbar = new QToolBar();
    m_toolbar->setObjectName("light_paths_viewport_toolbar");
    m_toolbar->setIconSize(QSize(18, 18));

    // Save Light Paths button.
    m_save_light_paths_button = new QToolButton();
    m_save_light_paths_button->setIcon(load_icons("lightpaths_toolbar_save_light_paths"));
    connect(
        m_save_light_paths_button , &QToolButton::clicked,
        this, &LightPathsViewportToolbar::slot_save_light_paths);
    m_toolbar->addWidget(m_save_light_paths_button);

    m_toolbar->addSeparator();

    // Previous Light Path button.
    m_prev_path_button = new QToolButton();
    m_prev_path_button->setIcon(load_icons("lightpaths_toolbar_prev_light_path"));
    m_prev_path_button->setToolTip("Display previous light path");
    m_prev_path_button->setEnabled(false);
    connect(
        m_prev_path_button, &QToolButton::clicked,
        &m_light_paths_manager, &LightPathsManager::slot_select_previous_light_path);
    m_toolbar->addWidget(m_prev_path_button);

    // Next Light Path button.
    m_next_path_button = new QToolButton();
    m_next_path_button->setIcon(load_icons("lightpaths_toolbar_next_light_path"));
    m_next_path_button->setToolTip("Display next light path");
    m_next_path_button->setEnabled(false);
    connect(
        m_next_path_button, &QToolButton::clicked,
        &m_light_paths_manager, &LightPathsManager::slot_select_next_light_path);
    m_toolbar->addWidget(m_next_path_button);

    m_toolbar->setDisabled(true);
    m_toolbar->hide();
}

void LightPathsViewportToolbar::refresh_toolbar() const
{
    const auto light_path_count = m_project->get_light_path_recorder().get_light_path_count();

    m_save_light_paths_button->setToolTip(
        QString("Save %1 Light Path%2...")
            .arg(QString::fromStdString(pretty_uint(light_path_count)))
            .arg(light_path_count > 1 ? "s" : ""));
}

bool LightPathsViewportToolbar::eventFilter(QObject* object, QEvent* event)
{
    if (m_enabled)
    {
        if (event->type() == QEvent::KeyRelease)
        {
            const QKeyEvent* key_event = static_cast<QKeyEvent*>(event);

            if (!(key_event->modifiers() & (Qt::AltModifier | Qt::ShiftModifier | Qt::ControlModifier)))
            {
                const int key = key_event->key();

                if (key == Qt::Key_Escape)
                    m_light_paths_manager.clear_light_paths();
                else if (key == Qt::Key_Left)
                    emit signal_display_previous_light_path();
                else if (key == Qt::Key_Right)
                    emit signal_display_next_light_path();
            }
        }
    }

    return QObject::eventFilter(object, event);
}

}   // namespace studio
}   // namespace appleseed
