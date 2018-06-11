
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "projectitem.h"

// appleseed.studio headers.
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/outputitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/sceneitem.h"
#include "mainwindow/project/searchpathswindow.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Qt headers.
#include <QMenu>

// Standard headers.
#include <cassert>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

namespace
{
    const UniqueID g_project_class_uid = new_guid();
}

ProjectItem::ProjectItem(EntityEditorContext& editor_context)
  : ItemBase(editor_context, g_project_class_uid, "Project")
{
    set_allow_deletion(false);
    set_allow_edition(false);

    Project& project = m_editor_context.m_project;

    assert(project.get_scene() != nullptr);
    m_scene_item = new SceneItem(editor_context, *project.get_scene());
    addChild(m_scene_item);

    assert(project.get_frame() != nullptr);
    m_output_item = new OutputItem(editor_context, project.get_frame());
    addChild(m_output_item);
}

QMenu* ProjectItem::get_single_item_context_menu() const
{
    QMenu* menu = ItemBase::get_single_item_context_menu();

    menu->addSeparator();
    menu->addAction("Edit Search Paths...", this, SLOT(slot_edit_search_paths()));

    return menu;
}

void ProjectItem::expand()
{
    setExpanded(true);

    m_scene_item->expand();
    m_output_item->setExpanded(true);
}

void ProjectItem::slot_edit_search_paths()
{
    if (m_search_paths_window.get() == nullptr)
    {
        m_search_paths_window.reset(
            new SearchPathsWindow(
                m_editor_context.m_project,
                QTreeWidgetItem::treeWidget()));

        connect(
            m_search_paths_window.get(), SIGNAL(signal_paths_modified()),
            &m_editor_context.m_project_builder, SLOT(slot_notify_project_modification()));
    }

    m_search_paths_window->showNormal();
    m_search_paths_window->activateWindow();
}

}   // namespace studio
}   // namespace appleseed
