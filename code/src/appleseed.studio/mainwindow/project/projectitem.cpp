
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
#include "projectitem.h"

// appleseed.studio headers.
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/multimodelcollectionitem.h"
#include "mainwindow/project/outputitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/sceneitem.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

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

    m_scene_item = new SceneItem(editor_context, *project.get_scene());
    addChild(m_scene_item);

    m_output_item = new OutputItem(editor_context, project);
    addChild(m_output_item);
}

void ProjectItem::expand()
{
    setExpanded(true);

    m_scene_item->expand();
    m_output_item->setExpanded(true);
}

}   // namespace studio
}   // namespace appleseed
