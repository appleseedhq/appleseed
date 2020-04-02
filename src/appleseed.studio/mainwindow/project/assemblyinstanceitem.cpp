
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
#include "assemblyinstanceitem.h"

// appleseed.studio headers.
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/rendering/renderingmanager.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Standard headers.
#include <memory>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

AssemblyInstanceItem::AssemblyInstanceItem(
    EntityEditorContext&            editor_context,
    AssemblyInstance*               assembly_instance,
    BaseGroup&                      parent,
    AssemblyInstanceCollectionItem* collection_item)
  : EntityItemBase<AssemblyInstance>(editor_context, assembly_instance)
  , m_parent(parent)
  , m_collection_item(collection_item)
{
    set_allow_edition(false);
}

void AssemblyInstanceItem::delete_multiple(const QList<ItemBase*>& items)
{
    m_editor_context.m_rendering_manager.schedule_or_execute(
        std::unique_ptr<RenderingManager::IScheduledAction>(
            new EntityDeletionAction<AssemblyInstanceItem>(
                qlist_static_cast<AssemblyInstanceItem*>(items))));
}

void AssemblyInstanceItem::do_delete()
{
    if (!allows_deletion())
        return;

    // Remove and delete the assembly instance.
    m_parent.assembly_instances().remove(m_entity_uid);

    // Mark the scene and the project as modified.
    m_editor_context.m_project.get_scene()->bump_version_id();
    m_editor_context.m_project_builder.slot_notify_project_modification();

    // Remove and delete the assembly instance item.
    delete this;
}

}   // namespace studio
}   // namespace appleseed
