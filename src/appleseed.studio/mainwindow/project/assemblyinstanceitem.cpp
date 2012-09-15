
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "mainwindow/project/basegroupitem.h"
#include "mainwindow/project/instancecollectionitem.h"
#include "mainwindow/project/projectbuilder.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"
#include "renderer/api/scene.h"

using namespace renderer;

namespace appleseed {
namespace studio {

AssemblyInstanceItem::AssemblyInstanceItem(
    AssemblyInstance*   assembly_instance,
    BaseGroup&          parent,
    BaseGroupItem*      parent_item,
    ProjectBuilder&     project_builder)
  : EntityItemBase<AssemblyInstance>(assembly_instance)
  , m_parent(parent)
  , m_parent_item(parent_item)
  , m_project_builder(project_builder)
{
    set_allow_edition(false);
}

void AssemblyInstanceItem::slot_delete()
{
    if (!allows_deletion())
        return;

    m_parent_item->get_assembly_instance_collection_item().remove_item(m_entity->get_uid());

    m_parent.assembly_instances().remove(m_entity->get_uid());
    
    m_project_builder.get_project().get_scene()->bump_version_id();
    m_project_builder.notify_project_modification();

    delete this;
}

}   // namespace studio
}   // namespace appleseed
