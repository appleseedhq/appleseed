
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
#include "assemblycollectionitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblyitem.h"
#include "mainwindow/project/basegroupitem.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/tools.h"

// appleseed.renderer headers.
#include "renderer/api/scene.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QMenu>

// Standard headers.
#include <cassert>
#include <string>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

namespace
{
    const UniqueID g_class_uid = new_guid();
}

AssemblyCollectionItem::AssemblyCollectionItem(
    EntityEditorContext&    editor_context,
    AssemblyContainer&      assemblies,
    BaseGroup&              parent,
    BaseGroupItem*          parent_item)
  : CollectionItemBase<Assembly>(editor_context, g_class_uid, "Assemblies")
  , m_parent(parent)
  , m_parent_item(parent_item)
{
    add_items(assemblies);
}

QMenu* AssemblyCollectionItem::get_single_item_context_menu() const
{
    QMenu* menu = CollectionItemBase<Assembly>::get_single_item_context_menu();

    menu->addSeparator();
    menu->addAction("Create Assembly...", this, SLOT(slot_create()));

    return menu;
}

void AssemblyCollectionItem::slot_create()
{
    const std::string assembly_name =
        get_entity_name_dialog(
            treeWidget(),
            "Create Assembly",
            "Assembly Name:",
            make_unique_name("assembly", m_parent.assemblies()));

    // todo: schedule creation of assembly when rendering.
    if (!assembly_name.empty())
    {
        auto_release_ptr<Assembly> assembly(
            AssemblyFactory().create(assembly_name.c_str(), ParamArray()));

        AssemblyItem* assembly_item =
            static_cast<AssemblyItem*>(m_parent_item->add_item(assembly.get()));

        m_parent.assemblies().insert(assembly);

        const std::string assembly_instance_name =
            make_unique_name(
                assembly_name + "_inst",
                m_parent.assembly_instances());

        assembly_item->instantiate(assembly_instance_name);

        m_editor_context.m_project_builder.slot_notify_project_modification();
    }
}

ItemBase* AssemblyCollectionItem::create_item(Assembly* assembly)
{
    assert(assembly);

    return
        new AssemblyItem(
            m_editor_context,
            *assembly,
            m_parent,
            m_parent_item);
}

}   // namespace studio
}   // namespace appleseed
