
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
#include "assemblycollectionitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblyitem.h"
#include "mainwindow/project/basegroupitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/tools.h"

// appleseed.renderer headers.
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QMenu>

// Standard headers.
#include <cassert>
#include <string>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

namespace
{
    const UniqueID g_class_uid = new_guid();
}

AssemblyCollectionItem::AssemblyCollectionItem(
    AssemblyContainer&  assemblies,
    BaseGroup&          parent,
    BaseGroupItem*      parent_item,
    ProjectBuilder&     project_builder,
    ParamArray&         settings)
  : CollectionItemBase<Assembly>(g_class_uid, "Assemblies")
  , m_parent(parent)
  , m_parent_item(parent_item)
  , m_project_builder(project_builder)
  , m_settings(settings)
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
    const string assembly_name_suggestion = get_name_suggestion("assembly", m_parent.assemblies());

    const string assembly_name =
        get_entity_name_dialog(
            treeWidget(),
            "Create Assembly",
            "Assembly Name:",
            assembly_name_suggestion);

    if (!assembly_name.empty())
    {
        auto_release_ptr<Assembly> assembly(
            AssemblyFactory::create(assembly_name.c_str(), ParamArray()));

        m_parent_item->add_item(assembly.get());

        m_parent.assemblies().insert(assembly);

        m_project_builder.notify_project_modification();
    }
}

ItemBase* AssemblyCollectionItem::create_item(Assembly* assembly) const
{
    assert(assembly);

    return
        new AssemblyItem(
            *assembly,
            m_parent,
            m_parent_item,
            m_project_builder,
            m_settings);
}

}   // namespace studio
}   // namespace appleseed
