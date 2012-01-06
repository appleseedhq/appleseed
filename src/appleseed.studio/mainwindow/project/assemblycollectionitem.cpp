
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/tools.h"

// appleseed.foundation headers.
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
    Scene&              scene,
    AssemblyContainer&  assemblies,
    ProjectBuilder&     project_builder,
    ParamArray&         settings)
  : CollectionItemBase<Assembly>(g_class_uid, "Assemblies")
  , m_scene(scene)
  , m_project_builder(project_builder)
  , m_settings(settings)
{
    add_items(assemblies);
}

QMenu* AssemblyCollectionItem::get_single_item_context_menu() const
{
    QMenu* menu = CollectionItemBase<Assembly>::get_single_item_context_menu();
    menu->addSeparator();

    menu->addAction("Create Assembly...", this, SLOT(slot_create_assembly()));

    return menu;
}

AssemblyItem& AssemblyCollectionItem::get_item(const Assembly& assembly) const
{
    const ItemMap::const_iterator i = m_items.find(assembly.get_uid());
    assert(i != m_items.end());

    return *static_cast<AssemblyItem*>(i->second);
}

void AssemblyCollectionItem::slot_create_assembly()
{
    const string assembly_name_suggestion =
        get_name_suggestion("assembly", m_scene.assemblies());

    const string assembly_name =
        get_entity_name_dialog(
            treeWidget(),
            "Create Assembly",
            "Assembly Name:",
            assembly_name_suggestion);

    if (!assembly_name.empty())
        m_project_builder.insert_assembly(assembly_name);
}

ItemBase* AssemblyCollectionItem::create_item(Assembly* assembly) const
{
    assert(assembly);

    return
        new AssemblyItem(
            assembly,
            m_scene,
            m_project_builder,
            m_settings);
}

}   // namespace studio
}   // namespace appleseed
