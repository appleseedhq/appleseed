
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
#include "textureitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblycollectionitem.h"
#include "mainwindow/project/assemblyitem.h"
#include "mainwindow/project/basegroupitem.h"
#include "mainwindow/project/instancecollectionitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/texturecollectionitem.h"
#include "mainwindow/project/textureinstanceitem.h"

// appleseed.renderer headers.
#include "renderer/api/scene.h"
#include "renderer/api/texture.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <vector>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

TextureItem::TextureItem(
    Texture*            texture,
    BaseGroup&          parent,
    BaseGroupItem*      parent_item,
    ProjectBuilder&     project_builder)
  : Base(texture, parent, project_builder)
  , m_parent_item(parent_item)
{
}

namespace
{
    vector<UniqueID> collect_texture_instances(
        const TextureInstanceContainer&     texture_instances,
        const UniqueID                      texture_uid)
    {
        vector<UniqueID> collected;

        for (const_each<TextureInstanceContainer> i = texture_instances; i; ++i)
        {
            const Texture* texture = i->find_texture();

            if (texture && texture->get_uid() == texture_uid)
                collected.push_back(i->get_uid());
        }

        return collected;
    }

    void remove_texture_instances(
        BaseGroup&                          base_group,
        BaseGroupItem*                      base_group_item,
        const UniqueID                      texture_uid)
    {
        TextureInstanceContainer& texture_instances = base_group.texture_instances();

        // Collect the texture instances to remove.
        const vector<UniqueID> remove_list =
            collect_texture_instances(texture_instances, texture_uid);

        // Remove texture instances and their corresponding project items.
        for (const_each<vector<UniqueID> > i = remove_list; i; ++i)
        {
            texture_instances.remove(texture_instances.get_by_uid(*i));
            base_group_item->get_texture_instance_collection_item().remove_item(*i);
        }

        // Recurse into child assemblies.
        for (each<AssemblyContainer> i = base_group.assemblies(); i; ++i)
        {
            AssemblyItem* child_item =
                static_cast<AssemblyItem*>(
                    base_group_item->get_assembly_collection_item().get_item(i->get_uid()));
            remove_texture_instances(*i, child_item, texture_uid);
        }
    }
}

void TextureItem::slot_delete()
{
    if (!allows_deletion())
        return;

    const UniqueID texture_uid = m_entity->get_uid();

    // Remove all texture instances and their corresponding project items.
    remove_texture_instances(m_parent, m_parent_item, texture_uid);

    // Remove and delete the texture.
    m_parent.textures().remove(m_parent.textures().get_by_uid(texture_uid));

    // Mark the project as modified.
    m_project_builder.notify_project_modification();

    // Remove and delete the texture item.
    m_parent_item->get_texture_collection_item().remove_item(texture_uid);

    // At this point 'this' no longer exists.
}

}   // namespace studio
}   // namespace appleseed
