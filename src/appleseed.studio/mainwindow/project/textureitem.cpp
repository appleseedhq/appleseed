
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/instancecollectionitem.h"
#include "mainwindow/project/itemregistry.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/texturecollectionitem.h"
#include "mainwindow/project/textureinstanceitem.h"
#include "mainwindow/rendering/renderingmanager.h"
#include "utility/miscellaneous.h"

// appleseed.renderer headers.
#include "renderer/api/scene.h"
#include "renderer/api/texture.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <memory>
#include <vector>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

TextureItem::TextureItem(
    EntityEditorContext&    editor_context,
    Texture*                texture,
    BaseGroup&              parent,
    TextureCollectionItem*  collection_item,
    BaseGroupItem*          base_group_item)
  : Base(editor_context, texture, parent, collection_item)
  , m_base_group_item(base_group_item)
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
        ItemRegistry&                       item_registry,
        BaseGroup&                          base_group,
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
            delete item_registry.get_item(*i);
            item_registry.remove(*i);
        }

        // Recurse into child assemblies.
        for (each<AssemblyContainer> i = base_group.assemblies(); i; ++i)
            remove_texture_instances(item_registry, *i, texture_uid);
    }
}

void TextureItem::delete_multiple(const QList<ItemBase*>& items)
{
    m_editor_context.m_rendering_manager.schedule_or_execute(
        auto_ptr<RenderingManager::IScheduledAction>(
            new EntityDeletionAction<TextureItem>(
                qlist_static_cast<TextureItem*>(items))));
}

void TextureItem::do_delete()
{
    if (!allows_deletion())
        return;

    const UniqueID texture_uid = m_entity->get_uid();

    // Remove all texture instances and their corresponding project items.
    remove_texture_instances(
        m_editor_context.m_item_registry,
        m_parent,
        texture_uid);

    // Remove and delete the texture.
    m_parent.textures().remove(m_parent.textures().get_by_uid(texture_uid));

    // Mark the project as modified.
    m_editor_context.m_project_builder.notify_project_modification();

    // Remove and delete the texture item.
    ItemBase* texture_item = m_editor_context.m_item_registry.get_item(texture_uid);
    m_editor_context.m_item_registry.remove(texture_uid);
    delete texture_item;

    // At this point 'this' no longer exists.
}

}   // namespace studio
}   // namespace appleseed
