
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "basegroupitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblycollectionitem.h"
#include "mainwindow/project/assemblyinstanceitem.h"
#include "mainwindow/project/instancecollectionitem.h"
#include "mainwindow/project/singlemodelcollectionitem.h"
#include "mainwindow/project/texturecollectionitem.h"
#include "mainwindow/project/textureinstanceitem.h"

// appleseed.renderer headers.
#include "renderer/api/color.h"
#include "renderer/api/entity.h"
#include "renderer/api/scene.h"

// Qt headers.
#include <QMenu>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

BaseGroupItem::BaseGroupItem(
    const UniqueID      class_uid,
    BaseGroup&          base_group,
    ProjectBuilder&     project_builder,
    ParamArray&         settings)
  : ItemBase(class_uid)
{
    add_items(base_group, project_builder, settings);
}

BaseGroupItem::BaseGroupItem(
    const UniqueID      class_uid,
    const QString&      title,
    BaseGroup&          base_group,
    ProjectBuilder&     project_builder,
    ParamArray&         settings)
  : ItemBase(class_uid, title)
{
    add_items(base_group, project_builder, settings);
}

QMenu* BaseGroupItem::get_single_item_context_menu() const
{
    QMenu* menu = ItemBase::get_single_item_context_menu();

    menu->addSeparator();
    menu->addAction("Import Textures...", m_texture_collection_item, SLOT(slot_import_textures()));

    menu->addSeparator();
    menu->addAction("Create Assembly...", m_assembly_collection_item, SLOT(slot_create()));

    return menu;
}

void BaseGroupItem::add_item(ColorEntity* color)
{
    m_color_collection_item->add_item(color);
}

void BaseGroupItem::add_item(Texture* texture)
{
    m_texture_collection_item->add_item(texture);
}

void BaseGroupItem::add_item(TextureInstance* texture_instance)
{
    m_texture_instance_collection_item->add_item(texture_instance);
}

void BaseGroupItem::add_item(renderer::Assembly* assembly)
{
    m_assembly_collection_item->add_item(assembly);
}

void BaseGroupItem::add_item(renderer::AssemblyInstance* assembly_instance)
{
    m_assembly_instance_collection_item->add_item(assembly_instance);
}

BaseGroupItem::ColorCollectionItem& BaseGroupItem::get_color_collection_item() const
{
    return *m_color_collection_item;
}

TextureCollectionItem& BaseGroupItem::get_texture_collection_item() const
{
    return *m_texture_collection_item;
}

BaseGroupItem::TextureInstanceCollectionItem& BaseGroupItem::get_texture_instance_collection_item() const
{
    return *m_texture_instance_collection_item;
}

AssemblyCollectionItem& BaseGroupItem::get_assembly_collection_item() const
{
    return *m_assembly_collection_item;
}

BaseGroupItem::AssemblyInstanceCollectionItem& BaseGroupItem::get_assembly_instance_collection_item() const
{
    return *m_assembly_instance_collection_item;
}

void BaseGroupItem::add_items(
    BaseGroup&          base_group,
    ProjectBuilder&     project_builder,
    ParamArray&         settings)
{
    addChild(
        m_color_collection_item =
            new ColorCollectionItem(
                new_guid(),
                EntityTraits<ColorEntity>::get_human_readable_collection_type_name(),
                base_group,
                this,
                project_builder));
    m_color_collection_item->add_items(base_group.colors());

    addChild(
        m_texture_collection_item =
            new TextureCollectionItem(
                base_group.textures(),
                base_group,
                this,
                project_builder,
                settings));

    addChild(
        m_texture_instance_collection_item =
            new TextureInstanceCollectionItem(
                new_guid(),
                EntityTraits<TextureInstance>::get_human_readable_collection_type_name(),
                base_group,
                project_builder));
    m_texture_instance_collection_item->add_items(base_group.texture_instances());

    addChild(
        m_assembly_collection_item =
            new AssemblyCollectionItem(
                base_group.assemblies(),
                base_group,
                this,
                project_builder,
                settings));

    addChild(
        m_assembly_instance_collection_item =
            new AssemblyInstanceCollectionItem(
                new_guid(),
                EntityTraits<AssemblyInstance>::get_human_readable_collection_type_name(),
                base_group,
                project_builder));
    m_assembly_instance_collection_item->add_items(base_group.assembly_instances());
}

}   // namespace studio
}   // namespace appleseed
