
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "projecttreewidgetdecorator.h"

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"
#include "renderer/api/color.h"
#include "renderer/api/edf.h"
#include "renderer/api/environmentedf.h"
#include "renderer/api/environmentshader.h"
#include "renderer/api/geometry.h"
#include "renderer/api/light.h"
#include "renderer/api/material.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"
#include "renderer/api/texture.h"

// Qt headers.
#include <QFont>
#include <QMetaType>
#include <QPair>
#include <QStringList>
#include <Qt>
#include <QTreeWidget>
#include <QTreeWidgetItem>
#include <QVariant>

// Standard headers.
#include <cassert>

using namespace foundation;
using namespace renderer;

namespace
{
    typedef QPair<QVariant, QVariant> QVariantPair;
}

Q_DECLARE_METATYPE(QVariantPair);

namespace appleseed {
namespace studio {

ProjectTreeWigetDecorator::ProjectTreeWigetDecorator(QTreeWidget* tree_widget)
  : m_tree_widget(tree_widget)
{
}

void ProjectTreeWigetDecorator::rebuild(const Scene& scene)
{
    create_scene_root_items();
    insert_scene_items(scene);
}

void ProjectTreeWigetDecorator::insert_scene_items(const Scene& scene)
{
    insert_scene_items(ProjectItem::ItemColor, scene.colors());
    insert_scene_items(ProjectItem::ItemTexture, scene.textures());
    insert_scene_items(ProjectItem::ItemTextureInstance, scene.texture_instances());
    insert_scene_items(ProjectItem::ItemEnvironmentEDF, scene.environment_edfs());
    insert_scene_items(ProjectItem::ItemEnvironmentShader, scene.environment_shaders());
    insert_scene_items(ProjectItem::ItemAssemblyInstance, scene.assembly_instances());

    for (each<AssemblyContainer> i = scene.assemblies(); i; ++i)
        insert_assembly_items(*i);
}

void ProjectTreeWigetDecorator::insert_assembly_items(Assembly& assembly)
{
    QTreeWidgetItem* assembly_root_item =
        insert_scene_item(ProjectItem::ItemAssembly, assembly);

    create_assembly_root_items(assembly_root_item, assembly);

    insert_assembly_items(assembly, ProjectItem::ItemColor, assembly.colors());
    insert_assembly_items(assembly, ProjectItem::ItemTexture, assembly.textures());
    insert_assembly_items(assembly, ProjectItem::ItemTextureInstance, assembly.texture_instances());
    insert_assembly_items(assembly, ProjectItem::ItemBSDF, assembly.bsdfs());
    insert_assembly_items(assembly, ProjectItem::ItemEDF, assembly.edfs());
    insert_assembly_items(assembly, ProjectItem::ItemSurfaceShader, assembly.surface_shaders());
    insert_assembly_items(assembly, ProjectItem::ItemMaterial, assembly.materials());
    insert_assembly_items(assembly, ProjectItem::ItemLight, assembly.lights());
    insert_assembly_items(assembly, ProjectItem::ItemObject, assembly.objects());
    insert_assembly_items(assembly, ProjectItem::ItemObjectInstance, assembly.object_instances());
}

namespace
{
    QVariant qvariant_from_ptr(void* ptr)
    {
        return QVariant::fromValue(ptr);
    }

    template <typename ParentWidget>
    QTreeWidgetItem* insert_item(
        ParentWidget*               parent_widget,
        const char*                 title,
        const ProjectItem::Type     item_type,
        const QVariant&             item_data = QVariant())
    {
        assert(parent_widget);
        assert(title);

        QTreeWidgetItem* item =
            new QTreeWidgetItem(parent_widget, QStringList() << title);

        item->setData(0, Qt::UserRole, item_type);
        item->setData(1, Qt::UserRole, item_data);

        return item;
    }

    template <typename ParentWidget>
    QTreeWidgetItem* insert_root_item(
        ParentWidget*               parent_widget,
        const char*                 title,
        const ProjectItem::Type     item_type,
        const QVariant&             item_data = QVariant())
    {
        QTreeWidgetItem* item =
            insert_item(
                parent_widget,
                title,
                item_type,
                item_data);

        QFont font;
        font.setBold(true);
        item->setFont(0, font);

        return item;
    }
}

void ProjectTreeWigetDecorator::create_scene_root_items()
{
    m_scene_root_items[ProjectItem::ItemColor] =
        insert_root_item(
            m_tree_widget,
            "Colors",
            ProjectItem::ItemColorCollection);

    m_scene_root_items[ProjectItem::ItemTexture] =
        insert_root_item(
            m_tree_widget,
            "Textures",
            ProjectItem::ItemTextureCollection);

    m_scene_root_items[ProjectItem::ItemTextureInstance] =
        insert_root_item(
            m_tree_widget,
            "Texture Instances",
            ProjectItem::ItemTextureInstanceCollection);

    m_scene_root_items[ProjectItem::ItemEnvironmentEDF] =
        insert_root_item(
            m_tree_widget,
            "Environment EDFs",
            ProjectItem::ItemEnvironmentEDFCollection);

    m_scene_root_items[ProjectItem::ItemEnvironmentShader] =
        insert_root_item(
            m_tree_widget,
            "Environment Shaders",
            ProjectItem::ItemEnvironmentShaderCollection);

    m_scene_root_items[ProjectItem::ItemAssembly] =
        insert_root_item(
            m_tree_widget,
            "Assemblies",
            ProjectItem::ItemAssemblyCollection);

    m_scene_root_items[ProjectItem::ItemAssemblyInstance] =
        insert_root_item(
            m_tree_widget,
            "Assembly Instances",
            ProjectItem::ItemAssemblyInstanceCollection);
}

void ProjectTreeWigetDecorator::create_assembly_root_items(QTreeWidgetItem* assembly_item, Assembly& assembly)
{
    RootItemCollection& assembly_root_items =
        m_assemblies_root_items[assembly.get_uid()];

    const QVariant item_data = qvariant_from_ptr(&assembly);

    assembly_root_items[ProjectItem::ItemColor] =
        insert_root_item(
            assembly_item,
            "Colors",
            ProjectItem::ItemColorCollection,
            item_data);

    assembly_root_items[ProjectItem::ItemTexture] =
        insert_root_item(
            assembly_item,
            "Textures",
            ProjectItem::ItemTextureCollection,
            item_data);

    assembly_root_items[ProjectItem::ItemTextureInstance] =
        insert_root_item(
            assembly_item,
            "Texture Instances",
            ProjectItem::ItemTextureInstanceCollection,
            item_data);

    assembly_root_items[ProjectItem::ItemBSDF] =
        insert_root_item(
            assembly_item,
            "BSDFs",
            ProjectItem::ItemBSDFCollection,
            item_data);

    assembly_root_items[ProjectItem::ItemEDF] =
        insert_root_item(
            assembly_item,
            "EDFs",
            ProjectItem::ItemEDFCollection,
            item_data);

    assembly_root_items[ProjectItem::ItemSurfaceShader] =
        insert_root_item(
            assembly_item,
            "Surface Shaders",
            ProjectItem::ItemSurfaceShaderCollection,
            item_data);

    assembly_root_items[ProjectItem::ItemMaterial] =
        insert_root_item(
            assembly_item,
            "Materials",
            ProjectItem::ItemMaterialCollection,
            item_data);

    assembly_root_items[ProjectItem::ItemLight] =
        insert_root_item(
            assembly_item,
            "Lights",
            ProjectItem::ItemLightCollection,
            item_data);

    assembly_root_items[ProjectItem::ItemObject] =
        insert_root_item(
            assembly_item,
            "Objects",
            ProjectItem::ItemObjectCollection,
            item_data);

    assembly_root_items[ProjectItem::ItemObjectInstance] =
        insert_root_item(
            assembly_item,
            "Object Instances",
            ProjectItem::ItemObjectInstanceCollection,
            item_data);
}

QTreeWidgetItem* ProjectTreeWigetDecorator::insert_scene_item(
    const ProjectItem::Type     item_type,
    Entity&                     entity)
{
    QTreeWidgetItem* root_item = m_scene_root_items[item_type];

    const QVariant item_data = qvariant_from_ptr(&entity);

    return
        insert_item(
            root_item,
            entity.get_name(),
            item_type,
            item_data);
}

QTreeWidgetItem* ProjectTreeWigetDecorator::insert_scene_item(
    const ProjectItem&          item)
{
    return
        insert_scene_item(
            item.get_type(),
            *item.get_entity());
}

QTreeWidgetItem* ProjectTreeWigetDecorator::insert_assembly_item(
    Assembly&                   assembly,
    const ProjectItem::Type     item_type,
    Entity&                     entity)
{
    RootItemCollection& assembly_root_items =
        m_assemblies_root_items[assembly.get_uid()];

    QTreeWidgetItem* root_item = assembly_root_items[item_type];

    const QVariant item_data =
        QVariant::fromValue(
            qMakePair(
                qvariant_from_ptr(&assembly),
                qvariant_from_ptr(&entity)));

    return
        insert_item(
            root_item,
            entity.get_name(),
            item_type,
            item_data);
}

QTreeWidgetItem* ProjectTreeWigetDecorator::insert_assembly_item(
    Assembly&                   assembly,
    const ProjectItem&          item)
{
    return
        insert_assembly_item(
            assembly,
            item.get_type(),
            *item.get_entity());
}

}   // namespace studio
}   // namespace appleseed
