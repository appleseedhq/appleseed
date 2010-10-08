
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
#include "projecttree.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblycollectionprojectitem.h"
#include "mainwindow/project/assemblyinstancecollectionprojectitem.h"
#include "mainwindow/project/colorcollectionprojectitem.h"
#include "mainwindow/project/environmentedfcollectionprojectitem.h"
#include "mainwindow/project/environmentshadercollectionprojectitem.h"
#include "mainwindow/project/texturecollectionprojectitem.h"
#include "mainwindow/project/textureinstancecollectionprojectitem.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"
#include "renderer/api/scene.h"

// Qt headers.
#include <QTreeWidget>

using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

ProjectTree::ProjectTree(
    Project&        project,
    QTreeWidget*    tree_widget)
  : m_project(project)
  , m_tree_widget(tree_widget)
  , m_project_builder(project, *this)
{
    const Scene& scene = *m_project.get_scene();

    m_color_collection_item = add_item(scene.colors());
    m_texture_collection_item = add_item(scene.textures());
    m_texture_instance_collection_item = add_item(scene.texture_instances());
    m_environment_edf_collection_item = add_item(scene.environment_edfs());
    m_environment_shader_collection_item = add_item(scene.environment_shaders());
    m_assembly_collection_item = add_item(scene.assemblies());
    m_assembly_instance_collection_item = add_item(scene.assembly_instances());
}

ColorCollectionProjectItem& ProjectTree::get_color_collection_item() const
{
    return *m_color_collection_item;
}

TextureCollectionProjectItem& ProjectTree::get_texture_collection_item() const
{
    return *m_texture_collection_item;
}

TextureInstanceCollectionProjectItem& ProjectTree::get_texture_instance_collection_item() const
{
    return *m_texture_instance_collection_item;
}

AssemblyCollectionProjectItem& ProjectTree::get_assembly_collection_item() const
{
    return *m_assembly_collection_item;
}

AssemblyInstanceCollectionProjectItem& ProjectTree::get_assembly_instance_collection_item() const
{
    return *m_assembly_instance_collection_item;
}

template <typename EntityContainer>
typename ProjectItemTypeMap<EntityContainer>::T* ProjectTree::add_item(EntityContainer& entities)
{
    typedef ProjectItemTypeMap<EntityContainer>::T ItemType;
    ItemType* item = new ItemType(m_project_builder, entities);
    m_tree_widget->addTopLevelItem(item);
    return item;
}

/*
void ProjectTree::insert_object_instance_items(
    Assembly&         assembly)
{
    for (each<ObjectInstanceContainer> i = assembly.object_instances(); i; ++i)
    {
        QTreeWidgetItem* item =
            insert_assembly_item(assembly, ProjectItem::ItemObjectInstance, *i);

        const MaterialIndexArray& material_indices = i->get_material_indices();

        if (material_indices.empty())
        {
            const QColor SolidPink(255, 0, 255, 255);
            item->setTextColor(0, SolidPink);
        }
    }
}
*/

}   // namespace studio
}   // namespace appleseed
