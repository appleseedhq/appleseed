
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
#include "mainwindow/project/assemblycollectionitem.h"
#include "mainwindow/project/assemblyinstancecollectionitem.h"
#include "mainwindow/project/colorcollectionitem.h"
#include "mainwindow/project/environmentedfcollectionitem.h"
#include "mainwindow/project/environmentshadercollectionitem.h"
#include "mainwindow/project/texturecollectionitem.h"
#include "mainwindow/project/textureinstancecollectionitem.h"

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
    connect(
        &m_project_builder, SIGNAL(project_modified()),
        this, SIGNAL(project_modified()));

    const Scene& scene = *m_project.get_scene();

    m_color_collection_item = add_collection_item(scene.colors());
    m_texture_collection_item = add_collection_item(scene.textures());
    m_texture_instance_collection_item = add_collection_item(scene.texture_instances());
    m_environment_edf_collection_item = add_collection_item(scene.environment_edfs());
    m_environment_shader_collection_item = add_collection_item(scene.environment_shaders());
    m_assembly_collection_item = add_collection_item(scene.assemblies());
    m_assembly_instance_collection_item = add_collection_item(scene.assembly_instances());
}

ColorCollectionItem& ProjectTree::get_color_collection_item() const
{
    return *m_color_collection_item;
}

TextureCollectionItem& ProjectTree::get_texture_collection_item() const
{
    return *m_texture_collection_item;
}

TextureInstanceCollectionItem& ProjectTree::get_texture_instance_collection_item() const
{
    return *m_texture_instance_collection_item;
}

EnvironmentEDFCollectionItem& ProjectTree::get_environment_edf_collection_item() const
{
    return *m_environment_edf_collection_item;
}

AssemblyCollectionItem& ProjectTree::get_assembly_collection_item() const
{
    return *m_assembly_collection_item;
}

AssemblyInstanceCollectionItem& ProjectTree::get_assembly_instance_collection_item() const
{
    return *m_assembly_instance_collection_item;
}

template <typename EntityContainer>
typename ItemTypeMap<EntityContainer>::T* ProjectTree::add_collection_item(EntityContainer& entities)
{
    Scene& scene = *m_project.get_scene();

    typedef ItemTypeMap<EntityContainer>::T ItemType;

    ItemType* item = new ItemType(scene, entities, m_project_builder);
    m_tree_widget->addTopLevelItem(item);

    return item;
}

}   // namespace studio
}   // namespace appleseed
