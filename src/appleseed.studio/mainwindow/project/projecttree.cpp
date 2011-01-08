
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
#include "mainwindow/project/multimodelcollectionitem.h"
#include "mainwindow/project/texturecollectionitem.h"
#include "mainwindow/project/textureinstancecollectionitem.h"

// appleseed.renderer headers.
#include "renderer/api/color.h"
#include "renderer/api/entity.h"
#include "renderer/api/environmentedf.h"
#include "renderer/api/environmentshader.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/texture.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Qt headers.
#include <QTreeWidget>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

ProjectTree::ProjectTree(QTreeWidget* tree_widget)
  : m_tree_widget(tree_widget)
{
}

void ProjectTree::initialize(Project& project, ProjectBuilder& project_builder)
{
    Scene& scene = *project.get_scene();

    m_color_collection_item =
        add_collection_item(
            scene,
            scene.colors(),
            project_builder);

    m_texture_collection_item =
        add_collection_item(
            scene,
            scene.textures(),
            project_builder);

    m_texture_instance_collection_item =
        add_collection_item(
            scene,
            scene.texture_instances(),
            project_builder);

    m_environment_edf_collection_item =
        add_multi_model_collection_item<EnvironmentEDF>(
            scene,
            scene.environment_edfs(),
            project_builder);

    m_environment_shader_collection_item =
        add_multi_model_collection_item<EnvironmentShader>(
            scene,
            scene.environment_shaders(),
            project_builder);

    m_assembly_collection_item =
        add_collection_item(
            scene,
            scene.assemblies(),
            project_builder);

    m_assembly_instance_collection_item =
        add_collection_item(
            scene,
            scene.assembly_instances(),
            project_builder);
}

void ProjectTree::add_item(ColorEntity& color)
{
    m_color_collection_item->add_item(color);
}

void ProjectTree::add_item(Texture& texture)
{
    m_texture_collection_item->add_item(texture);
}

void ProjectTree::add_item(TextureInstance& texture_instance)
{
    m_texture_instance_collection_item->add_item(texture_instance);
}

void ProjectTree::add_item(EnvironmentEDF& environment_edf)
{
    m_environment_edf_collection_item->add_item(environment_edf);
}

void ProjectTree::add_item(EnvironmentShader& environment_shader)
{
    m_environment_shader_collection_item->add_item(environment_shader);
}

void ProjectTree::add_item(Assembly& assembly)
{
    m_assembly_collection_item->add_item(assembly);
}

void ProjectTree::add_item(AssemblyInstance& assembly_instance)
{
    m_assembly_instance_collection_item->add_item(assembly_instance);
}

AssemblyCollectionItem& ProjectTree::get_assembly_collection_item() const
{
    return *m_assembly_collection_item;
}

template <typename EntityContainer>
typename ItemTypeMap<EntityContainer>::T* ProjectTree::add_collection_item(
    Scene&              scene,
    EntityContainer&    entities,
    ProjectBuilder&     project_builder)
{
    typedef ItemTypeMap<EntityContainer>::T ItemType;

    ItemType* item =
        new ItemType(
            scene,
            entities,
            project_builder);

    m_tree_widget->addTopLevelItem(item);

    return item;
}

template <typename Entity, typename EntityContainer>
CollectionItem<Entity, Scene>* ProjectTree::add_multi_model_collection_item(
    Scene&              scene,
    EntityContainer&    entities,
    ProjectBuilder&     project_builder)
{
    CollectionItem<Entity, Scene>* item =
        new MultiModelCollectionItem<Entity, Scene>(
            new_guid(),
            EntityTraits<Entity>::get_human_readable_collection_type_name(),
            scene,
            project_builder);

    item->add_items(entities);

    m_tree_widget->addTopLevelItem(item);

    return item;
}

}   // namespace studio
}   // namespace appleseed
