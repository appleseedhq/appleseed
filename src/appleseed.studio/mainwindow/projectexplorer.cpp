
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
#include "projectexplorer.h"

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"
#include "renderer/api/color.h"
#include "renderer/api/edf.h"
#include "renderer/api/environmentedf.h"
#include "renderer/api/environmentshader.h"
#include "renderer/api/geometry.h"
#include "renderer/api/light.h"
#include "renderer/api/material.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"
#include "renderer/api/texture.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// Qt headers.
#include <QStringList>
#include <QTreeWidget>

// Standard headers.
#include <cassert>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

ProjectExplorer::ProjectExplorer(QTreeWidget* tree_widget)
  : m_tree_widget(tree_widget)
{
}

void ProjectExplorer::clear()
{
    m_tree_widget->clear();
}

void ProjectExplorer::update(const Project& project)
{
    const Scene* scene = project.get_scene();

    clear();
    update(*scene);
}

namespace
{
    template <typename ParentWidget>
    QTreeWidgetItem* make_category_item(
        ParentWidget*               parent_widget,
        const char*                 category_title)
    {
        assert(parent_widget);
        assert(category_title);

        QTreeWidgetItem* category_item =
            new QTreeWidgetItem(parent_widget, QStringList() << category_title);

        QFont font;
        font.setBold(true);
        category_item->setFont(0, font);

        return category_item;
    }

    template <typename EntityContainer>
    void insert_entities(
        QTreeWidget*                tree_widget,
        const char*                 category_title,
        const EntityContainer&      entities)
    {
        QTreeWidgetItem* root_item =
            make_category_item(tree_widget, category_title);

        insert_entities(root_item, entities);

        tree_widget->addTopLevelItem(root_item);
    }

    template <typename EntityContainer>
    void insert_entities(
        QTreeWidgetItem*            parent_item,
        const char*                 category_title,
        const EntityContainer&      entities)
    {
        QTreeWidgetItem* root_item =
            make_category_item(parent_item, category_title);

        insert_entities(root_item, entities);

        parent_item->addChild(root_item);
    }

    template <typename EntityContainer>
    void insert_entities(
        QTreeWidgetItem*            parent_item,
        const EntityContainer&      entities)
    {
        assert(parent_item);

        for (const_each<EntityContainer> i = entities; i; ++i)
        {
            QTreeWidgetItem* entity_item =
                new QTreeWidgetItem(parent_item, QStringList() << i->get_name());

            parent_item->addChild(entity_item);
        }
    }

    void insert_assemblies(
        QTreeWidget*                tree_widget,
        const char*                 category_title,
        const AssemblyContainer&    assemblies)
    {
        QTreeWidgetItem* root_item =
            make_category_item(tree_widget, category_title);

        tree_widget->addTopLevelItem(root_item);

        for (const_each<AssemblyContainer> i = assemblies; i; ++i)
        {
            QTreeWidgetItem* entity_item =
                new QTreeWidgetItem(root_item, QStringList() << i->get_name());

            root_item->addChild(entity_item);

            insert_entities(entity_item, "Colors", i->colors());
            insert_entities(entity_item, "Textures", i->textures());
            insert_entities(entity_item, "Texture Instances", i->texture_instances());
            insert_entities(entity_item, "BSDFs", i->bsdfs());
            insert_entities(entity_item, "EDFs", i->edfs());
            insert_entities(entity_item, "Surface Shaders", i->surface_shaders());
            insert_entities(entity_item, "Materials", i->materials());
            insert_entities(entity_item, "Lights", i->lights());
            insert_entities(entity_item, "Objects", i->objects());
            insert_entities(entity_item, "Object Instances", i->object_instances());
        }
    }
}

void ProjectExplorer::update(const Scene& scene)
{
    insert_entities(m_tree_widget, "Colors", scene.colors());
    insert_entities(m_tree_widget, "Textures", scene.textures());
    insert_entities(m_tree_widget, "Texture Instances", scene.texture_instances());
    insert_entities(m_tree_widget, "Environment EDFs", scene.environment_edfs());
    insert_entities(m_tree_widget, "Environment Shaders", scene.environment_shaders());
    insert_assemblies(m_tree_widget, "Assemblies", scene.assemblies());
    insert_entities(m_tree_widget, "Assembly Instances", scene.assembly_instances());
}

}   // namespace studio
}   // namespace appleseed
