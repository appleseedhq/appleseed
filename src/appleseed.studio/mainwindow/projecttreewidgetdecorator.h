
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECTTREEWIDGETDECORATOR_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECTTREEWIDGETDECORATOR_H

// appleseed.studio headers.
#include "mainwindow/projectitem.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <map>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class Entity; }
namespace renderer  { class Scene; }
class QTreeWidget;
class QTreeWidgetItem;

namespace appleseed {
namespace studio {

class ProjectTreeWigetDecorator
{
  public:
    explicit ProjectTreeWigetDecorator(QTreeWidget* tree_widget);

    void rebuild(const renderer::Scene& scene);

    void insert_scene_items(const renderer::Scene& scene);

    void insert_assembly_items(renderer::Assembly& assembly);

    template <typename EntityContainer>
    void insert_scene_items(
        const ProjectItem::Type     item_type,
        EntityContainer&            entities);

    QTreeWidgetItem* insert_scene_item(
        const ProjectItem::Type     item_type,
        renderer::Entity&           entity);

    QTreeWidgetItem* insert_scene_item(
        const ProjectItem&          item);

    template <typename EntityContainer>
    void insert_assembly_items(
        renderer::Assembly&         assembly,
        const ProjectItem::Type     item_type,
        EntityContainer&            entities);

    QTreeWidgetItem* insert_assembly_item(
        renderer::Assembly&         assembly,
        const ProjectItem::Type     item_type,
        renderer::Entity&           entity);

    QTreeWidgetItem* insert_assembly_item(
        renderer::Assembly&         assembly,
        const ProjectItem&          item);

  private:
    typedef std::map<ProjectItem::Type, QTreeWidgetItem*> RootItemCollection;
    typedef std::map<foundation::UniqueID, RootItemCollection> AssemblyRootItemCollection;

    QTreeWidget*                    m_tree_widget;

    RootItemCollection              m_scene_root_items;
    AssemblyRootItemCollection      m_assemblies_root_items;

    void create_scene_root_items();

    void create_assembly_root_items(
        QTreeWidgetItem*            assembly_item,
        renderer::Assembly&         assembly);
};


//
// ProjectTreeWigetDecorator class implementation.
//

template <typename EntityContainer>
void ProjectTreeWigetDecorator::insert_scene_items(
    const ProjectItem::Type         item_type,
    EntityContainer&                entities)
{
    for (foundation::each<EntityContainer> i = entities; i; ++i)
        insert_scene_item(item_type, *i);
}

template <typename EntityContainer>
void ProjectTreeWigetDecorator::insert_assembly_items(
    renderer::Assembly&             assembly,
    const ProjectItem::Type         item_type,
    EntityContainer&                entities)
{
    for (foundation::each<EntityContainer> i = entities; i; ++i)
        insert_assembly_item(assembly, item_type, *i);
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECTTREEWIDGETDECORATOR_H
