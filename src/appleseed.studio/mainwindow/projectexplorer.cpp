
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
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"

// Qt headers.
#include <QFileDialog>
#include <QInputDialog>
#include <QMenu>
#include <QMetaType>
#include <QPoint>
#include <QString>
#include <QStringList>
#include <Qt>
#include <QTreeWidget>

// boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>

using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

Q_DECLARE_METATYPE(const void*);

namespace appleseed {
namespace studio {

ProjectExplorer::ProjectExplorer(QTreeWidget* tree_widget, Project* project)
  : m_tree_widget(tree_widget)
  , m_project(project)
{
    m_tree_widget->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(
        m_tree_widget, SIGNAL(customContextMenuRequested(const QPoint&)),
        this, SLOT(slot_context_menu(const QPoint&)));

    update_tree_widget();
}

namespace
{
    enum EntityType
    {
        ItemAssembly,
        ItemAssemblyInstance,
        ItemBSDF,
        ItemCamera,
        ItemColor,
        ItemEDF,
        ItemEnvironment,
        ItemEnvironmentEDF,
        ItemEnvironmentShader,
        ItemMaterial,
        ItemLight,
        ItemObject,
        ItemObjectInstance,
        ItemSurfaceShader,
        ItemTexture,
        ItemTextureInstance
    };

    template <typename ParentWidget>
    QTreeWidgetItem* make_entity_item(
        ParentWidget*               parent_widget,
        const char*                 title,
        const EntityType            entity_type,
        const void*                 entity)
    {
        assert(parent_widget);
        assert(title);

        QTreeWidgetItem* item =
            new QTreeWidgetItem(
                parent_widget,
                QStringList() << title);

        item->setData(0, Qt::UserRole, entity_type);
        item->setData(1, Qt::UserRole, QVariant::fromValue(entity));

        return item;
    }

    template <typename ParentWidget>
    QTreeWidgetItem* make_root_item(
        ParentWidget*               parent_widget,
        const char*                 title,
        const EntityType            entity_type)
    {
        QTreeWidgetItem* root_item =
            make_entity_item(
                parent_widget,
                title,
                entity_type,
                0);

        QFont font;
        font.setBold(true);
        root_item->setFont(0, font);

        return root_item;
    }

    template <typename EntityContainer>
    void insert_entities(
        QTreeWidget*                tree_widget,
        const char*                 root_item_title,
        const EntityType            entity_type,
        const EntityContainer&      entities)
    {
        QTreeWidgetItem* root_item =
            make_root_item(tree_widget, root_item_title, entity_type);

        insert_entities(root_item, entity_type, entities);

        tree_widget->addTopLevelItem(root_item);
    }

    template <typename EntityContainer>
    void insert_entities(
        QTreeWidgetItem*            parent_item,
        const char*                 root_item_title,
        const EntityType            entity_type,
        const EntityContainer&      entities)
    {
        QTreeWidgetItem* root_item =
            make_root_item(parent_item, root_item_title, entity_type);

        insert_entities(root_item, entity_type, entities);

        parent_item->addChild(root_item);
    }

    template <typename EntityContainer>
    void insert_entities(
        QTreeWidgetItem*            parent_item,
        const EntityType            entity_type,
        const EntityContainer&      entities)
    {
        assert(parent_item);

        for (const_each<EntityContainer> i = entities; i; ++i)
        {
            const Entity& entity = *i;

            QTreeWidgetItem* entity_item =
                make_entity_item(
                    parent_item,
                    entity.get_name(),
                    entity_type,
                    &entity);

            parent_item->addChild(entity_item);
        }
    }

    void insert_assemblies(
        QTreeWidget*                tree_widget,
        const AssemblyContainer&    assemblies)
    {
        QTreeWidgetItem* root_item =
            make_root_item(tree_widget, "Assemblies", ItemAssembly);

        tree_widget->addTopLevelItem(root_item);

        for (const_each<AssemblyContainer> i = assemblies; i; ++i)
        {
            const Assembly& assembly = *i;

            QTreeWidgetItem* entity_item =
                make_entity_item(
                    root_item,
                    assembly.get_name(),
                    ItemAssembly,
                    &assembly);

            root_item->addChild(entity_item);

            insert_entities(entity_item, "Colors", ItemColor, i->colors());
            insert_entities(entity_item, "Textures", ItemTexture, i->textures());
            insert_entities(entity_item, "Texture Instances", ItemTextureInstance, i->texture_instances());
            insert_entities(entity_item, "BSDFs", ItemBSDF, i->bsdfs());
            insert_entities(entity_item, "EDFs", ItemEDF, i->edfs());
            insert_entities(entity_item, "Surface Shaders", ItemSurfaceShader, i->surface_shaders());
            insert_entities(entity_item, "Materials", ItemMaterial, i->materials());
            insert_entities(entity_item, "Lights", ItemLight, i->lights());
            insert_entities(entity_item, "Objects", ItemObject, i->objects());
            insert_entities(entity_item, "Object Instances", ItemObjectInstance, i->object_instances());
        }
    }
}

void ProjectExplorer::update_tree_widget()
{
    m_tree_widget->clear();

    if (m_project)
    {
        const Scene& scene = *m_project->get_scene();

        insert_entities(m_tree_widget, "Colors", ItemColor, scene.colors());
        insert_entities(m_tree_widget, "Textures", ItemTexture, scene.textures());
        insert_entities(m_tree_widget, "Texture Instances", ItemTextureInstance, scene.texture_instances());
        insert_entities(m_tree_widget, "Environment EDFs", ItemEnvironmentEDF, scene.environment_edfs());
        insert_entities(m_tree_widget, "Environment Shaders", ItemEnvironmentShader, scene.environment_shaders());
        insert_assemblies(m_tree_widget, scene.assemblies());
        insert_entities(m_tree_widget, "Assembly Instances", ItemAssemblyInstance, scene.assembly_instances());
    }
}

QMenu* ProjectExplorer::build_context_menu(const QList<QTreeWidgetItem*> selected_items) const
{
    if (selected_items.empty())
        return build_generic_context_menu();

    const QTreeWidgetItem* selected_item = selected_items.first();

    const EntityType entity_type =
        static_cast<EntityType>(selected_item->data(0, Qt::UserRole).value<int>());
    
    const void* entity = selected_item->data(1, Qt::UserRole).value<const void*>();

    switch (entity_type)
    {
      case ItemAssembly:
        return build_assembly_context_menu(entity);

      default:
        return 0;
    }
}

QMenu* ProjectExplorer::build_generic_context_menu() const
{
    QMenu* context_menu = new QMenu(m_tree_widget);
    context_menu->addAction("Add Assembly...", this, SLOT(slot_add_assembly()));
    return context_menu;
}

QMenu* ProjectExplorer::build_assembly_context_menu(const void* assembly) const
{
    QMenu* context_menu = new QMenu(m_tree_widget);

    if (assembly)
    {
        context_menu
            ->addAction("Add Objects...", this, SLOT(slot_add_objects()))
            ->setData(QVariant::fromValue(assembly));
    }
    else
    {
        context_menu->addAction("Add Assembly...", this, SLOT(slot_add_assembly()));
    }

    return context_menu;
}

void ProjectExplorer::add_objects(const Assembly& assembly, const string& path) const
{
    const string base_name = filesystem::path(path).replace_extension().filename();

    const MeshObjectArray mesh_objects =
        MeshObjectReader().read(path.c_str(), base_name.c_str(), ParamArray());

    for (size_t i = 0; i < mesh_objects.size(); ++i)
    {
        MeshObject* object = mesh_objects[i];
        const size_t object_index =
            assembly.objects().insert(auto_release_ptr<Object>(object));

        const string object_instance_name = string(object->get_name()) + "_inst";
        MaterialIndexArray material_indices;
        auto_release_ptr<ObjectInstance> object_instance(
            ObjectInstanceFactory::create(
                object_instance_name.c_str(),
                *object,
                object_index,
                Transformd(Matrix4d::identity()),
                material_indices));
        assembly.object_instances().insert(object_instance);
    }
}

void ProjectExplorer::slot_context_menu(const QPoint& point)
{
    assert(m_project);

    const QList<QTreeWidgetItem*> selected_items = m_tree_widget->selectedItems();
    assert(selected_items.size() <= 1);

    QMenu* context_menu = build_context_menu(selected_items);

    if (context_menu)
        context_menu->exec(m_tree_widget->mapToGlobal(point));
}

namespace
{
    QString getNonEmptyText(
        QWidget*            parent,
        const QString&      title,
        const QString&      label,
        QLineEdit::EchoMode echo = QLineEdit::Normal,
        const QString&      text = QString(),
        Qt::WindowFlags     flags = 0)
    {
        QString result;
        bool ok;

        do
        {
            result =
                QInputDialog::getText(
                    parent,
                    title,
                    label,
                    echo,
                    text,
                    &ok,
                    flags);
        } while (ok && result.isEmpty());

        return ok ? result : QString();
    }

    template <typename EntityContainer>
    string get_name_suggestion(
        const string&           prefix,
        const EntityContainer&  entities)
    {
        int max_number = 0;

        for (const_each<EntityContainer> i = entities; i; ++i)
        {
            const Entity& entity = *i;

            const string entity_name = entity.get_name();
            const string entity_name_prefix = entity_name.substr(0, prefix.size());

            if (entity_name_prefix == prefix)
            {
                try
                {
                    const string entity_name_suffix = entity_name.substr(prefix.size());
                    const int number = from_string<int>(entity_name_suffix);

                    if (max_number < number)
                        max_number = number;
                }
                catch (const ExceptionStringConversionError&)
                {
                }
            }
        }

        return prefix + to_string(max_number + 1);
    }

    FOUNDATION_TEST_SUITE(Studio_ProjectExplorer)
    {
        class DummyEntity
          : public Entity
        {
          public:
            explicit DummyEntity(const string& name)
              : m_name(name)
            {
            }

            virtual void release()
            {
                delete this;
            }

            virtual const char* get_name() const
            {
                return m_name.c_str();
            }

          private:
            const string m_name;
        };

        typedef TypedEntityVector<DummyEntity> DummyEntityVector;

        FOUNDATION_TEST_CASE(GetNameSuggestion_GivenZeroEntity_ReturnsNameWithFirstSuffix)
        {
            DummyEntityVector entities;

            const string result = get_name_suggestion("assembly", entities);

            FOUNDATION_EXPECT_EQ("assembly1", result);
        }

        FOUNDATION_TEST_CASE(GetNameSuggestion_GivenTwoEntitiesWithMatchingPrefixes_ReturnsNameWithNextSuffix)
        {
            DummyEntityVector entities;
            entities.insert(auto_release_ptr<DummyEntity>(new DummyEntity("assembly3")));
            entities.insert(auto_release_ptr<DummyEntity>(new DummyEntity("assembly1")));

            const string result = get_name_suggestion("assembly", entities);

            FOUNDATION_EXPECT_EQ("assembly4", result);
        }

        FOUNDATION_TEST_CASE(GetNameSuggestion_GivenEntityWithNegativeSuffix_ReturnsNameWithFirstSuffix)
        {
            DummyEntityVector entities;
            entities.insert(auto_release_ptr<DummyEntity>(new DummyEntity("assembly-5")));

            const string result = get_name_suggestion("assembly", entities);

            FOUNDATION_EXPECT_EQ("assembly1", result);
        }

        FOUNDATION_TEST_CASE(GetNameSuggestion_GivenOneEntityWithNonMatchingPrefix_ReturnsNameWithFirstSuffix)
        {
            DummyEntityVector entities;
            entities.insert(auto_release_ptr<DummyEntity>(new DummyEntity("object")));

            const string result = get_name_suggestion("assembly", entities);

            FOUNDATION_EXPECT_EQ("assembly1", result);
        }

        FOUNDATION_TEST_CASE(GetNameSuggestion_GivenOneEntityWithNonNumericSuffix_ReturnsNameWithFirstSuffix)
        {
            DummyEntityVector entities;
            entities.insert(auto_release_ptr<DummyEntity>(new DummyEntity("assembly_instance")));

            const string result = get_name_suggestion("assembly", entities);

            FOUNDATION_EXPECT_EQ("assembly1", result);
        }
    }

    template <
        typename EntityType,
        typename EntityFactoryType,
        typename EntityContainerType
    >
    bool add_entity(
        QWidget*                parent,
        EntityContainerType&    entities,
        const string&           entity_type_label,
        const string&           entity_name_prefix)
    {
        const QString label = QString::fromStdString(entity_type_label);

        const string entity_name_suggestion =
            get_name_suggestion(entity_name_prefix, entities);

        const QString entity_name =
            getNonEmptyText(
                parent,
                QString("Add %1...").arg(label),
                QString("%1 Name:").arg(label),
                QLineEdit::Normal,
                QString::fromStdString(entity_name_suggestion));

        if (entity_name.isEmpty())
            return false;

        entities.insert(
            auto_release_ptr<EntityType>(
                EntityFactoryType::create(
                    entity_name.toAscii().constData(),
                    ParamArray())));

        return true;
    }

    template <typename T>
    const T& get_entity_from_data(QObject* object)
    {
        const QAction* action = qobject_cast<const QAction*>(object);
        assert(action);

        const void* action_data = action->data().value<const void*>();
        assert(action_data);

        return *static_cast<const T*>(action_data);
    }
}

void ProjectExplorer::slot_add_assembly()
{
    const bool added =
        add_entity<Assembly, AssemblyFactory, AssemblyContainer>(
            m_tree_widget,
            m_project->get_scene()->assemblies(),
            "Assembly",
            "assembly");

    if (added)
        update_tree_widget();
}

void ProjectExplorer::slot_add_objects()
{
    QFileDialog::Options options;
    QString selected_filter;

    QString filepath =
        QFileDialog::getOpenFileName(
            m_tree_widget,
            "Import...",
            "",
            "Geometry Files (*.obj);;All Files (*.*)",
            &selected_filter,
            options);

    if (!filepath.isEmpty())
    {
        add_objects(
            get_entity_from_data<Assembly>(sender()),
            filepath.toStdString());

        update_tree_widget();
    }
}

}   // namespace studio
}   // namespace appleseed
