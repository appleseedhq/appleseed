
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
#include "renderer/api/surfaceshader.h"
#include "renderer/api/texture.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"

// Qt headers.
#include <QFileDialog>
#include <QInputDialog>
#include <QList>
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

void ProjectExplorer::SceneItems::clear()
{
    m_color_items = 0;
    m_texture_items = 0;
    m_texture_instance_items = 0;
    m_environment_edf_items = 0;
    m_environment_shader_items = 0;
    m_assembly_items = 0;
    m_assembly_instance_items = 0;
}

ProjectExplorer::ProjectExplorer(QTreeWidget* tree_widget, Project* project)
  : m_tree_widget(tree_widget)
  , m_project(project)
{
    m_tree_widget->clear();

    m_tree_widget->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(
        m_tree_widget, SIGNAL(customContextMenuRequested(const QPoint&)),
        this, SLOT(slot_context_menu(const QPoint&)));

    build_tree_widget();
}

namespace
{
    enum ItemType
    {
        ItemAssembly,
        ItemAssemblyCollection,
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
        ItemTextureCollection,
        ItemTextureInstance,
        ItemCollection
    };

    template <typename ParentWidget>
    QTreeWidgetItem* insert_item(
        ParentWidget*               parent_widget,
        const char*                 title,
        const ItemType              item_type,
        const void*                 item_data)
    {
        assert(parent_widget);
        assert(title);

        QTreeWidgetItem* item =
            new QTreeWidgetItem(
                parent_widget,
                QStringList() << title);

        item->setData(0, Qt::UserRole, item_type);
        item->setData(1, Qt::UserRole, QVariant::fromValue(item_data));

        return item;
    }

    template <typename ParentWidget>
    QTreeWidgetItem* insert_collection_item(
        ParentWidget*               parent_widget,
        const char*                 title,
        const ItemType              item_type,
        const void*                 item_data)
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

    template <typename ParentWidget>
    QTreeWidgetItem* insert_collection_item(
        ParentWidget*               parent_widget,
        const char*                 title)
    {
        return
            insert_collection_item(
                parent_widget,
                title,
                ItemCollection,
                0);
    }

    template <typename ParentWidget, typename EntityContainer>
    QTreeWidgetItem* insert_items(
        ParentWidget*               parent_widget,
        const char*                 root_title,
        const ItemType              root_item_type,
        const void*                 root_item_data,
        const ItemType              item_type,
        const EntityContainer&      entities)
    {
        QTreeWidgetItem* root_item =
            insert_collection_item(
                parent_widget,
                root_title,
                root_item_type,
                root_item_data);

        for (const_each<EntityContainer> i = entities; i; ++i)
        {
            const Entity& entity = *i;

            QTreeWidgetItem* entity_item =
                insert_item(
                    root_item,
                    entity.get_name(),
                    item_type,
                    &entity);
        }

        return root_item;
    }

    template <typename ParentWidget, typename EntityContainer>
    QTreeWidgetItem* insert_items(
        ParentWidget*               parent_widget,
        const char*                 root_title,
        const ItemType              item_type,
        const EntityContainer&      entities)
    {
        return
            insert_items(
                parent_widget,
                root_title,
                ItemCollection,
                0,
                item_type,
                entities);
    }
}

void ProjectExplorer::build_tree_widget()
{
    if (m_project)
    {
        const Scene& scene = *m_project->get_scene();

        m_scene_items.m_color_items =
            insert_items(
                m_tree_widget,
                "Colors",
                ItemColor,
                scene.colors());

        m_scene_items.m_texture_items =
            insert_items(
                m_tree_widget,
                "Textures",
                ItemTexture,
                scene.textures());

        m_scene_items.m_texture_instance_items =
            insert_items(
                m_tree_widget,
                "Texture Instances",
                ItemTextureInstance,
                scene.texture_instances());

        m_scene_items.m_environment_edf_items =
            insert_items(
                m_tree_widget,
                "Environment EDFs",
                ItemEnvironmentEDF,
                scene.environment_edfs());

        m_scene_items.m_environment_shader_items =
            insert_items(
                m_tree_widget,
                "Environment Shaders",
                ItemEnvironmentShader,
                scene.environment_shaders());

        m_scene_items.m_assembly_items =
            insert_collection_item(
                m_tree_widget,
                "Assemblies",
                ItemAssemblyCollection,
                0);

        for (const_each<AssemblyContainer> i = scene.assemblies(); i; ++i)
            insert_assembly_items(*i);

        m_scene_items.m_assembly_instance_items =
            insert_items(
                m_tree_widget,
                "Assembly Instances",
                ItemAssemblyInstance,
                scene.assembly_instances());
    }
}

void ProjectExplorer::insert_assembly_items(const Assembly& assembly)
{
    AssemblyItems assembly_items;

    assembly_items.m_assembly_item =
        insert_item(
            m_scene_items.m_assembly_items,
            assembly.get_name(),
            ItemAssembly,
            &assembly);

    assembly_items.m_color_items =
        insert_items(
            assembly_items.m_assembly_item,
            "Colors",
            ItemColor,
            assembly.colors());

    assembly_items.m_texture_items =
        insert_items(
            assembly_items.m_assembly_item,
            "Textures",
            ItemTextureCollection,
            &assembly,
            ItemTexture,
            assembly.textures());

    assembly_items.m_texture_instance_items =
        insert_items(
            assembly_items.m_assembly_item,
            "Texture Instances",
            ItemTextureInstance,
            assembly.texture_instances());

    assembly_items.m_bsdf_items =
        insert_items(
            assembly_items.m_assembly_item,
            "BSDFs",
            ItemBSDF,
            assembly.bsdfs());

    assembly_items.m_edf_items =
        insert_items(
            assembly_items.m_assembly_item,
            "EDFs",
            ItemEDF,
            assembly.edfs());

    assembly_items.m_surface_shader_items =
        insert_items(
            assembly_items.m_assembly_item,
            "Surface Shaders",
            ItemSurfaceShader,
            assembly.surface_shaders());

    assembly_items.m_material_items =
        insert_items(
            assembly_items.m_assembly_item,
            "Materials",
            ItemMaterial,
            assembly.materials());

    assembly_items.m_light_items =
        insert_items(
            assembly_items.m_assembly_item,
            "Lights",
            ItemLight,
            assembly.lights());

    assembly_items.m_object_items =
        insert_items(
            assembly_items.m_assembly_item,
            "Objects",
            ItemObject,
            assembly.objects());

    assembly_items.m_object_instance_items =
        insert_items(
            assembly_items.m_assembly_item,
            "Object Instances",
            ItemObjectInstance,
            assembly.object_instances());

    m_assembly_items[assembly.get_uid()] = assembly_items;
}

void ProjectExplorer::insert_objects(
    ObjectContainer&            objects,
    ObjectInstanceContainer&    object_instances,
    QTreeWidgetItem*            object_items,
    QTreeWidgetItem*            object_instance_items,
    const string&               path) const
{
    const string base_object_name = filesystem::path(path).replace_extension().filename();

    const MeshObjectArray mesh_objects =
        MeshObjectReader().read(
            path.c_str(),
            base_object_name.c_str(),
            ParamArray());

    for (size_t i = 0; i < mesh_objects.size(); ++i)
    {
        MeshObject* object = mesh_objects[i];

        object->get_parameters().insert("filename", filesystem::path(path).filename());
        object->get_parameters().insert("__common_base_name", base_object_name);

        insert_item(
            object_items,
            object->get_name(),
            ItemObject,
            object);

        const size_t object_index = objects.insert(auto_release_ptr<Object>(object));

        const string object_instance_name = string(object->get_name()) + "_inst";
        MaterialIndexArray material_indices;
        auto_release_ptr<ObjectInstance> object_instance(
            ObjectInstanceFactory::create(
                object_instance_name.c_str(),
                *object,
                object_index,
                Transformd(Matrix4d::identity()),
                material_indices));

        insert_item(
            object_instance_items,
            object_instance.get()->get_name(),
            ItemObjectInstance,
            object_instance.get());

        object_instances.insert(object_instance);
    }
}

void ProjectExplorer::insert_textures(
    TextureContainer&           textures,
    TextureInstanceContainer&   texture_instances,
    QTreeWidgetItem*            texture_items,
    QTreeWidgetItem*            texture_instance_items,
    const string&               path) const
{
    const string texture_name = filesystem::path(path).replace_extension().filename();

    ParamArray texture_params;
    texture_params.insert("filename", path);
    texture_params.insert("color_space", "srgb");

    SearchPaths search_paths;
    auto_release_ptr<Texture> texture(
        DiskTextureFactory::create(
            texture_name.c_str(),
            texture_params,
            search_paths));

    insert_item(
        texture_items,
        texture.get()->get_name(),
        ItemTexture,
        texture.get());

    const size_t texture_index = textures.insert(texture);

    ParamArray texture_instance_params;
    texture_instance_params.insert("addressing_mode", "clamp");
    texture_instance_params.insert("filtering_mode", "bilinear");

    const string texture_instance_name = texture_name + "_inst";
    auto_release_ptr<TextureInstance> texture_instance(
        TextureInstanceFactory::create(
            texture_instance_name.c_str(),
            texture_instance_params,
            texture_index));

    insert_item(
        texture_instance_items,
        texture_instance.get()->get_name(),
        ItemTextureInstance,
        texture_instance.get());

    texture_instances.insert(texture_instance);
}

QMenu* ProjectExplorer::build_item_context_menu(const QTreeWidgetItem* item) const
{
    const ItemType item_type =
        static_cast<ItemType>(item->data(0, Qt::UserRole).value<int>());

    QMenu* menu = 0;

    switch (item_type)
    {
      case ItemAssembly:
        menu = build_assembly_context_menu();
        break;

      case ItemAssemblyCollection:
        menu = build_assembly_collection_context_menu();
        break;

      case ItemTextureCollection:
        menu = build_texture_collection_context_menu();
        break;
    }

    if (menu)
    {
        for (const_each<QList<QAction*> > i = menu->actions(); i; ++i)
            (*i)->setData(item->data(1, Qt::UserRole));
    }

    return menu;
}

QMenu* ProjectExplorer::build_generic_context_menu() const
{
    QMenu* menu = new QMenu(m_tree_widget);
    menu->addAction("Add Assembly...", this, SLOT(slot_add_assembly()));
    return menu;
}

QMenu* ProjectExplorer::build_assembly_context_menu() const
{
    QMenu* menu = new QMenu(m_tree_widget);
    menu->addAction("Instantiate...", this, SLOT(slot_instantiate_assembly()));
    menu->addAction("Add Objects...", this, SLOT(slot_add_objects_to_assembly()));
    menu->addAction("Add Textures...", this, SLOT(slot_add_textures_to_assembly()));
    return menu;
}

QMenu* ProjectExplorer::build_assembly_collection_context_menu() const
{
    QMenu* menu = new QMenu(m_tree_widget);
    menu->addAction("Add Assembly...", this, SLOT(slot_add_assembly()));
    return menu;
}

QMenu* ProjectExplorer::build_texture_collection_context_menu() const
{
    QMenu* menu = new QMenu(m_tree_widget);
    menu->addAction("Add Textures...", this, SLOT(slot_add_textures_to_assembly()));
    return menu;
}

void ProjectExplorer::slot_context_menu(const QPoint& point)
{
    const QList<QTreeWidgetItem*> selected_items = m_tree_widget->selectedItems();

    QMenu* menu =
        selected_items.isEmpty()
            ? build_generic_context_menu()
            : build_item_context_menu(selected_items.first());

    if (menu)
        menu->exec(m_tree_widget->mapToGlobal(point));
}

namespace
{
    string get_entity_name(
        QWidget*            parent,
        const string&       title,
        const string&       label,
        const string&       text)
    {
        QString result;
        bool ok;

        do
        {
            result =
                QInputDialog::getText(
                    parent,
                    QString::fromStdString(title),
                    QString::fromStdString(label),
                    QLineEdit::Normal,
                    QString::fromStdString(text),
                    &ok);
        } while (ok && result.isEmpty());

        return ok ? result.toStdString() : string();
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

    template <typename T>
    const T& get_data(const QObject* object)
    {
        assert(object);

        const QAction* action = qobject_cast<const QAction*>(object);
        assert(action);

        const void* data = action->data().value<const void*>();
        assert(data);

        return *static_cast<const T*>(data);
    }
}

void ProjectExplorer::slot_add_assembly()
{
    AssemblyContainer& assemblies = m_project->get_scene()->assemblies();

    const string assembly_name_suggestion =
        get_name_suggestion("assembly", assemblies);

    const string assembly_name =
        get_entity_name(
            m_tree_widget,
            "Add Assembly...",
            "Assembly Name:",
            assembly_name_suggestion);

    if (!assembly_name.empty())
    {
        auto_release_ptr<Assembly> assembly(
            AssemblyFactory::create(
                assembly_name.c_str(),
                ParamArray()));

        insert_assembly_items(*assembly.get());

        assemblies.insert(assembly);
    }
}

void ProjectExplorer::slot_instantiate_assembly()
{
    AssemblyInstanceContainer& assembly_instances =
        m_project->get_scene()->assembly_instances();

    const Assembly& assembly = get_data<Assembly>(sender());

    const string instance_name_suggestion =
        get_name_suggestion(
            string(assembly.get_name()) + "_inst",
            assembly_instances);

    const string instance_name =
        get_entity_name(
            m_tree_widget,
            "Instantiate Assembly...",
            "Assembly Instance Name:",
            instance_name_suggestion);

    if (!instance_name.empty())
    {
        auto_release_ptr<AssemblyInstance> assembly_instance(
            AssemblyInstanceFactory::create(
                instance_name.c_str(),
                assembly,
                Transformd(Matrix4d::identity())));

        insert_item(
            m_scene_items.m_assembly_instance_items,
            assembly_instance.get()->get_name(),
            ItemAssemblyInstance,
            assembly_instance.get());

        assembly_instances.insert(assembly_instance);
    }
}

void ProjectExplorer::slot_add_objects_to_assembly()
{
    QFileDialog::Options options;
    QString selected_filter;

    const QStringList filepaths =
        QFileDialog::getOpenFileNames(
            m_tree_widget,
            "Add Objects...",
            "",
            "Geometry Files (*.obj);;All Files (*.*)",
            &selected_filter,
            options);

    const Assembly& assembly = get_data<Assembly>(sender());
    const AssemblyItems& assembly_items = m_assembly_items[assembly.get_uid()];

    for (int i = 0; i < filepaths.size(); ++i)
    {
        insert_objects(
            assembly.objects(),
            assembly.object_instances(),
            assembly_items.m_object_items,
            assembly_items.m_object_instance_items,
            filepaths[i].toStdString());
    }
}

void ProjectExplorer::slot_add_textures_to_assembly()
{
    QFileDialog::Options options;
    QString selected_filter;

    const QStringList filepaths =
        QFileDialog::getOpenFileNames(
            m_tree_widget,
            "Add Textures...",
            "",
            "Texture Files (*.exr);;All Files (*.*)",
            &selected_filter,
            options);

    const Assembly& assembly = get_data<Assembly>(sender());
    const AssemblyItems& assembly_items = m_assembly_items[assembly.get_uid()];

    for (int i = 0; i < filepaths.size(); ++i)
    {
        insert_textures(
            assembly.textures(),
            assembly.texture_instances(),
            assembly_items.m_texture_items,
            assembly_items.m_texture_instance_items,
            filepaths[i].toStdString());
    }
}

}   // namespace studio
}   // namespace appleseed
