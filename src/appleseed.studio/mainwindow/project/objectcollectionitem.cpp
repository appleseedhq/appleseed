
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
#include "objectcollectionitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblyitem.h"
#include "mainwindow/project/itemregistry.h"
#include "mainwindow/project/objectitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/rendering/renderingmanager.h"
#include "utility/interop.h"
#include "utility/settingskeys.h"

// appleseed.renderer headers.
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QDir>
#include <QFileDialog>
#include <QMenu>
#include <QString>
#include <QStringList>

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <cstddef>

using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

namespace
{
    const UniqueID g_class_uid = new_guid();
}

ObjectCollectionItem::ObjectCollectionItem(
    ObjectContainer&    objects,
    Assembly&           parent,
    AssemblyItem*       parent_item,
    ProjectBuilder&     project_builder,
    ParamArray&         settings)
  : CollectionItemBase<Object>(g_class_uid, "Objects", project_builder)
  , m_parent(parent)
  , m_parent_item(parent_item)
  , m_settings(settings)
{
    add_items(objects);
}

QMenu* ObjectCollectionItem::get_single_item_context_menu() const
{
    QMenu* menu = CollectionItemBase<Object>::get_single_item_context_menu();

    menu->addSeparator();
    menu->addAction("Import Objects...", this, SLOT(slot_import_objects()));

    return menu;
}

void ObjectCollectionItem::slot_import_objects()
{
    QFileDialog::Options options;
    QString selected_filter;

    const QStringList filepaths =
        QFileDialog::getOpenFileNames(
            treeWidget(),
            "Import Objects...",
            m_settings.get_path_optional<QString>(SETTINGS_LAST_DIRECTORY),
            "Geometry Files (*.abc; *.binarymesh; *.obj);;All Files (*.*)",
            &selected_filter,
            options);

    if (filepaths.empty())
        return;

    const filesystem::path path(
        QDir::toNativeSeparators(filepaths.first()).toStdString());

    m_settings.insert_path(
        SETTINGS_LAST_DIRECTORY,
        path.parent_path().string());

    if (m_project_builder.get_rendering_manager().is_rendering())
        schedule_import_objects(filepaths);
    else import_objects(filepaths);

}

namespace
{
    class ImportObjectsDelayedAction
      : public RenderingManager::IDelayedAction
    {
      public:
        ImportObjectsDelayedAction(
            ObjectCollectionItem*   parent,
            const QStringList&      filepaths)
          : m_parent(parent)
          , m_filepaths(filepaths)
        {
        }

        virtual void operator()(
            MasterRenderer&         master_renderer,
            Project&                project) APPLESEED_OVERRIDE
        {
            m_parent->import_objects(m_filepaths);
        }

      private:
        ObjectCollectionItem*       m_parent;
        const QStringList           m_filepaths;
    };
}

void ObjectCollectionItem::schedule_import_objects(const QStringList& filepaths)
{
    m_project_builder.get_rendering_manager().push_delayed_action(
        auto_ptr<RenderingManager::IDelayedAction>(
            new ImportObjectsDelayedAction(this, filepaths)));

    m_project_builder.get_rendering_manager().reinitialize_rendering();
}

void ObjectCollectionItem::import_objects(const QStringList& filepaths)
{
    for (int i = 0; i < filepaths.size(); ++i)
        insert_objects(QDir::toNativeSeparators(filepaths[i]).toStdString());
}

void ObjectCollectionItem::insert_objects(const string& path) const
{
    const string base_object_name =
        filesystem::path(path).replace_extension().filename().string();

    ParamArray params;
    params.insert("filename", path);

    SearchPaths search_paths;
    MeshObjectArray mesh_objects;

    if (!MeshObjectReader().read(
            search_paths,
            base_object_name.c_str(),
            params,
            mesh_objects))
        return;

    for (size_t i = 0; i < mesh_objects.size(); ++i)
    {
        MeshObject* object = mesh_objects[i];

        m_parent_item->add_item(object);

        m_parent.objects().insert(auto_release_ptr<Object>(object));

        const string object_instance_name = string(object->get_name()) + "_inst";

        auto_release_ptr<ObjectInstance> object_instance(
            ObjectInstanceFactory::create(
                object_instance_name.c_str(),
                ParamArray(),
                object->get_name(),
                Transformd::identity(),
                StringDictionary()));

        m_parent_item->add_item(object_instance.get());

        m_parent.object_instances().insert(object_instance);
    }

    if (!mesh_objects.empty())
    {
        m_parent.bump_version_id();
        m_project_builder.notify_project_modification();
    }
}

ItemBase* ObjectCollectionItem::create_item(Object* object)
{
    assert(object);

    ItemBase* item =
        new ObjectItem(
            object,
            m_parent,
            m_parent_item,
            m_project_builder);

    m_project_builder.get_item_registry().insert(object->get_uid(), item);

    return item;
}

}   // namespace studio
}   // namespace appleseed
