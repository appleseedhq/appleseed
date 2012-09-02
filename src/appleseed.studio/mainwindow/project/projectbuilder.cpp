
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "projectbuilder.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblyinstancecollectionitem.h"
#include "mainwindow/project/basegroupitem.h"
#include "mainwindow/project/collectionitem.h"
#include "mainwindow/project/exceptioninvalidentityname.h"
#include "mainwindow/project/objectcollectionitem.h"
#include "mainwindow/project/objectinstancecollectionitem.h"
#include "mainwindow/project/sceneitem.h"
#include "mainwindow/project/texturecollectionitem.h"

// appleseed.renderer headers.
#include "renderer/api/object.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <memory>
#include <vector>

using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

ProjectBuilder::ProjectBuilder(Project& project)
  : m_project(project)
{
}

Project& ProjectBuilder::get_project()
{
    return m_project;
}

const Project& ProjectBuilder::get_project() const
{
    return m_project;
}

void ProjectBuilder::notify_project_modification() const
{
    emit signal_project_modified();
}

void ProjectBuilder::insert_assembly(
    BaseGroup&          parent,
    BaseGroupItem*      parent_item,
    const string&       name) const
{
    auto_release_ptr<Assembly> assembly(
        AssemblyFactory::create(name.c_str(), ParamArray()));

    parent_item->add_item(assembly.get());

    parent.assemblies().insert(assembly);

    notify_project_modification();
}

namespace
{
    vector<UniqueID> collect_assembly_instances(
        const AssemblyInstanceContainer&    assembly_instances,
        const UniqueID                      assembly_uid)
    {
        vector<UniqueID> collected;

        for (const_each<AssemblyInstanceContainer> i = assembly_instances; i; ++i)
        {
            if (i->get_assembly().get_uid() == assembly_uid)
                collected.push_back(i->get_uid());
        }

        return collected;
    }
}

void ProjectBuilder::remove_assembly(
    BaseGroup&          parent,
    BaseGroupItem*      parent_item,
    const UniqueID      assembly_uid) const
{
    AssemblyContainer& assemblies = parent.assemblies();
    AssemblyInstanceContainer& assembly_instances = parent.assembly_instances();

    const vector<UniqueID> remove_list = collect_assembly_instances(assembly_instances, assembly_uid);

    for (const_each<vector<UniqueID> > i = remove_list; i; ++i)
    {
        const UniqueID assembly_instance_uid = *i;

        // Remove the project item corresponding to this assembly instance.
        parent_item->get_assembly_instance_collection_item().remove_item(assembly_instance_uid);

        // Remove this assembly instance.
        assembly_instances.remove(assembly_instance_uid);
    }

    // Remove the project item corresponding to the assembly itself.
    parent_item->get_assembly_collection_item().remove_item(assembly_uid);

    // Remove the assembly itself.
    assemblies.remove(assembly_uid);

    m_project.get_scene()->bump_geometry_version_id();

    notify_project_modification();
}

void ProjectBuilder::insert_assembly_instance(
    BaseGroup&          parent,
    BaseGroupItem*      parent_item,
    const string&       name,
    Assembly&           assembly) const
{
    auto_release_ptr<AssemblyInstance> assembly_instance(
        AssemblyInstanceFactory::create(
            name.c_str(),
            ParamArray(),
            assembly));

    parent_item->get_assembly_instance_collection_item().add_item(assembly_instance.get());

    parent.assembly_instances().insert(assembly_instance);

    m_project.get_scene()->bump_geometry_version_id();

    notify_project_modification();
}

void ProjectBuilder::remove_assembly_instance(
    BaseGroup&          parent,
    BaseGroupItem*      parent_item,
    const UniqueID      assembly_instance_uid) const
{
    parent_item->get_assembly_instance_collection_item().remove_item(assembly_instance_uid);

    parent.assembly_instances().remove(assembly_instance_uid);
    
    m_project.get_scene()->bump_geometry_version_id();

    notify_project_modification();
}

namespace
{
    auto_release_ptr<Texture> create_texture(const string& path)
    {
        const string texture_name =
            filesystem::path(path).replace_extension().filename().string();

        ParamArray texture_params;
        texture_params.insert("filename", path);
        texture_params.insert("color_space", "srgb");

        SearchPaths search_paths;

        return
            auto_release_ptr<Texture>(
                DiskTexture2dFactory().create(
                    texture_name.c_str(),
                    texture_params,
                    search_paths));
    }

    auto_release_ptr<TextureInstance> create_texture_instance(const string& texture_name)
    {
        const string texture_instance_name = texture_name + "_inst";

        ParamArray texture_instance_params;
        texture_instance_params.insert("addressing_mode", "clamp");
        texture_instance_params.insert("filtering_mode", "bilinear");

        return
            auto_release_ptr<TextureInstance>(
                TextureInstanceFactory::create(
                    texture_instance_name.c_str(),
                    texture_instance_params,
                    texture_name.c_str()));
    }
}

void ProjectBuilder::insert_texture(
    BaseGroup&          parent,
    BaseGroupItem*      parent_item,
    const string&       path) const
{
    auto_release_ptr<Texture> texture = create_texture(path);
    auto_release_ptr<TextureInstance> texture_instance = create_texture_instance(texture->get_name());

    parent_item->add_item(texture.get());
    parent_item->add_item(texture_instance.get());

    parent.textures().insert(texture);
    parent.texture_instances().insert(texture_instance);

    notify_project_modification();
}

namespace
{
    vector<UniqueID> collect_texture_instances(
        const TextureContainer&             textures,
        const TextureInstanceContainer&     texture_instances,
        const UniqueID                      texture_uid)
    {
        vector<UniqueID> collected;

        for (const_each<TextureInstanceContainer> i = texture_instances; i; ++i)
        {
            const Texture* texture = textures.get_by_name(i->get_texture_name());

            if (texture && texture->get_uid() == texture_uid)
                collected.push_back(i->get_uid());
        }

        return collected;
    }
}

void ProjectBuilder::remove_texture(
    BaseGroup&          parent,
    BaseGroupItem*      parent_item,
    const UniqueID      texture_uid) const
{
    TextureContainer& textures = parent.textures();
    TextureInstanceContainer& texture_instances = parent.texture_instances();

    const vector<UniqueID> remove_list =
        collect_texture_instances(textures, texture_instances, texture_uid);

    for (const_each<vector<UniqueID> > i = remove_list; i; ++i)
    {
        const UniqueID texture_instance_uid = *i;

        // Remove the project item corresponding to this texture instance.
        parent_item->get_texture_instance_collection_item().remove_item(texture_instance_uid);

        // Remove this texture instance.
        texture_instances.remove(texture_instances.get_by_uid(texture_instance_uid));
    }

    // Remove the project item corresponding to the texture itself.
    parent_item->get_texture_collection_item().remove_item(texture_uid);

    // Remove the texture itself.
    textures.remove(textures.get_by_uid(texture_uid));

    notify_project_modification();
}

void ProjectBuilder::insert_objects(
    Assembly&           parent,
    AssemblyItem*       parent_item,
    const string&       path) const
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

        parent_item->add_item(object);

        parent.objects().insert(auto_release_ptr<Object>(object));

        const string object_instance_name = string(object->get_name()) + "_inst";
        
        auto_release_ptr<ObjectInstance> object_instance(
            ObjectInstanceFactory::create(
                object_instance_name.c_str(),
                ParamArray(),
                *object,
                Transformd::identity(),
                StringArray()));

        parent_item->add_item(object_instance.get());

        parent.object_instances().insert(object_instance);
    }

    if (!mesh_objects.empty())
    {
        parent.bump_version_id();
        notify_project_modification();
    }
}

namespace
{
    vector<UniqueID> collect_object_instances(
        const ObjectInstanceContainer&      object_instances,
        const UniqueID                      object_uid)
    {
        vector<UniqueID> collected;

        for (const_each<ObjectInstanceContainer> i = object_instances; i; ++i)
        {
            if (i->get_object().get_uid() == object_uid)
                collected.push_back(i->get_uid());
        }

        return collected;
    }
}

void ProjectBuilder::remove_object(
    Assembly&           parent,
    AssemblyItem*       parent_item,
    const UniqueID      object_uid) const
{
    ObjectContainer& objects = parent.objects();
    ObjectInstanceContainer& object_instances = parent.object_instances();

    const vector<UniqueID> remove_list = collect_object_instances(object_instances, object_uid);

    for (const_each<vector<UniqueID> > i = remove_list; i; ++i)
    {
        const UniqueID object_instance_uid = *i;

        // Remove the project item corresponding to this object instance.
        parent_item->get_object_instance_collection_item().remove_item(object_instance_uid);

        // Remove this object instance.
        object_instances.remove(object_instances.get_by_uid(object_instance_uid));
    }

    // Remove the project item corresponding to the object itself.
    parent_item->get_object_collection_item().remove_item(object_uid);

    // Remove the object itself.
    objects.remove(objects.get_by_uid(object_uid));

    parent.bump_version_id();

    notify_project_modification();
}

void ProjectBuilder::remove_object_instance(
    Assembly&           parent,
    AssemblyItem*       parent_item,
    const UniqueID      object_instance_uid) const
{
    parent_item->get_object_instance_collection_item().remove_item(object_instance_uid);

    parent.object_instances().remove(
        parent.object_instances().get_by_uid(object_instance_uid));

    parent.bump_version_id();

    notify_project_modification();
}

string ProjectBuilder::get_entity_name(const Dictionary& values)
{
    if (!values.strings().exist(EntityEditorFormFactoryBase::NameParameter))
        throw ExceptionInvalidEntityName();

    const string name = trim_both(
        values.get<string>(EntityEditorFormFactoryBase::NameParameter));

    if (!is_valid_entity_name(name))
        throw ExceptionInvalidEntityName();

    return name;
}

bool ProjectBuilder::is_valid_entity_name(const string& name)
{
    return !name.empty();
}

}   // namespace studio
}   // namespace appleseed
