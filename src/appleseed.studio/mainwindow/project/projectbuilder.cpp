
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "mainwindow/project/exceptioninvalidentityname.h"
#include "mainwindow/project/texturecollectionitem.h"
#include "mainwindow/project/textureinstancecollectionitem.h"

// appleseed.renderer headers.
#include "renderer/api/object.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/texture.h"

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

using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

ProjectBuilder::ProjectBuilder(
    Project&            project,
    ProjectTree&        project_tree)
  : m_project(project)
  , m_project_tree(project_tree)
{
}

void ProjectBuilder::notify_project_modification() const
{
    emit signal_project_modified();
}

void ProjectBuilder::insert_assembly(
    const string&       name) const
{
    auto_release_ptr<Assembly> assembly(
        AssemblyFactory::create(name.c_str(), ParamArray()));

    m_project_tree.get_assembly_collection_item().add_item(assembly.get());

    m_project.get_scene()->assemblies().insert(assembly);

    notify_project_modification();
}

void ProjectBuilder::insert_assembly_instance(
    const string&       name,
    Assembly&           assembly) const
{
    auto_release_ptr<AssemblyInstance> assembly_instance(
        AssemblyInstanceFactory::create(
            name.c_str(),
            assembly,
            Transformd(Matrix4d::identity())));

    m_project_tree.add_item(assembly_instance.get());

    Scene* scene = m_project.get_scene();
    scene->assembly_instances().insert(assembly_instance);
    scene->bump_geometry_version_id();

    notify_project_modification();
}

void ProjectBuilder::remove_assembly_instance(
    const UniqueID      assembly_instance_id) const
{
    m_project_tree
        .get_assembly_instance_collection_item()
            .remove_item(assembly_instance_id);

    Scene* scene = m_project.get_scene();
    scene->assembly_instances().remove(assembly_instance_id);
    scene->bump_geometry_version_id();

    notify_project_modification();
}

void ProjectBuilder::insert_objects(
    Assembly&           assembly,
    const string&       path) const
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

        m_project_tree.get_assembly_collection_item().get_item(assembly).add_item(object);

        const size_t object_index =
            assembly.objects().insert(auto_release_ptr<Object>(object));

        const string object_instance_name = string(object->get_name()) + "_inst";
        
        auto_release_ptr<ObjectInstance> object_instance(
            ObjectInstanceFactory::create(
                object_instance_name.c_str(),
                *object,
                object_index,
                Transformd(Matrix4d::identity()),
                StringArray()));

        m_project_tree.get_assembly_collection_item().get_item(assembly).add_item(object_instance.get());

        assembly.object_instances().insert(object_instance);
    }

    assembly.bump_version_id();

    if (!mesh_objects.empty())
        notify_project_modification();
}

namespace
{
    auto_release_ptr<Texture> create_texture(
        const string&   path)
    {
        const string texture_name = filesystem::path(path).replace_extension().filename();

        ParamArray texture_params;
        texture_params.insert("filename", path);
        texture_params.insert("color_space", "srgb");

        SearchPaths search_paths;
        return auto_release_ptr<Texture>(
            DiskTexture2dFactory().create(
                texture_name.c_str(),
                texture_params,
                search_paths));
    }

    auto_release_ptr<TextureInstance> create_texture_instance(
        const string&   texture_name,
        const size_t    texture_index)
    {
        const string texture_instance_name = texture_name + "_inst";

        ParamArray texture_instance_params;
        texture_instance_params.insert("addressing_mode", "clamp");
        texture_instance_params.insert("filtering_mode", "bilinear");

        return auto_release_ptr<TextureInstance>(
            TextureInstanceFactory::create(
                texture_instance_name.c_str(),
                texture_instance_params,
                texture_index));
    }
}

void ProjectBuilder::insert_textures(
    Assembly&           assembly,
    const string&       path) const
{
    auto_release_ptr<Texture> texture = create_texture(path);
    const string texture_name = texture->get_name();

    m_project_tree.get_assembly_collection_item().get_item(assembly).add_item(texture.get());

    const size_t texture_index = assembly.textures().insert(texture);

    auto_release_ptr<TextureInstance> texture_instance =
        create_texture_instance(texture_name, texture_index);

    m_project_tree.get_assembly_collection_item().get_item(assembly).add_item(texture_instance.get());

    assembly.texture_instances().insert(texture_instance);

    notify_project_modification();
}

void ProjectBuilder::insert_textures(
    const string&       path) const
{
    const Scene& scene = *m_project.get_scene();

    auto_release_ptr<Texture> texture = create_texture(path);
    const string texture_name = texture->get_name();

    m_project_tree.add_item(texture.get());

    const size_t texture_index = scene.textures().insert(texture);

    auto_release_ptr<TextureInstance> texture_instance =
        create_texture_instance(texture_name, texture_index);

    m_project_tree.add_item(texture_instance.get());

    scene.texture_instances().insert(texture_instance);

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
