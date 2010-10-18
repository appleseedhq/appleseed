
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
#include "projectfilewriter.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/geometry/meshobject.h"
#include "renderer/modeling/geometry/meshobjectwriter.h"
#include "renderer/modeling/geometry/object.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/math/transform.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/indenter.h"
#include "foundation/utility/string.h"

// boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cstdio>
#include <cstring>
#include <map>
#include <utility>
#include <vector>

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

//
// ProjectFileWriter class implementation.
//

namespace
{
    //
    // A class representing a XML element.
    //

    class Element
    {
      public:
        // Constructor, opens the element.
        Element(
            const string&   name,
            FILE*           file,
            Indenter&       indenter)
          : m_name(name)
          , m_file(file)
          , m_indenter(indenter)
          , m_opened(false)
          , m_closed(false)
        {
        }

        // Destructor, closes the element.
        ~Element()
        {
            assert(m_opened);
            if (!m_closed)
            {
                --m_indenter;
                fprintf(m_file, "%s</%s>\n", m_indenter.c_str(), m_name.c_str());
            }
        }

        // Append an attribute to the element.
        template <typename T>
        void add_attribute(
            const string&   name,
            const T&        value)
        {
            assert(!m_opened);
            m_attributes.push_back(make_pair(name, to_string(value)));
        }

        // Write the element.
        void write(const bool has_content)
        {
            assert(!m_opened);

            fprintf(m_file, "%s<%s", m_indenter.c_str(), m_name.c_str());

            for (const_each<AttributeVector> i = m_attributes; i; ++i)
                fprintf(m_file, " %s=\"%s\"", i->first.c_str(), i->second.c_str());

            if (has_content)
            {
                fprintf(m_file, ">\n");
                ++m_indenter;
                m_closed = false;
            }
            else
            {
                fprintf(m_file, " />\n");
                m_closed = true;
            }

            m_opened = true;
        }

      private:
        typedef pair<string, string> Attribute;
        typedef vector<Attribute> AttributeVector;

        const string        m_name;
        FILE*               m_file;
        Indenter&           m_indenter;
        AttributeVector     m_attributes;
        bool                m_opened;
        bool                m_closed;
    };


    //
    // The actual project writer.
    //

    // Floating-point formatting settings.
    const char* VectorFormat     = "%.15f";
    const char* MatrixFormat     = "%.15f";
    const char* ColorValueFormat = "%.6f";

    class Writer
    {
      public:
        // Constructor.
        Writer(
            const Project&  project,
            FILE*           file,
            const bool      omit_header_comment)
          : m_file(file)
          , m_indenter(4)
        {
            assert(m_file);

            // Extract the root path of the project.
            m_project_root_path = filesystem::path(project.get_path()).branch_path();

            // Write the file header.
            fprintf(m_file, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");

            // Write an optional header comment.
            if (!omit_header_comment)
            {
                fprintf(
                    m_file,
                    "<!-- File generated by %s. -->\n",
                    Appleseed::get_synthetic_version_string());
            }

            // Write the project.
            write(project);
        }

      private:
        FILE*               m_file;
        Indenter            m_indenter;
        filesystem::path    m_project_root_path;

        // Write a vector of scalars.
        template <typename Vec>
        void write_vector(
            const Vec&      v,
            const size_t    size,
            const size_t    columns,
            const char*     fmt)
        {
            assert(columns > 0);

            for (size_t i = 0; i < size; ++i)
            {
                const size_t col = i % columns;

                if (col == 0)
                    fputs(m_indenter.c_str(), m_file);
                else fputc(' ', m_file);

                fprintf(m_file, fmt, v[i]);

                if (col == columns - 1 || i == size - 1)
                    fputc('\n', m_file);
            }
        }

        // Write a (possibly hierarchical) set of parameters.
        void write(const Dictionary& params)
        {
            for (const_each<StringDictionary> i = params.strings(); i; ++i)
            {
                Element element("parameter", m_file, m_indenter);
                element.add_attribute("name", i->name());
                element.add_attribute("value", i->value<string>());
                element.write(false);
            }

            for (const_each<DictionaryDictionary> i = params.dictionaries(); i; ++i)
            {
                Element element("parameters", m_file, m_indenter);
                element.add_attribute("name", i->name());
                element.write(true);
                write(i->value());
            }
        }

        // Write a <transform> element.
        void write(const Transformd& transform)
        {
            Element element("transform", m_file, m_indenter);
            element.write(true);

            {
                Element element("matrix", m_file, m_indenter);
                element.write(true);

                write_vector(
                    transform.get_local_to_parent(),
                    16,
                    4,
                    MatrixFormat);
            }
        }

        // Write a <values> element.
        void write(const ColorValueArray& values)
        {
            Element element("values", m_file, m_indenter);
            element.write(true);
            write_vector(
                values,
                values.size(),
                8,
                ColorValueFormat);
        }

        template <typename Entity>
        void write_entity(const char* entity_name, const Entity& entity)
        {
            Element element(entity_name, m_file, m_indenter);
            element.add_attribute("name", entity.get_name());
            element.add_attribute("model", entity.get_model());
            element.write(!entity.get_parameters().empty());
            write(entity.get_parameters());
        }

        template <typename T>
        void write(const TypedEntityVector<T>& collection)
        {
            for (const_each<TypedEntityVector<T> > i = collection; i; ++i)
                write(*i);
        }

        template <typename T>
        void write(const TypedEntityMap<T>& collection)
        {
            for (const_each<TypedEntityMap<T> > i = collection; i; ++i)
                write(*i);
        }

        void write(const TextureInstanceContainer& texture_instances, const TextureContainer& textures)
        {
            for (const_each<TextureInstanceContainer> i = texture_instances; i; ++i)
                write(*i, textures);
        }

        void write(const ObjectInstanceContainer& object_instances, const Assembly& assembly)
        {
            for (const_each<ObjectInstanceContainer> i = object_instances; i; ++i)
                write(*i, assembly);
        }

        void write(const AssemblyInstanceContainer& assembly_instances, const Scene& scene)
        {
            for (const_each<AssemblyInstanceContainer> i = assembly_instances; i; ++i)
                write(*i, scene);
        }

        // Write a <color> element.
        void write(const ColorEntity& color_entity)
        {
            Element element("color", m_file, m_indenter);
            element.add_attribute("name", color_entity.get_name());
            element.write(true);
            write(color_entity.get_parameters());
            write(color_entity.get_values());
        }

        // Write a <texture> element.
        void write(const Texture& texture)
        {
            write_entity("texture", texture);
        }

        // Write a <texture_instance> element.
        void write(
            const TextureInstance&  texture_instance,
            const TextureContainer& textures)
        {
            const Texture* texture = textures.get(texture_instance.get_texture_index());

            if (texture == 0)
                return;

            Element element("texture_instance", m_file, m_indenter);
            element.add_attribute("name", texture_instance.get_name());
            element.add_attribute("texture", texture->get_name());
            element.write(!texture_instance.get_parameters().empty());

            write(texture_instance.get_parameters());
        }

        // Write a <bsdf> element.
        void write(const BSDF& bsdf)
        {
            write_entity("bsdf", bsdf);
        }

        // Write an <edf> element.
        void write(const EDF& edf)
        {
            write_entity("edf", edf);
        }

        // Write a <surface_shader> element.
        void write(const SurfaceShader& surface_shader)
        {
            write_entity("surface_shader", surface_shader);
        }

        // Write an <environment> element.
        void write(const Environment& environment)
        {
            write_entity("environment", environment);
        }

        // Write an <environment_edf> element.
        void write(const EnvironmentEDF& env_edf)
        {
            write_entity("environment_edf", env_edf);
        }

        // Write an <environment_shader> element.
        void write(const EnvironmentShader& env_shader)
        {
            write_entity("environment_shader", env_shader);
        }

        // Write a <material> element.
        void write(const Material& material)
        {
            write_entity("material", material);
        }

        // Write a <light> element.
        void write(const Light& light)
        {
            write_entity("light", light);
        }

        // Write a <camera> element.
        void write(const Camera& camera)
        {
            Element element("camera", m_file, m_indenter);
            element.add_attribute("name", camera.get_name());
            element.add_attribute("model", camera.get_model());
            element.write(true);
            write(camera.get_parameters());
            write(camera.get_transform());
        }

        void write(const ObjectContainer& objects)
        {
            FilenameToObjectIndices objects_with_file;
            ObjectIndices objects_without_file;

            classify_mesh_objects(
                objects,
                objects_with_file,
                objects_without_file);

            write_mesh_objects_with_file(objects_with_file, objects);
            write_mesh_objects_without_file(objects_without_file, objects);
        }

        //
        // Classify mesh objects into two groups:
        //
        // - those objects for which a mesh file already exists on disk in the
        //   destination directory;
        //
        // - those objects for which a mesh file doesn't already exist on disk,
        //   either because they were procedurally generated, or the mesh file
        //   is not present in the target directory.
        //

        typedef vector<size_t> ObjectIndices;
        typedef map<string, ObjectIndices> FilenameToObjectIndices;

        void classify_mesh_objects(
            const ObjectContainer&          objects,
            FilenameToObjectIndices&        objects_with_file,
            ObjectIndices&                  objects_without_file)
        {
            for (size_t i = 0; i < objects.size(); ++i)
            {
                const Object* object = objects.get(i);
                assert(object);

                // Only consider mesh objects.
                if (strcmp(object->get_model(), MeshObjectFactory::get_model()))
                    continue;

                const string filename =
                    object->get_parameters().get_optional<string>("filename", "");

                const bool has_file =
                    !filename.empty() && filesystem::exists(m_project_root_path / filename);

                if (has_file)
                    objects_with_file[filename].push_back(i);
                else objects_without_file.push_back(i);
            }
        }

        // Write mesh objects for which a mesh file already exists on disk.
        void write_mesh_objects_with_file(
            const FilenameToObjectIndices&  objects_with_file,
            const ObjectContainer&          objects)
        {
            for (const_each<FilenameToObjectIndices> i = objects_with_file; i; ++i)
            {
                const vector<size_t>& object_indices = i->second;
                assert(!object_indices.empty());

                // Retrieve the common base name of this set of mesh objects.
                ParamArray params = objects.get(object_indices[0])->get_parameters();
                const string common_base_name = params.get<string>("__common_base_name");
                params.strings().remove("__common_base_name");

                // Write a single <object> element for this set of mesh objects.
                write_mesh_object(common_base_name, params);
            }
        }

        // Object name mapping established by write_mesh_objects_without_file().
        typedef map<string, string> ObjectNameMapping;
        ObjectNameMapping m_object_name_mapping;

        // Get the new name of an object, given its old name.
        string translate_object_name(const string& old_name) const
        {
            const ObjectNameMapping::const_iterator i = m_object_name_mapping.find(old_name);
            return i == m_object_name_mapping.end() ? old_name : i->second;
        }

        // Write mesh objects that don't have their geometry already stored on disk.
        void write_mesh_objects_without_file(
            const ObjectIndices&            objects_without_geom,
            const ObjectContainer&          objects)
        {
            for (const_each<ObjectIndices> i = objects_without_geom; i; ++i)
            {
                const Object* object = objects.get(*i);
                assert(object);

                // Generate a filename for this mesh object.
                const string object_name = object->get_name();
                const string filename = object_name + ".obj";

                // Write the mesh object to disk.
                const string file_path = (m_project_root_path / filename).file_string();
                const bool success =
                    MeshObjectWriter::write(
                        static_cast<const MeshObject&>(*object),
                        file_path.c_str());

                // Only keep mesh objects that were successfully written to disk.
                if (success)
                {
                    // Update the object name mapping.
                    m_object_name_mapping[object_name] = object_name + "." + object_name;

                    // Add a "filename" parameter to the parameters of the object.
                    ParamArray params = object->get_parameters();
                    params.insert("filename", filename);

                    // Write an <object> element.
                    write_mesh_object(object->get_name(), params);
                }
            }
        }

        // Write a mesh object.
        void write_mesh_object(const string& name, const ParamArray& params)
        {
            Element element("object", m_file, m_indenter);
            element.add_attribute("name", name);
            element.add_attribute("model", MeshObjectFactory::get_model());
            element.write(!params.empty());
            write(params);
        }

        // Write an <assign_material> element.
        void write_assign_material(
            const size_t            slot,
            const string&           material)
        {
            Element element("assign_material", m_file, m_indenter);
            element.add_attribute("slot", slot);
            element.add_attribute("material", material);
            element.write(false);
        }

        // Write an <object_instance> element.
        void write(
            const ObjectInstance&   object_instance,
            const Assembly&         assembly)
        {
            const Object* object = assembly.objects().get(object_instance.get_object_index());

            if (object == 0)
                return;

            Element element("object_instance", m_file, m_indenter);
            element.add_attribute("name", object_instance.get_name());
            element.add_attribute("object", translate_object_name(object->get_name()));
            element.write(true);

            write(object_instance.get_transform());

            // Write the material indices of the object instance.
            const MaterialIndexArray& material_indices = object_instance.get_material_indices();
            for (size_t i = 0; i < material_indices.size(); ++i)
            {
                const Material* material = assembly.materials().get(material_indices[i]);
                assert(material);
                write_assign_material(i, material->get_name());
            }
        }

        // Write an <assembly> element.
        void write(const Assembly& assembly)
        {
            Element element("assembly", m_file, m_indenter);
            element.add_attribute("name", assembly.get_name());
            element.write(
                !assembly.colors().empty() ||
                !assembly.textures().empty() ||
                !assembly.texture_instances().empty() ||
                !assembly.bsdfs().empty() ||
                !assembly.edfs().empty() ||
                !assembly.surface_shaders().empty() ||
                !assembly.materials().empty() ||
                !assembly.lights().empty() ||
                !assembly.objects().empty() ||
                !assembly.object_instances().empty());

            write(assembly.colors());
            write(assembly.textures());
            write(assembly.texture_instances(), assembly.textures());
            write(assembly.bsdfs());
            write(assembly.edfs());
            write(assembly.surface_shaders());
            write(assembly.materials());
            write(assembly.lights());
            write(assembly.objects());
            write(assembly.object_instances(), assembly);
        }

        // Write an <assembly_instance> element.
        void write(
            const AssemblyInstance& assembly_instance,
            const Scene&            scene)
        {
            Element element("assembly_instance", m_file, m_indenter);
            element.add_attribute("name", assembly_instance.get_name());
            element.add_attribute("assembly", assembly_instance.get_assembly().get_name());
            element.write(true);

            write(assembly_instance.get_transform());
        }

        // Write a <scene> element.
        void write(const Scene& scene)
        {
            Element element("scene", m_file, m_indenter);
            element.write(
                scene.get_camera() != 0 ||
                !scene.colors().empty() ||
                !scene.textures().empty() ||
                !scene.texture_instances().empty() ||
                !scene.environment_edfs().empty() ||
                !scene.environment_shaders().empty() ||
                scene.get_environment() != 0 ||
                !scene.assemblies().empty() ||
                !scene.assembly_instances().empty());

            if (scene.get_camera())
                write(*scene.get_camera());

            write(scene.colors());
            write(scene.textures());
            write(scene.texture_instances(), scene.textures());
            write(scene.environment_edfs());
            write(scene.environment_shaders());

            if (scene.get_environment())
                write(*scene.get_environment());

            write(scene.assemblies());
            write(scene.assembly_instances(), scene);
        }

        // Write a <frame> element.
        void write(const Frame& frame)
        {
            Element element("frame", m_file, m_indenter);
            element.add_attribute("name", frame.get_name());
            element.write(!frame.get_parameters().empty());
            write(frame.get_parameters());
        }

        // Write a <configuration> element.
        void write(const Configuration& configuration)
        {
            Element element("configuration", m_file, m_indenter);
            element.add_attribute("name", configuration.get_name());
            if (configuration.get_base())
                element.add_attribute("base", configuration.get_base()->get_name());
            element.write(!configuration.get_parameters().empty());
            write(configuration.get_parameters());
        }

        // Write a <configurations> element.
        void write_configurations(const Project& project)
        {
            Element element("configurations", m_file, m_indenter);
            element.write(!project.configurations().empty());

            // Write configurations.
            for (const_each<ConfigurationContainer> i = project.configurations(); i; ++i)
            {
                const Configuration& configuration = *i;
                if (!BaseConfigurationFactory::is_base_configuration(configuration.get_name()))
                    write(configuration);
            }
        }

        // Write an <output> element.
        void write_output(const Project& project)
        {
            Element element("output", m_file, m_indenter);
            element.write(project.get_frame() != 0);
            write(*project.get_frame());
        }

        // Write a <project> element.
        void write(const Project& project)
        {
            Element element("project", m_file, m_indenter);
            element.write(true);

            if (project.get_scene())
                write(*project.get_scene());

            write_output(project);
            write_configurations(project);
        }
    };
}

// Write a project to disk.
bool ProjectFileWriter::write(const Project& project, const bool omit_header_comment)
{
    if (!project.has_path())
        return false;

    RENDERER_LOG_INFO("writing project file %s...", project.get_path());

    // Open the file for writing.
    FILE* file = fopen(project.get_path(), "wt");
    if (file == 0)
        return false;

    // Write the project.
    Writer writer(project, file, omit_header_comment);

    // Close the file.
    fclose(file);

    RENDERER_LOG_INFO("wrote project file %s", project.get_path());

    return true;
}

}   // namespace renderer
