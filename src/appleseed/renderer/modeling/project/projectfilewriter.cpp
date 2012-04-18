
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/meshobjectwriter.h"
#include "renderer/modeling/object/object.h"
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
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/math/transform.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/indenter.h"
#include "foundation/utility/string.h"

// boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cstdio>
#include <cstring>
#include <exception>
#include <map>
#include <set>
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
            {
                const string attribute_value = replace_special_xml_characters(i->second);
                fprintf(m_file, " %s=\"%s\"", i->first.c_str(), attribute_value.c_str());
            }

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
            const Project&                      project,
            FILE*                               file,
            const ProjectFileWriter::Options    options)
          : m_options(options)
          , m_file(file)
          , m_indenter(4)
        {
            assert(m_file);

            // Extract the root path of the project.
            m_project_root_path = filesystem::path(project.get_path()).branch_path();

            // Write the file header.
            fprintf(m_file, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");

            // Write an optional header comment.
            if (!(m_options & ProjectFileWriter::OmitHeaderComment))
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
        const ProjectFileWriter::Options        m_options;
        FILE*                                   m_file;
        Indenter                                m_indenter;
        filesystem::path                        m_project_root_path;

        static void copy_file_if_not_exists(
            const filesystem::path& source_path,
            const filesystem::path& dest_path)
        {
            if (!filesystem::exists(dest_path))
            {
                try
                {
                    filesystem::copy_file(source_path, dest_path);
                }
                catch (const std::exception& e)
                {
                    RENDERER_LOG_ERROR(
                        "failed to copy %s to %s: %s.",
                        source_path.native_file_string().c_str(),
                        dest_path.native_file_string().c_str(),
                        e.what());
                }
            }
        }

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
                Element child_element("matrix", m_file, m_indenter);
                child_element.write(true);

                write_vector(
                    transform.get_local_to_parent(),
                    16,
                    4,
                    MatrixFormat);
            }
        }

        // Write a <transform> element with a "time" attribute.
        void write(const Transformd& transform, const double time)
        {
            Element element("transform", m_file, m_indenter);
            element.add_attribute("time", time);
            element.write(true);

            {
                Element child_element("matrix", m_file, m_indenter);
                child_element.write(true);

                write_vector(
                    transform.get_local_to_parent(),
                    16,
                    4,
                    MatrixFormat);
            }
        }

        // Write an array of color values.
        void write(const char* element_name, const ColorValueArray& values)
        {
            Element element(element_name, m_file, m_indenter);
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
            write("values", color_entity.get_values());
            write("alpha", color_entity.get_alpha());
        }

        // Write a <texture> element.
        void write(const Texture& texture)
        {
            Element element("texture", m_file, m_indenter);
            element.add_attribute("name", texture.get_name());
            element.add_attribute("model", texture.get_model());
            element.write(!texture.get_parameters().empty());

            ParamArray params = texture.get_parameters();

            if (params.strings().exist("filename"))
            {
                const filesystem::path source_filepath = params.get<string>("filename");
                const filesystem::path filename = source_filepath.filename();
                const filesystem::path dest_filepath = m_project_root_path / filename;

                params.insert("filename", filename.string());

                copy_file_if_not_exists(source_filepath, dest_filepath);
            }

            write(params);
        }

        // Write a <texture_instance> element.
        void write(
            const TextureInstance&  texture_instance,
            const TextureContainer& textures)
        {
            const Texture* texture = textures.get_by_index(texture_instance.get_texture_index());

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
            Element element("light", m_file, m_indenter);
            element.add_attribute("name", light.get_name());
            element.add_attribute("model", light.get_model());
            element.write(true);
            write(light.get_parameters());
            write(light.get_transform());
        }

        // Write a <camera> element.
        void write(const Camera& camera)
        {
            Element element("camera", m_file, m_indenter);
            element.add_attribute("name", camera.get_name());
            element.add_attribute("model", camera.get_model());
            element.write(true);

            write(camera.get_parameters());

            const TransformSequence& transform_sequence = camera.transform_sequence();

            for (size_t i = 0; i < transform_sequence.size(); ++i)
            {
                double time;
                Transformd transform;
                transform_sequence.get_transform(i, time, transform);
                write(transform, time);
            }
        }

        // Write a collection of <object> elements.
        void write(const ObjectContainer& objects)
        {
            set<string> groups;

            for (size_t i = 0; i < objects.size(); ++i)
            {
                const Object& object = *objects.get_by_index(i);

                if (strcmp(object.get_model(), MeshObjectFactory::get_model()) == 0)
                {
                    const ParamArray& params = object.get_parameters();

                    if (params.strings().exist("__base_object_name"))
                    {
                        // Mesh object group.
                        const string base_object_name = params.get<string>("__base_object_name");
                        if (groups.find(base_object_name) == groups.end())
                        {
                            groups.insert(base_object_name);
                            write_mesh_object_group(base_object_name, params);
                        }
                    }
                    else
                    {
                        // Orphan mesh object.
                        write_orphan_mesh_object(object);
                    }
                }
                else
                {
                    // Non-mesh object.
                    write(object);
                }
            }
        }

        // Object name mapping established by write_orphan_mesh_object().
        typedef map<string, string> ObjectNameMapping;
        ObjectNameMapping m_object_name_mapping;

        // Get the new name of an object, given its old name.
        string translate_object_name(const string& old_name) const
        {
            const ObjectNameMapping::const_iterator i = m_object_name_mapping.find(old_name);
            return i == m_object_name_mapping.end() ? old_name : i->second;
        }

        void write_orphan_mesh_object(const Object& object)
        {
            const string name = object.get_name();
            const string filename = name + ".obj";

            if (!(m_options & ProjectFileWriter::OmitMeshFiles))
            {
                // Write the mesh object to disk.
                const string filepath = (m_project_root_path / filename).file_string();
                MeshObjectWriter::write(
                    static_cast<const MeshObject&>(object),
                    name.c_str(),
                    filepath.c_str());
            }

            // Add a "filename" parameter to the parameters of the object.
            ParamArray params = object.get_parameters();
            params.insert("filename", filename);

            // Write an <object> element.
            write_mesh_object(name, params);

            // Update the object name mapping.
            m_object_name_mapping[name] = name + "." + name;
        }

        void write_mesh_object_group(const string& base_object_name, ParamArray params)
        {
            // Iterate over file paths, convert them to file names, and write mesh files to output directory.
            if (params.strings().exist("filename"))
            {
                // Transform "filename" from a file path to a file name.
                const string filepath = params.get<string>("filename");
                const string filename = filesystem::path(filepath).filename();
                params.insert("filename", filename);

                // Copy the mesh file to the output directory.
                copy_file_if_not_exists(filepath, m_project_root_path / filename);
            }
            else if (params.dictionaries().exist("filename"))
            {
                StringDictionary& filepaths = params.dictionaries().get("filename").strings();
                for (const_each<StringDictionary> i = filepaths; i; ++i)
                {
                    // Transform the value of this key from a file path to a file name.
                    const string key = i->name();
                    const string filepath = i->value<string>();
                    const string filename = filesystem::path(filepath).filename();
                    filepaths.insert(key, filename);

                    // Copy the mesh file to the output directory.
                    copy_file_if_not_exists(filepath, m_project_root_path / filename);
                }
            }

            // Remove hidden parameters.
            params.strings().remove("__base_object_name");

            // Write a single <object> element for this group of mesh objects.
            write_mesh_object(base_object_name, params);
        }

        // Write an <object> element for a mesh object.
        void write_mesh_object(const string& name, const ParamArray& params)
        {
            Element element("object", m_file, m_indenter);
            element.add_attribute("name", name);
            element.add_attribute("model", MeshObjectFactory::get_model());
            element.write(!params.empty());
            write(params);
        }

        // Write an <object> element.
        void write(const Object& object)
        {
            write_entity("object", object);
        }

        // Write an <assign_material> element.
        void write_assign_material(
            const size_t                slot,
            const ObjectInstance::Side  side,
            const string&               material_name)
        {
            Element element("assign_material", m_file, m_indenter);
            element.add_attribute("slot", slot);
            element.add_attribute("side", side == ObjectInstance::FrontSide ? "front" : "back");
            element.add_attribute("material", material_name);
            element.write(false);
        }

        // Write a series of <assign_material> elements.
        void write_assign_materials(
            const ObjectInstance::Side  side,
            const StringArray&          material_names)
        {
            for (size_t i = 0; i < material_names.size(); ++i)
                write_assign_material(i, side, material_names[i]);
        }

        // Write an <object_instance> element.
        void write(
            const ObjectInstance&   object_instance,
            const Assembly&         assembly)
        {
            Element element("object_instance", m_file, m_indenter);
            element.add_attribute("name", object_instance.get_name());
            element.add_attribute("object", translate_object_name(object_instance.get_object().get_name()));
            element.write(true);

            write(object_instance.get_parameters());
            write(object_instance.get_transform());

            // Write the <assign_material> elements.
            write_assign_materials(ObjectInstance::FrontSide, object_instance.get_front_material_names());
            write_assign_materials(ObjectInstance::BackSide, object_instance.get_back_material_names());
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

bool ProjectFileWriter::write(
    const Project&  project,
    const Options   options)
{
    if (!project.has_path())
        return false;

    RENDERER_LOG_INFO("writing project file %s...", project.get_path());

    // Open the file for writing.
    FILE* file = fopen(project.get_path(), "wt");
    if (file == 0)
        return false;

    // Write the project.
    Writer writer(
        project,
        file,
        options);

    // Close the file.
    fclose(file);

    RENDERER_LOG_INFO("wrote project file %s.", project.get_path());

    return true;
}

}   // namespace renderer
