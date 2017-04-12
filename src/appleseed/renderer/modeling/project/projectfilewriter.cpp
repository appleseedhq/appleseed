
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/display/display.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/curveobject.h"
#include "renderer/modeling/object/curveobjectwriter.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/meshobjectwriter.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/project/assethandler.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/renderlayerrule.h"
#include "renderer/modeling/project/renderlayerrulecontainer.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/proceduralassembly.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/shadergroup/shader.h"
#include "renderer/modeling/shadergroup/shaderconnection.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/modeling/shadergroup/shaderparam.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/math/transform.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/indenter.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/string.h"
#include "foundation/utility/xmlelement.h"
#include "foundation/utility/zip.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <exception>
#include <map>
#include <set>
#include <string>
#include <vector>

using namespace boost;
using namespace foundation;
using namespace std;
namespace bf = boost::filesystem;

namespace renderer
{

//
// ProjectFileWriter class implementation.
//

namespace
{
    // Floating-point formatting settings.
    const char* MatrixFormat     = "%.15f";
    const char* ColorValueFormat = "%.6f";

    class Writer
    {
      public:
        // Constructor.
        Writer(
            const Project&      project,
            const char*         filepath,
            FILE*               file,
            const int           options)
          : m_project_new_root_dir(filesystem::path(filepath).parent_path())
          , m_file(file)
          , m_options(options)
          , m_indenter(4)
        {
        }

        // Write the <project> element.
        void write_project(const Project& project)
        {
            XMLElement element("project", m_file, m_indenter);
            element.add_attribute("format_revision", project.get_format_revision());
            element.write(XMLElement::HasChildElements);

            write_search_paths(project);

            if (project.get_display())
                write(*project.get_display());

            if (project.get_scene())
                write_scene(*project.get_scene());

            write_rules(project);
            write_output(project);
            write_configurations(project);
        }

      private:
        const filesystem::path  m_project_new_root_dir;
        FILE*                   m_file;
        const int               m_options;
        Indenter                m_indenter;

        // Write a vector of scalars.
        template <typename Vec>
        void write_vector(
            const Vec&          v,
            const size_t        size,
            const size_t        columns,
            const char*         fmt)
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
        void write_params(const Dictionary& params)
        {
            write_dictionary(params, m_file, m_indenter);
        }

        // Write a <transform> element.
        template <typename T>
        void write_transform(const Transform<T>& transform)
        {
            if (transform.get_local_to_parent() == Matrix<T, 4, 4>::identity())
                return;

            XMLElement element("transform", m_file, m_indenter);
            element.write(XMLElement::HasChildElements);

            {
                XMLElement child_element("matrix", m_file, m_indenter);
                child_element.write(XMLElement::HasChildElements);

                write_vector(
                    transform.get_local_to_parent(),
                    16,
                    4,
                    MatrixFormat);
            }
        }

        // Write a transform sequence.
        void write_transform_sequence(const TransformSequence& transform_sequence)
        {
            if (transform_sequence.size() == 1)
            {
                float time;
                Transformd transform;
                transform_sequence.get_transform(0, time, transform);
                if (transform.get_local_to_parent() == Matrix4d::identity())
                    return;
            }

            for (size_t i = 0, e = transform_sequence.size(); i < e; ++i)
            {
                float time;
                Transformd transform;
                transform_sequence.get_transform(i, time, transform);

                XMLElement element("transform", m_file, m_indenter);
                element.add_attribute("time", time);
                element.write(XMLElement::HasChildElements);

                {
                    XMLElement child_element("matrix", m_file, m_indenter);
                    child_element.write(XMLElement::HasChildElements);

                    write_vector(
                        transform.get_local_to_parent(),
                        16,
                        4,
                        MatrixFormat);
                }
            }
        }

        // Write an array of color values.
        void write_value_array(const char* element_name, const ColorValueArray& values)
        {
            XMLElement element(element_name, m_file, m_indenter);
            element.write(XMLElement::HasChildElements);
            write_vector(
                values,
                values.size(),
                8,
                ColorValueFormat);
        }

        template <typename Entity>
        void write_entity(const char* element_name, const Entity& entity)
        {
            write_entity(element_name, entity, entity.get_name());
        }

        template <typename Entity>
        void write_entity(const char* element_name, const Entity& entity, const char* entity_name)
        {
            XMLElement element(element_name, m_file, m_indenter);
            element.add_attribute("name", entity_name);
            element.add_attribute("model", entity.get_model());
            element.write(
                !entity.get_parameters().empty()
                    ? XMLElement::HasChildElements
                    : XMLElement::HasNoContent);

            write_params(entity.get_parameters());
        }

        // An alphabetically-sorted vector of mutable entities.
        template <typename Collection>
        class SortedMutableEntityVector
          : public vector<typename Collection::value_type*>
        {
          public:
            explicit SortedMutableEntityVector(Collection& collection)
            {
                Base::reserve(collection.size());

                for (each<Collection> i = collection; i; ++i)
                    Base::push_back(&*i);

                sort(Base::begin(), Base::end(), EntityOrderingPredicate());
            }

          private:
            typedef vector<typename Collection::value_type*> Base;

            struct EntityOrderingPredicate
            {
                bool operator()(const Entity* lhs, const Entity* rhs) const
                {
                    return strcmp(lhs->get_name(), rhs->get_name()) < 0;
                }
            };
        };

        template <typename Collection>
        void write_collection(Collection& collection)
        {
            const SortedMutableEntityVector<Collection> sorted(collection);

            for (const_each<SortedMutableEntityVector<Collection> > i = sorted; i; ++i)
                write(**i);
        }

        // Write an <aov> element.
        void write(const AOV& aov)
        {
            write_entity("aov", aov);
        }

        // Write an <assembly> element.
        void write(const Assembly& assembly)
        {
            XMLElement element("assembly", m_file, m_indenter);
            element.add_attribute("name", assembly.get_name());

            // Don't write the assembly model for normal assemblies
            // to preserve compatibility with older appleseed versions.
            if (strcmp(assembly.get_model(), AssemblyFactory().get_model()) != 0)
                element.add_attribute("model", assembly.get_model());

            // Don't write the content of the assembly if it was
            // generated procedurally.
            if (dynamic_cast<const ProceduralAssembly*>(&assembly))
            {
                element.write(
                    !assembly.get_parameters().empty()
                        ? XMLElement::HasChildElements
                        : XMLElement::HasNoContent);

                write_params(assembly.get_parameters());
                return;
            }

            element.write(
                !assembly.get_parameters().empty() ||
                !assembly.colors().empty() ||
                !assembly.textures().empty() ||
                !assembly.texture_instances().empty() ||
                !assembly.bsdfs().empty() ||
                !assembly.bssrdfs().empty() ||
                !assembly.edfs().empty() ||
                !assembly.shader_groups().empty() ||
                !assembly.surface_shaders().empty() ||
                !assembly.materials().empty() ||
                !assembly.lights().empty() ||
                !assembly.objects().empty() ||
                !assembly.object_instances().empty() ||
                !assembly.assemblies().empty() ||
                !assembly.assembly_instances().empty()
                    ? XMLElement::HasChildElements
                    : XMLElement::HasNoContent);

            write_params(assembly.get_parameters());

            write_collection(assembly.colors());
            write_collection(assembly.textures());
            write_collection(assembly.texture_instances());
            write_collection(assembly.bsdfs());
            write_collection(assembly.bssrdfs());
            write_collection(assembly.edfs());
            write_collection(assembly.shader_groups());
            write_collection(assembly.surface_shaders());
            write_collection(assembly.materials());
            write_collection(assembly.lights());
            write_object_collection(assembly.objects());
            write_collection(assembly.object_instances());
            write_collection(assembly.assemblies());
            write_collection(assembly.assembly_instances());
        }

        // Write an <assembly_instance> element.
        void write(const AssemblyInstance& assembly_instance)
        {
            XMLElement element("assembly_instance", m_file, m_indenter);
            element.add_attribute("name", assembly_instance.get_name());
            element.add_attribute("assembly", assembly_instance.get_assembly_name());
            element.write(XMLElement::HasChildElements);

            write_params(assembly_instance.get_parameters());
            write_transform_sequence(assembly_instance.transform_sequence());
        }

        // Write an <aovs> element.
        void write_aovs(const Frame& frame)
        {
            if (!frame.aovs().empty())
            {
                XMLElement element("aovs", m_file, m_indenter);
                element.write(XMLElement::HasChildElements);

                write_collection(frame.aovs());
            }
        }

        // Write an <assign_material> element.
        void write_assign_material(
            const string&               slot,
            const string&               side,
            const string&               name)
        {
            XMLElement element("assign_material", m_file, m_indenter);
            element.add_attribute("slot", slot);
            element.add_attribute("side", side);
            element.add_attribute("material", name);
            element.write(XMLElement::HasNoContent);
        }

        // Write a series of <assign_material> elements.
        void write_assign_materials(
            const ObjectInstance::Side  side,
            const StringDictionary&     material_mappings)
        {
            const string side_string = side == ObjectInstance::FrontSide ? "front" : "back";

            for (const_each<StringDictionary> i = material_mappings; i; ++i)
                write_assign_material(i->key(), side_string, i->value<string>());
        }

        // Write a <bsdf> element.
        void write(const BSDF& bsdf)
        {
            write_entity("bsdf", bsdf);
        }

        // Write a <bssrdf> element.
        void write(const BSSRDF& bssrdf)
        {
            write_entity("bssrdf", bssrdf);
        }

        // Write a <camera> element.
        void write(const Camera& camera)
        {
            XMLElement element("camera", m_file, m_indenter);
            element.add_attribute("name", camera.get_name());
            element.add_attribute("model", camera.get_model());
            element.write(XMLElement::HasChildElements);

            write_params(camera.get_parameters());
            write_transform_sequence(camera.transform_sequence());
        }

        // Write a <color> element.
        void write(const ColorEntity& color_entity)
        {
            XMLElement element("color", m_file, m_indenter);
            element.add_attribute("name", color_entity.get_name());
            element.write(XMLElement::HasChildElements);

            write_params(color_entity.get_parameters());

            write_value_array("values", color_entity.get_values());
            write_value_array("alpha", color_entity.get_alpha());
        }

        // Write a <configuration> element.
        void write_configuration(const Configuration& configuration)
        {
            XMLElement element("configuration", m_file, m_indenter);
            element.add_attribute("name", configuration.get_name());
            if (configuration.get_base())
                element.add_attribute("base", configuration.get_base()->get_name());
            element.write(
                !configuration.get_parameters().empty()
                    ? XMLElement::HasChildElements
                    : XMLElement::HasNoContent);
            write_params(configuration.get_parameters());
        }

        size_t count_non_base_configurations(const ConfigurationContainer& configurations)
        {
            size_t count = 0;

            for (const_each<ConfigurationContainer> i = configurations; i; ++i)
            {
                if (!BaseConfigurationFactory::is_base_configuration(i->get_name()))
                    ++count;
            }

            return count;
        }

        // Write a <configurations> element.
        void write_configurations(const Project& project)
        {
            XMLElement element("configurations", m_file, m_indenter);
            element.write(
                count_non_base_configurations(project.configurations()) > 0
                    ? XMLElement::HasChildElements
                    : XMLElement::HasNoContent);

            // Write configurations.
            for (const_each<ConfigurationContainer> i = project.configurations(); i; ++i)
            {
                const Configuration& configuration = *i;
                if (!BaseConfigurationFactory::is_base_configuration(configuration.get_name()))
                    write_configuration(configuration);
            }
        }

        // Write a <display> element.
        void write(const Display& display)
        {
            XMLElement element("display", m_file, m_indenter);
            element.add_attribute("name", display.get_name());
            element.write(
                !display.get_parameters().empty()
                    ? XMLElement::HasChildElements
                    : XMLElement::HasNoContent);

            write_params(display.get_parameters());
        }

        // Write an <edf> element.
        void write(const EDF& edf)
        {
            write_entity("edf", edf);
        }

        // Write an <environment> element.
        void write(const Environment& environment)
        {
            write_entity("environment", environment);
        }

        // Write an <environment_edf> element.
        void write(const EnvironmentEDF& env_edf)
        {
            XMLElement element("environment_edf", m_file, m_indenter);
            element.add_attribute("name", env_edf.get_name());
            element.add_attribute("model", env_edf.get_model());
            element.write(XMLElement::HasChildElements);

            write_params(env_edf.get_parameters());
            write_transform_sequence(env_edf.transform_sequence());
        }

        // Write an <environment_shader> element.
        void write(const EnvironmentShader& env_shader)
        {
            write_entity("environment_shader", env_shader);
        }

        // Write a <frame> element.
        void write_frame(const Frame& frame)
        {
            XMLElement element("frame", m_file, m_indenter);
            element.add_attribute("name", frame.get_name());
            element.write(
                !frame.get_parameters().empty() ||
                !frame.aovs().empty()
                    ? XMLElement::HasChildElements
                    : XMLElement::HasNoContent);
            write_params(frame.get_parameters());
            write_aovs(frame);
        }

        // Write a <light> element.
        void write(const Light& light)
        {
            XMLElement element("light", m_file, m_indenter);
            element.add_attribute("name", light.get_name());
            element.add_attribute("model", light.get_model());
            element.write(XMLElement::HasChildElements);

            write_params(light.get_parameters());
            write_transform(light.get_transform());
        }

        // Write a <material> element.
        void write(const Material& material)
        {
            write_entity("material", material);
        }

        // Write a collection of <object> elements.
        void write_object_collection(ObjectContainer& objects)
        {
            const SortedMutableEntityVector<ObjectContainer> sorted(objects);
            set<string> groups;

            for (const_each<SortedMutableEntityVector<ObjectContainer> > i = sorted; i; ++i)
            {
                Object& object = **i;

                if (strcmp(object.get_model(), MeshObjectFactory::get_model()) == 0)
                    write_mesh_object(static_cast<MeshObject&>(object), groups);
                else if (strcmp(object.get_model(), CurveObjectFactory::get_model()) == 0)
                    write_curve_object(static_cast<CurveObject&>(object));
                else write(object);
            }
        }

        // Write a mesh object.
        void write_mesh_object(MeshObject& object, set<string>& groups)
        {
            ParamArray& params = object.get_parameters();

            // If the object is a mesh primitive, do not write geometry to disk.
            if (params.strings().exist("primitive"))
            {
                XMLElement element("object", m_file, m_indenter);
                element.add_attribute("name", object.get_name());
                element.add_attribute("model", MeshObjectFactory::get_model());
                element.write(XMLElement::HasChildElements);
                write_params(params);
                return;
            }

            if (params.strings().exist("__base_object_name"))
            {
                // This object belongs to a group of objects.
                const string group_name = params.get<string>("__base_object_name");
                if (groups.find(group_name) == groups.end())
                {
                    // This is the first time we encounter this group of objects.
                    groups.insert(group_name);

                    // Write the object group.
                    params.strings().remove("__base_object_name");
                    write_entity("object", object, group_name.c_str());
                    params.strings().insert("__base_object_name", group_name);
                }
            }
            else if (params.strings().exist("filename") || params.dictionaries().exist("filename"))
            {
                // This object has a filename parameter.
                write_entity("object", object, object.get_name());
            }
            else
            {
                // This object does not belong to a group and does not have a filename parameter.
                write_orphan_mesh_object(object);
            }
        }

        // Object name mapping established by do_write_orphan_mesh_object().
        typedef map<string, string> ObjectNameMapping;
        ObjectNameMapping m_object_name_mapping;

        // Get the new name of an object, given its old name.
        string translate_object_name(const string& old_name) const
        {
            const ObjectNameMapping::const_iterator i = m_object_name_mapping.find(old_name);
            return i == m_object_name_mapping.end() ? old_name : i->second;
        }

        // Write an <object> element for a mesh object without a filename.
        void write_orphan_mesh_object(const MeshObject& object)
        {
            // Construct the name of the mesh file.
            const string object_name = object.get_name();
            const string filename = object_name + ".binarymesh";

            if (!(m_options & ProjectFileWriter::OmitWritingGeometryFiles))
            {
                // Write the mesh file to disk.
                const string filepath = (m_project_new_root_dir / filename).string();
                MeshObjectWriter::write(object, object_name.c_str(), filepath.c_str());
            }

            // Write the <object> element.
            XMLElement element("object", m_file, m_indenter);
            element.add_attribute("name", object_name);
            element.add_attribute("model", MeshObjectFactory::get_model());
            element.write(XMLElement::HasChildElements);

            // Output a "filename" parameter but don't add it to the object.
            ParamArray params = object.get_parameters();
            params.insert("filename", filename);
            write_params(params);

            // Update the object name mapping.
            m_object_name_mapping[object_name] = object_name + "." + object_name;
        }

        // Write a curve object.
        void write_curve_object(CurveObject& object)
        {
            ParamArray& params = object.get_parameters();

            if (!params.strings().exist("filepath"))
            {
                const string object_name = object.get_name();
                const string filename = object_name + ".txt";

                if (!(m_options & ProjectFileWriter::OmitWritingGeometryFiles))
                {
                    // Write the curve file to disk.
                    const string filepath = (m_project_new_root_dir / filename).string();
                    CurveObjectWriter::write(object, filepath.c_str());
                }

                // Add a file path parameter to the object.
                params.insert("filepath", filename);
            }

            // Write the <object> element.
            write_entity("object", object);
        }

        // Write an <object> element.
        void write(const Object& object)
        {
            write_entity("object", object);
        }

        // Write an <object_instance> element.
        void write(const ObjectInstance& object_instance)
        {
            XMLElement element("object_instance", m_file, m_indenter);
            element.add_attribute("name", object_instance.get_name());
            element.add_attribute("object", translate_object_name(object_instance.get_object_name()));
            element.write(XMLElement::HasChildElements);

            write_params(object_instance.get_parameters());
            write_transform(object_instance.get_transform());

            write_assign_materials(ObjectInstance::FrontSide, object_instance.get_front_material_mappings());
            write_assign_materials(ObjectInstance::BackSide, object_instance.get_back_material_mappings());
        }

        // Write an <output> element.
        void write_output(const Project& project)
        {
            XMLElement element("output", m_file, m_indenter);
            element.write(
                project.get_frame() != 0
                    ? XMLElement::HasChildElements
                    : XMLElement::HasNoContent);

            if (project.get_frame())
                write_frame(*project.get_frame());
        }

        // Write a <rules> element.
        void write_rules(const Project& project)
        {
            if (!project.render_layer_rules().empty())
            {
                XMLElement element("rules", m_file, m_indenter);
                element.write(XMLElement::HasChildElements);

                write_collection(project.render_layer_rules());
            }
        }

        // Write an <render_layer_assignment> element.
        void write(const RenderLayerRule& rule)
        {
            write_entity("render_layer_assignment", rule);
        }

        // Write a <scene> element.
        void write_scene(const Scene& scene)
        {
            XMLElement element("scene", m_file, m_indenter);
            element.write(
                !scene.cameras().empty() ||
                !scene.colors().empty() ||
                !scene.textures().empty() ||
                !scene.texture_instances().empty() ||
                !scene.environment_edfs().empty() ||
                !scene.environment_shaders().empty() ||
                scene.get_environment() != 0 ||
                !scene.shader_groups().empty() ||
                !scene.assemblies().empty() ||
                !scene.assembly_instances().empty()
                    ? XMLElement::HasChildElements
                    : XMLElement::HasNoContent);

            write_params(scene.get_parameters());

            write_collection(scene.cameras());
            write_collection(scene.colors());
            write_collection(scene.textures());
            write_collection(scene.texture_instances());
            write_collection(scene.environment_edfs());
            write_collection(scene.environment_shaders());

            if (scene.get_environment())
                write(*scene.get_environment());

            write_collection(scene.shader_groups());
            write_collection(scene.assemblies());
            write_collection(scene.assembly_instances());
        }

        // Write a <search_path> element.
        void write_search_path(const char* search_path)
        {
            XMLElement element("search_path", m_file, m_indenter);
            element.write(XMLElement::HasChildElements);

            fprintf(m_file, "%s%s\n", m_indenter.c_str(), search_path);
        }

        // Write a <search_paths> element.
        void write_search_paths(const Project& project)
        {
            const SearchPaths& search_paths = project.search_paths();

            if (!search_paths.empty())
            {
                XMLElement element("search_paths", m_file, m_indenter);
                element.write(XMLElement::HasChildElements);

                for (size_t i = 0; i < search_paths.size(); ++i)
                    write_search_path(search_paths[i]);
            }
        }

        // Write a shader's <parameter> element.
        void write(const ShaderParam& param)
        {
            XMLElement element("parameter", m_file, m_indenter);
            element.add_attribute("name", param.get_name());
            element.add_attribute("value", param.get_value_as_string());
            element.write(XMLElement::HasNoContent);
        }

        // Write a <shader> element.
        void write(const Shader& shader)
        {
            XMLElement element("shader", m_file, m_indenter);
            element.add_attribute("type", shader.get_type());
            element.add_attribute("name", shader.get_shader());
            element.add_attribute("layer", shader.get_layer());
            element.write(XMLElement::HasChildElements);

            for (const_each<ShaderParamContainer> i = shader.shader_params(); i; ++i)
                write(*i);
        }

        // Write a <connect_shaders> element.
        void write(const ShaderConnection& connection)
        {
            XMLElement element("connect_shaders", m_file, m_indenter);
            element.add_attribute("src_layer", connection.get_src_layer());
            element.add_attribute("src_param", connection.get_src_param());
            element.add_attribute("dst_layer", connection.get_dst_layer());
            element.add_attribute("dst_param", connection.get_dst_param());
            element.write(XMLElement::HasNoContent);
        }

        // Write a <shader_group> element.
        void write(const ShaderGroup& shader_group)
        {
            XMLElement element("shader_group", m_file, m_indenter);
            element.add_attribute("name", shader_group.get_name());
            element.write(XMLElement::HasChildElements);

            for (const_each<ShaderContainer> i = shader_group.shaders(); i; ++i)
                write(*i);

            for (const_each<ShaderConnectionContainer> i = shader_group.shader_connections(); i; ++i)
                write(*i);
        }

        // Write a <surface_shader> element.
        void write(const SurfaceShader& surface_shader)
        {
            write_entity("surface_shader", surface_shader);
        }

        // Write a <texture> element.
        void write(const Texture& texture)
        {
            write_entity("texture", texture);
        }

        // Write a <texture_instance> element.
        void write(const TextureInstance& texture_instance)
        {
            XMLElement element("texture_instance", m_file, m_indenter);
            element.add_attribute("name", texture_instance.get_name());
            element.add_attribute("texture", texture_instance.get_texture_name());
            element.write(XMLElement::HasChildElements);

            write_params(texture_instance.get_parameters());
            write_transform(texture_instance.get_transform());
        }
    };
}

bool ProjectFileWriter::write(
    const Project&  project,
    const char*     filepath,
    const int       options)
{
    return
        bf::path(filepath).extension() == ".appleseedz"
            ? write_packed_project_file(project, filepath, options)
            : write_plain_project_file(project, filepath, options);
}

bool ProjectFileWriter::write_plain_project_file(
    const Project&  project,
    const char*     filepath,
    const int       options)
{
    RENDERER_LOG_INFO("writing project file %s...", filepath);

    if (!(options & OmitHandlingAssetFiles))
    {
        // Manage references to external asset files.
        const AssetHandler asset_handler(
            project,
            filepath,
            (options & CopyAllAssets) != 0
                ? AssetHandler::CopyAllAssets
                : AssetHandler::CopyRelativeAssetsOnly);
        if (!asset_handler.handle_assets())
        {
            RENDERER_LOG_ERROR("failed to write project file %s.", filepath);
            return false;
        }
    }

    // Open the file for writing.
    FILE* file = fopen(filepath, "wt");
    if (file == 0)
    {
        RENDERER_LOG_ERROR("failed to write project file %s: i/o error.", filepath);
        return false;
    }

    // Write the file header.
    fprintf(file, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");

    // Write an optional header comment.
    if (!(options & ProjectFileWriter::OmitHeaderComment))
    {
        fprintf(
            file,
            "<!-- File generated by %s. -->\n",
            Appleseed::get_synthetic_version_string());
    }

    // Write the project.
    Writer writer(project, filepath, file, options);
    writer.write_project(project);

    // Close the file.
    fclose(file);

    RENDERER_LOG_INFO("wrote project file %s.", filepath);
    return true;
}

bool ProjectFileWriter::write_packed_project_file(
    const Project&  project,
    const char*     filepath,
    const int       options)
{
    const bf::path project_path(filepath);

    const bf::path temp_directory =
        project_path.parent_path() /
        project_path.filename().replace_extension(".unpacked.temp");

    const bf::path temp_project_filepath =
        temp_directory /
        project_path.filename().replace_extension(".appleseed");

    if (!bf::create_directory(temp_directory))
    {
        RENDERER_LOG_ERROR("failed to create directory %s", temp_directory.string().c_str());
        return false;
    }

    bool success = true;

    try
    {
        success =
            write_plain_project_file(
                project,
                temp_project_filepath.string().c_str(),
                options | ProjectFileWriter::CopyAllAssets);

        if (success)
        {
            RENDERER_LOG_INFO("packing project to %s...", filepath);
            zip(filepath, temp_directory.string());
        }
    }
    catch (const std::exception&)
    {
        RENDERER_LOG_ERROR("failed to write project file %s.", filepath);
        success = false;
    }

    if (bf::exists(temp_directory))
        bf::remove_all(temp_directory);

    return success;
}

}   // namespace renderer
