
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.python headers.
#include "bindentitycontainers.h"
#include "dict2dict.h"

// appleseed.renderer headers.
#include "renderer/api/aov.h"
#include "renderer/api/bsdf.h"
#include "renderer/api/bssrdf.h"
#include "renderer/api/camera.h"
#include "renderer/api/display.h"
#include "renderer/api/edf.h"
#include "renderer/api/environmentedf.h"
#include "renderer/api/environmentshader.h"
#include "renderer/api/frame.h"
#include "renderer/api/light.h"
#include "renderer/api/material.h"
#include "renderer/api/object.h"
#include "renderer/api/postprocessing.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"
#include "renderer/api/texture.h"
#include "renderer/api/volume.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    auto_release_ptr<Project> create_project(const std::string& name)
    {
        return ProjectFactory::create(name.c_str());
    }

    bpy::object create_default_project()
    {
        auto_release_ptr<Project> project(DefaultProjectFactory::create());
        return bpy::object(project);
    }

    bpy::object create_cornell_box_project()
    {
        auto_release_ptr<Project> project(CornellBoxProjectFactory::create());
        return bpy::object(project);
    }

    bpy::object wrap_cpp_project_pointer(const bpy::long_& ptr)
    {
        const uintptr_t x = bpy::extract<uintptr_t>(ptr);
        return bpy::object(bpy::ptr(binary_cast<Project*>(x)));
    }

    bpy::list project_get_search_paths(const Project* project)
    {
        bpy::list paths;

        const SearchPaths& search_paths = project->search_paths();

        for (size_t i = 0; i < search_paths.get_explicit_path_count(); ++i)
            paths.append(search_paths.get_explicit_path(i));

        return paths;
    }

    void project_set_search_paths(Project* project, const bpy::list& paths)
    {
        project->search_paths().clear_explicit_paths();

        for (bpy::ssize_t i = 0, e = bpy::len(paths); i < e; ++i)
        {
            const bpy::extract<const char*> extractor(paths[i]);
            if (extractor.check())
                project->search_paths().push_back_explicit_path(extractor());
            else
            {
                PyErr_SetString(PyExc_TypeError, "Incompatible type. Only strings accepted.");
                bpy::throw_error_already_set();
            }
        }
    }

    ConfigurationContainer* project_get_configs(Project* project)
    {
        return &(project->configurations());
    }

    bool write_project_default_opts(
        const ProjectFileWriter*            writer,
        Project*                            project,
        const char*                         filepath)
    {
        return ProjectFileWriter::write(*project, filepath);
    }

    bool write_project_with_opts(
        const ProjectFileWriter*            writer,
        Project*                            project,
        const char*                         filepath,
        int                                 opts)
    {
        return ProjectFileWriter::write(*project, filepath, opts);
    }

    bool write_project_with_opts_and_comments(
        const ProjectFileWriter*            writer,
        Project*                            project,
        const char*                         filepath,
        int                                 opts,
        const char*                         extra_comments)
    {
        return ProjectFileWriter::write(*project, filepath, opts, extra_comments);
    }

    auto_release_ptr<Configuration> create_config(const std::string& name)
    {
        return ConfigurationFactory::create(name.c_str());
    }

    auto_release_ptr<Configuration> create_config_with_params(
        const std::string&                  name,
        const bpy::dict&                    params)
    {
        return ConfigurationFactory::create(name.c_str(), bpy_dict_to_param_array(params));
    }

    bpy::object create_base_final_config()
    {
        auto_release_ptr<Configuration> config(BaseConfigurationFactory::create_base_final());
        return bpy::object(config);
    }

    bpy::object create_base_interactive_config()
    {
        auto_release_ptr<Configuration> config(BaseConfigurationFactory::create_base_interactive());
        return bpy::object(config);
    }

    bpy::dict config_get_inherited_parameters(const Configuration* config)
    {
        ParamArray params(config->get_inherited_parameters());
        return param_array_to_bpy_dict(params);
    }

    void config_insert_path(Configuration* config, const char* path, const bpy::object& value)
    {
        {
            bpy::extract<const char*> extractor(value);
            if (extractor.check())
            {
                config->get_parameters().insert_path(path, extractor());
                return;
            }
        }

        if (PyBool_Check(value.ptr()))
        {
            bpy::extract<bool> extractor(value);
            if (extractor.check())
            {
                config->get_parameters().insert_path(path, extractor());
                return;
            }
        }

#if PY_MAJOR_VERSION == 2
        if (PyInt_Check(value.ptr()))
        {
            bpy::extract<int> extractor(value);
            if (extractor.check())
            {
                config->get_parameters().insert_path(path, extractor());
                return;
            }
        }
#endif

        if (PyLong_Check(value.ptr()))
        {
            bpy::extract<int> extractor(value);
            if (extractor.check())
            {
                config->get_parameters().insert_path(path, extractor());
                return;
            }
        }

        if (PyFloat_Check(value.ptr()))
        {
            bpy::extract<double> extractor(value);
            if (extractor.check())
            {
                config->get_parameters().insert_path(path, extractor());
                return;
            }
        }

        PyErr_SetString(PyExc_TypeError, "Unsupported value type.");
        bpy::throw_error_already_set();
    }

    void config_remove_path(Configuration* config, const char* path)
    {
        config->get_parameters().remove_path(path);
    }

    bpy::dict config_get_metadata()
    {
        return dictionary_to_bpy_dict(Configuration::get_metadata());
    }

    bpy::object project_file_reader_read_default_opts(
        ProjectFileReader*                  reader,
        const char*                         project_filename,
        const char*                         schema_filename)
    {
        auto_release_ptr<Project> project(reader->read(project_filename, schema_filename));
        return bpy::object(project);
    }

    bpy::object project_file_reader_read_with_opts(
        ProjectFileReader*                  reader,
        const char*                         project_filename,
        const char*                         schema_filename,
        const ProjectFileReader::Options    opts)
    {
        auto_release_ptr<Project> project(reader->read(project_filename, schema_filename, opts));
        return bpy::object(project);
    }

    bpy::object project_file_reader_load_builtin(ProjectFileReader* reader, const char* project_name)
    {
        auto_release_ptr<Project> project(reader->load_builtin(project_name));
        return bpy::object(project);
    }

    std::string qualify_path(const Project* project, const char* filepath)
    {
        const SearchPaths& search_paths = project->search_paths();
        return std::string(search_paths.qualify(filepath).c_str());
    }
}

void bind_project()
{
    bpy::class_<Configuration, auto_release_ptr<Configuration>, bpy::bases<Entity>, boost::noncopyable>("Configuration", bpy::no_init)
        .def("create_base_final", create_base_final_config).staticmethod("create_base_final")
        .def("create_base_interactive", create_base_interactive_config).staticmethod("create_base_interactive")

        .def("__init__", bpy::make_constructor(create_config))
        .def("__init__", bpy::make_constructor(create_config_with_params))

        .def("set_base", &Configuration::set_base)
        .def("get_base", &Configuration::get_base, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_inherited_parameters", config_get_inherited_parameters)

        .def("insert_path", config_insert_path)
        .def("remove_path", config_remove_path)

        .def("get_metadata", config_get_metadata).staticmethod("get_metadata");

    bind_typed_entity_map<Configuration>("ConfigurationContainer");

    bpy::class_<Project, auto_release_ptr<Project>, bpy::bases<Entity>, boost::noncopyable>("Project", bpy::no_init)
        .def("create_default", create_default_project).staticmethod("create_default")
        .def("create_cornell_box", create_cornell_box_project).staticmethod("create_cornell_box")

        .def("__init__", bpy::make_constructor(create_project))

        .def("add_default_configurations", &Project::add_default_configurations)
        .def("configurations", project_get_configs, bpy::return_value_policy<bpy::reference_existing_object>())

        .def("get_aov_factory_registrar", &Project::get_factory_registrar<AOV>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_assembly_factory_registrar", &Project::get_factory_registrar<Assembly>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_bsdf_factory_registrar", &Project::get_factory_registrar<BSDF>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_bssrdf_factory_registrar", &Project::get_factory_registrar<BSSRDF>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_camera_factory_registrar", &Project::get_factory_registrar<Camera>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_edf_factory_registrar", &Project::get_factory_registrar<EDF>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_environment_edf_factory_registrar", &Project::get_factory_registrar<EnvironmentEDF>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_environment_shader_factory_registrar", &Project::get_factory_registrar<EnvironmentShader>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_light_factory_registrar", &Project::get_factory_registrar<Light>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_material_factory_registrar", &Project::get_factory_registrar<Material>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_object_factory_registrar", &Project::get_factory_registrar<Object>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_post_processing_stage_factory_registrar", &Project::get_factory_registrar<PostProcessingStage>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_surface_shader_factory_registrar", &Project::get_factory_registrar<SurfaceShader>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_texture_factory_registrar", &Project::get_factory_registrar<Texture>, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_volume_factory_registrar", &Project::get_factory_registrar<Volume>, bpy::return_value_policy<bpy::reference_existing_object>())

        .def("has_path", &Project::has_path)
        .def("set_path", &Project::set_path)
        .def("get_path", &Project::get_path)

        .def("get_search_paths", project_get_search_paths)
        .def("set_search_paths", project_set_search_paths)
        .def("qualify_path", qualify_path, bpy::args("filepath"))

        .def("set_scene", &Project::set_scene)
        .def("get_scene", &Project::get_scene, bpy::return_value_policy<bpy::reference_existing_object>())

        .def("set_frame", &Project::set_frame)
        .def("get_frame", &Project::get_frame, bpy::return_value_policy<bpy::reference_existing_object>())

        .def("get_display", &Project::get_display, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("set_display", &Project::set_display)

        .def("get_active_camera", &Project::get_uncached_active_camera, bpy::return_value_policy<bpy::reference_existing_object>())

        .def("_wrap_cpp_project_pointer", &wrap_cpp_project_pointer).staticmethod("_wrap_cpp_project_pointer");

    bpy::enum_<ProjectFileReader::Options>("ProjectFileReaderOptions")
        .value("Defaults", ProjectFileReader::Defaults)
        .value("OmitReadingMeshFiles", ProjectFileReader::OmitReadingMeshFiles)
        .value("OmitProjectFileUpdate", ProjectFileReader::OmitProjectFileUpdate)
        .value("OmitSearchPaths", ProjectFileReader::OmitSearchPaths)
        .value("OmitProjectSchemaValidation", ProjectFileReader::OmitProjectSchemaValidation);

    bpy::class_<ProjectFileReader>("ProjectFileReader")
        .def("read", &project_file_reader_read_default_opts)
        .def("read", &project_file_reader_read_with_opts)
        .def("load_builtin", &project_file_reader_load_builtin);

    bpy::enum_<ProjectFileWriter::Options>("ProjectFileWriterOptions")
        .value("Defaults", ProjectFileWriter::Defaults)
        .value("OmitHeaderComment", ProjectFileWriter::OmitHeaderComment)
        .value("OmitWritingGeometryFiles", ProjectFileWriter::OmitWritingGeometryFiles)
        .value("OmitHandlingAssetFiles", ProjectFileWriter::OmitHandlingAssetFiles)
        .value("CopyAllAssets", ProjectFileWriter::CopyAllAssets);

    bpy::class_<ProjectFileWriter>("ProjectFileWriter")
        // These methods are static but for symmetry with ProjectFileReader we're exposing them as non-static.
        .def("write", write_project_default_opts)
        .def("write", write_project_with_opts)
        .def("write", write_project_with_opts_and_comments);
}
