
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

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/modeling/object/curveobject.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/projectfilewriter.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/platform/thread.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/test.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <string>

using namespace boost;
using namespace boost::filesystem;
using namespace foundation;
using namespace renderer;
using namespace std;

TEST_SUITE(Renderer_Modeling_Project_ProjectFileWriter)
{
    struct Fixture
    {
        const path                  m_input_directory;
        const path                  m_output_directory;
        auto_release_ptr<Project>   m_project;

        Fixture()
          : m_input_directory(absolute("unit tests/inputs/test_projectfilewriter/"))
          , m_output_directory(absolute("unit tests/outputs/test_projectfilewriter/"))
        {
            remove_all(m_output_directory);

            // On Windows, the create_directory() call below will fail with an Access Denied error
            // if a File Explorer window was opened in the output directory that we just deleted.
            // A small pause solves the problem. The namespace qualifier is required on Linux.
            foundation::sleep(50);

            create_directory(m_output_directory);
        }

        void create_project()
        {
            m_project = ProjectFactory::create("project");
            m_project->set_scene(SceneFactory::create());

            m_project->set_path((m_output_directory / "project.appleseed").string().c_str());
            m_project->search_paths().set_root_path(m_output_directory.string());
        }

        void create_assembly()
        {
            m_project->get_scene()->assemblies().insert(AssemblyFactory().create("assembly"));
        }

        Assembly* get_assembly()
        {
            return m_project->get_scene()->assemblies().get_by_name("assembly");
        }

        template <typename T>
        void create_mesh_object(const char* object_name, const T& filename)
        {
            get_assembly()->objects().insert(
                auto_release_ptr<Object>(
                    MeshObjectFactory::create(
                        object_name,
                        ParamArray().insert("filename", filename))));
        }

        void create_mesh_object(const char* object_name, const path& filename)
        {
            create_mesh_object(object_name, filename.string());
        }

        const char* get_mesh_object_filename(const char* object_name)
        {
            return get_assembly()->objects().get_by_name(object_name)->get_parameters().get("filename");
        }

        void create_curve_object(const char* object_name)
        {
            auto_release_ptr<CurveObject> curve_object(
                CurveObjectFactory::create(object_name, ParamArray()));

            static const GVector3 ControlPoints[] = { GVector3(0.0, 0.0, 0.0), GVector3(0.0, 1.0, 0.0) };
            curve_object->push_curve1(Curve1Type(ControlPoints, GScalar(0.1)));

            get_assembly()->objects().insert(auto_release_ptr<Object>(curve_object));
        }

        void create_geometry_file(const path& filepath)
        {
            const path output_path = m_output_directory / filepath;
            create_directories(output_path.parent_path());
            copy_file(m_input_directory / "object.obj", output_path);
        }
    };

    TEST_CASE_F(Write_CopyAllAssetsIsNotSet_HandleAssetPaths, Fixture)
    {
        m_project = ProjectFactory::create("project");
        m_project->set_scene(SceneFactory::create());

        const path project_directory = m_input_directory / "setup/main/";
        m_project->set_path((project_directory / "project.appleseed").string().c_str());
        m_project->search_paths().set_root_path(project_directory.string());

        m_project->search_paths().push_back("subdirectory");
        m_project->search_paths().push_back(canonical(project_directory / "../alternate/subdirectory").string());

        create_assembly();
        create_mesh_object("asset1", "asset1.obj"); // found in project's root directory
        create_mesh_object("asset2", "asset2.obj"); // found in subdirectory/ via relative search path
        create_mesh_object("asset3", "asset3.obj"); // found in ../alternate/subdirectory/ via absolute search path
        create_mesh_object("asset4", "subdirectory/asset4.obj");
        create_mesh_object("asset5", canonical(project_directory / "asset5.obj"));
        create_mesh_object("asset6", canonical(project_directory / "../alternate/asset6.obj"));
        create_mesh_object("asset7", "../alternate/asset7.obj");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_output_directory / "project.appleseed").string().c_str(),
                ProjectFileWriter::OmitHeaderComment);

        ASSERT_TRUE(success);

        // Check the search paths.
        EXPECT_EQ(2, m_project->search_paths().size());
        EXPECT_EQ(string("subdirectory"), m_project->search_paths()[0]);
        EXPECT_EQ(canonical(m_input_directory / "setup/alternate/subdirectory").string(), m_project->search_paths()[1]);

        // Check the asset paths.
        EXPECT_EQ(string("asset1.obj"),                                        get_mesh_object_filename("asset1"));
        EXPECT_EQ(string("asset2.obj"),                                        get_mesh_object_filename("asset2"));
        EXPECT_EQ(string("asset3.obj"),                                        get_mesh_object_filename("asset3"));
        EXPECT_EQ(string("subdirectory/asset4.obj"),                           get_mesh_object_filename("asset4"));
        EXPECT_EQ(canonical(project_directory / "asset5.obj"),                 get_mesh_object_filename("asset5"));
        EXPECT_EQ(canonical(m_input_directory / "setup/alternate/asset6.obj"), get_mesh_object_filename("asset6"));
        EXPECT_EQ(canonical(m_input_directory / "setup/alternate/asset7.obj"), get_mesh_object_filename("asset7"));
    }

    TEST_CASE_F(Write_CopyAllAssetsIsSet_HandleAssetPaths, Fixture)
    {
        m_project = ProjectFactory::create("project");
        m_project->set_scene(SceneFactory::create());

        const path project_directory = m_input_directory / "setup/main/";
        m_project->set_path((project_directory / "project.appleseed").string().c_str());
        m_project->search_paths().set_root_path(project_directory.string());

        m_project->search_paths().push_back("subdirectory");
        m_project->search_paths().push_back(canonical(project_directory / "../alternate/subdirectory").string());

        create_assembly();
        create_mesh_object("asset1", "asset1.obj"); // found in project's root directory
        create_mesh_object("asset2", "asset2.obj"); // found in subdirectory/ via search paths
        create_mesh_object("asset3", "asset3.obj"); // found in ../alternate/subdirectory/ via absolute search path
        create_mesh_object("asset4", "subdirectory/asset4.obj");
        create_mesh_object("asset5", canonical(project_directory / "asset5.obj"));
        create_mesh_object("asset6", canonical(project_directory / "../alternate/asset6.obj"));
        create_mesh_object("asset7", "../alternate/asset7.obj");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_output_directory / "project.appleseed").string().c_str(),
                ProjectFileWriter::OmitHeaderComment |
                ProjectFileWriter::CopyAllAssets);

        ASSERT_TRUE(success);

        // Check the search paths.
        EXPECT_EQ(1, m_project->search_paths().size());
        EXPECT_EQ(string("subdirectory"), m_project->search_paths()[0]);

        // Check the asset paths.
        EXPECT_EQ(string("asset1.obj"),              get_mesh_object_filename("asset1"));
        EXPECT_EQ(string("asset2.obj"),              get_mesh_object_filename("asset2"));
        EXPECT_EQ(string("assets/asset3.obj"),       get_mesh_object_filename("asset3"));
        EXPECT_EQ(string("subdirectory/asset4.obj"), get_mesh_object_filename("asset4"));
        EXPECT_EQ(string("assets/asset5.obj"),       get_mesh_object_filename("asset5"));
        EXPECT_EQ(string("assets/asset6.obj"),       get_mesh_object_filename("asset6"));
        EXPECT_EQ(string("assets/asset7.obj"),       get_mesh_object_filename("asset7"));
    }

    TEST_CASE_F(Write_MeshObjectWithMultivaluedFilenameParameter_DoesNotAddAnotherFilenameParameter, Fixture)
    {
        create_geometry_file("bunny.0.obj");
        create_geometry_file("bunny.1.obj");

        create_project();
        create_assembly();
        create_mesh_object(
            "bunny",
            ParamArray()
                .insert("0", "bunny.0.obj")
                .insert("1", "bunny.1.obj"));

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_output_directory / "multivaluedfilenameobject.appleseed").string().c_str(),
                ProjectFileWriter::OmitHeaderComment);

        ASSERT_TRUE(success);
        EXPECT_FALSE(get_assembly()->objects().get_by_name("bunny")->get_parameters().strings().exist("filename"));
    }

    TEST_CASE_F(Write_CurveObjectWithoutFilePath_OmitWritingGeometryFilesIsNotSet_CreatesCurvesFileAndAssignsFilePath, Fixture)
    {
        create_project();
        create_assembly();
        create_curve_object("curve_object");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_output_directory / "curve_object.appleseed").string().c_str(),
                ProjectFileWriter::OmitHeaderComment);

        ASSERT_TRUE(success);
        EXPECT_TRUE(exists(m_output_directory / "curve_object.txt"));
        EXPECT_EQ(
            string("curve_object.txt"),
            get_assembly()->objects().get_by_name("curve_object")->get_parameters().get("filepath"));
    }

    TEST_CASE_F(Write_CurveObjectWithoutFilePath_OmitWritingGeometryFilesIsSet_OnlyAssignsFilePath, Fixture)
    {
        create_project();
        create_assembly();
        create_curve_object("curve_object");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_output_directory / "curve_object.appleseed").string().c_str(),
                ProjectFileWriter::OmitHeaderComment |
                ProjectFileWriter::OmitWritingGeometryFiles);

        ASSERT_TRUE(success);
        EXPECT_FALSE(exists(m_output_directory / "curve_object.txt"));
        EXPECT_EQ(
            string("curve_object.txt"),
            get_assembly()->objects().get_by_name("curve_object")->get_parameters().get("filepath"));
    }

    TEST_CASE_F(Write_PackValidProject, Fixture)
    {
        create_project();

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_output_directory / "test_pack_valid_project.appleseedz").string().c_str(),
                ProjectFileWriter::Defaults
            );

        ASSERT_TRUE(success);
        ASSERT_TRUE(exists(m_output_directory / "test_pack_valid_project.appleseedz"));
        ASSERT_FALSE(exists(m_output_directory / "test_pack_valid_project.unpacked"));
    }
}
