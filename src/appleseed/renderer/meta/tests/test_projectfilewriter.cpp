
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

// appleseed.renderer headers.
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/projectfilewriter.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/texture/disktexture2d.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/test.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <string>

using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

TEST_SUITE(Renderer_Modeling_Project_ProjectFileWriter)
{
    struct Fixture
    {
        const filesystem::path      m_base_output;
        const filesystem::path      m_alternate_output;
        auto_release_ptr<Project>   m_project;

        Fixture()
          : m_base_output(filesystem::absolute("unit tests/outputs/test_projectfilewriter/"))
          , m_alternate_output(filesystem::absolute("unit tests/outputs/test_projectfilewriter/alternate/"))
        {
            recreate_directories();
        }

        void recreate_directories()
        {
            filesystem::remove_all(m_base_output);
            filesystem::create_directory(m_base_output);
            filesystem::create_directory(m_alternate_output);
        }

        void create_project()
        {
            m_project = ProjectFactory::create("project");
            m_project->set_scene(SceneFactory::create());

            m_project->set_path((m_base_output / "input.appleseed").string().c_str());
            m_project->search_paths().set_root_path(m_base_output.string());
        }

        void create_texture_entity(const string& filepath)
        {
            ParamArray params;
            params.insert("filename", filepath);
            params.insert("color_space", "linear_rgb");

            SearchPaths search_paths;

            m_project->get_scene()->textures().insert(
                DiskTexture2dFactory().create("texture", params, search_paths));
        }

        void create_texture_file(const filesystem::path& filepath)
        {
            const filesystem::path fullpath = m_base_output / filepath;
            filesystem::create_directories(fullpath.parent_path());
            filesystem::copy_file("unit tests/inputs/test_projectfilewriter_texture.png", fullpath);
        }

        void create_geometry_file(const filesystem::path& filepath)
        {
            const filesystem::path fullpath = m_base_output / filepath;
            filesystem::create_directories(fullpath.parent_path());
            filesystem::copy_file("unit tests/inputs/test_projectfilewriter_object.obj", fullpath);
        }

        string get_texture_entity_filepath() const
        {
            return m_project->get_scene()->textures().get_by_name("texture")->get_parameters().get<string>("filename");
        }

        static string make_absolute_path(const filesystem::path& base, const string& relative)
        {
            filesystem::path result = base / relative;
            result.make_preferred();
            return result.string();
        }
    };

    TEST_CASE_F(Write_TexturePathIsFilename_AndBringAssetsIsTrue_AndOutputDirIsTheSame_LeavesFilenameParamUnchanged, Fixture)
    {
        create_project();
        create_texture_entity("texture.png");
        create_texture_file("texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_base_output / "texturepathisfilename.appleseed").string().c_str());

        ASSERT_TRUE(success);
        EXPECT_EQ("texture.png", get_texture_entity_filepath());
    }

    TEST_CASE_F(Write_TexturePathIsLocal_AndBringAssetsIsTrue_AndOutputDirIsTheSame_LeavesFilenameParamUnchanged, Fixture)
    {
        create_project();
        create_texture_entity("tex/texture.png");
        create_texture_file("tex/texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_base_output / "texturepathislocal.appleseed").string().c_str());

        ASSERT_TRUE(success);
        EXPECT_EQ("tex/texture.png", get_texture_entity_filepath());
    }

    TEST_CASE_F(Write_TexturePathIsAbsolute_AndBringAssetsIsTrue_AndOutputDirIsTheSame_CopiesTexture_AndFixesFilenameParam, Fixture)
    {
        create_project();
        create_texture_entity((m_base_output / "tex" / "texture.png").string());
        create_texture_file("tex/texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_base_output / "texturepathisabsolute.appleseed").string().c_str());

        ASSERT_TRUE(success);
        EXPECT_EQ("tex/texture.png", get_texture_entity_filepath());
    }

    TEST_CASE_F(Write_TexturePathIsFilename_AndBringAssetsIsTrue_AndOutputDirIsDifferent_CopiesTexture_AndLeavesFilenameParamUnchanged, Fixture)
    {
        create_project();
        create_texture_entity("texture.png");
        create_texture_file("texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathisfilename.appleseed").string().c_str());

        ASSERT_TRUE(success);
        EXPECT_TRUE(filesystem::exists(m_alternate_output / "texture.png"));
        EXPECT_EQ("texture.png", get_texture_entity_filepath());
    }

    TEST_CASE_F(Write_TexturePathIsLocal_AndBringAssetsIsTrue_AndOutputDirIsDifferent_CopiesTexture_AndLeavesFilenameParamUnchanged, Fixture)
    {
        create_project();
        create_texture_entity("tex/texture.png");
        create_texture_file("tex/texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathislocal.appleseed").string().c_str());

        ASSERT_TRUE(success);
        EXPECT_TRUE(filesystem::exists(m_alternate_output / "tex" / "texture.png"));
        EXPECT_EQ("tex/texture.png", get_texture_entity_filepath());
    }

    TEST_CASE_F(Write_TexturePathIsAbsolute_AndBringAssetsIsTrue_AndOutputDirIsDifferent_CopiesTexture_AndFixesFilenameParam, Fixture)
    {
        create_project();
        create_texture_entity((m_base_output / "tex" / "texture.png").string());
        create_texture_file("tex/texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathisabsolute.appleseed").string().c_str());

        ASSERT_TRUE(success);
        EXPECT_TRUE(filesystem::exists(m_alternate_output / "tex" / "texture.png"));
        EXPECT_EQ("tex/texture.png", get_texture_entity_filepath());
    }

    TEST_CASE_F(Write_TexturePathIsFilename_AndBringAssetsIsFalse_AndOutputDirIsDifferent_FixesFilenameParam, Fixture)
    {
        create_project();
        create_texture_entity("texture.png");
        create_texture_file("texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathisfilename.appleseed").string().c_str(),
                ProjectFileWriter::OmitBringingAssets);

        const string expected_filepath = make_absolute_path(m_base_output, "texture.png");

        ASSERT_TRUE(success);
        EXPECT_EQ(expected_filepath, get_texture_entity_filepath());
    }

    TEST_CASE_F(Write_TexturePathIsFilename_AndBringAssetsIsFalse_AndOutputDirIsDifferent_AndProjectHasNoPathSet_FixesFilenameParam, Fixture)
    {
        create_project();
        create_texture_entity("texture.png");
        create_texture_file("texture.png");

        m_project->set_path("");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathisfilename.appleseed").string().c_str(),
                ProjectFileWriter::OmitBringingAssets);

        const string expected_filepath = make_absolute_path(m_base_output, "texture.png");

        ASSERT_TRUE(success);
        EXPECT_EQ(expected_filepath, get_texture_entity_filepath());
    }

    TEST_CASE_F(Write_TexturePathIsFilename_AndBringAssetsIsFalse_AndOutputDirIsDifferent_AndProjectHasNoSearchPathsSet_FixesFilenameParam, Fixture)
    {
        create_project();
        create_texture_entity("texture.png");
        create_texture_file("texture.png");

        m_project->search_paths().clear();

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathisfilename.appleseed").string().c_str(),
                ProjectFileWriter::OmitBringingAssets);

        const string expected_filepath = make_absolute_path(m_base_output, "texture.png");

        ASSERT_TRUE(success);
        EXPECT_EQ(expected_filepath, get_texture_entity_filepath());
    }

    TEST_CASE_F(Write_TexturePathIsLocal_AndBringAssetsIsFalse_AndOutputDirIsDifferent_FixesFilenameParam, Fixture)
    {
        create_project();
        create_texture_entity("tex/texture.png");
        create_texture_file("tex/texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathislocal.appleseed").string().c_str(),
                ProjectFileWriter::OmitBringingAssets);

        const string expected_filepath = make_absolute_path(m_base_output, "tex/texture.png");

        ASSERT_TRUE(success);
        EXPECT_EQ(expected_filepath, get_texture_entity_filepath());
    }

    TEST_CASE_F(Write_TexturePathIsLocal_AndBringAssetsIsFalse_AndOutputDirIsTheSame_FixesFilenameParam, Fixture)
    {
        create_project();
        create_texture_entity("tex/texture.png");
        create_texture_file("tex/texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_base_output / "texturepathislocal.appleseed").string().c_str(),
                ProjectFileWriter::OmitBringingAssets);

        ASSERT_TRUE(success);
        EXPECT_EQ("tex/texture.png", get_texture_entity_filepath());
    }

    TEST_CASE_F(Write_TexturePathIsAbsolute_AndBringAssetsIsFalse_AndOutputDirIsDifferent_LeavesFilenameParamUnchanged, Fixture)
    {
        create_project();
        create_texture_entity((m_base_output / "tex" / "texture.png").string());
        create_texture_file("tex/texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathisabsolute.appleseed").string().c_str(),
                ProjectFileWriter::OmitBringingAssets);

        ASSERT_TRUE(success);
        EXPECT_EQ("tex/texture.png", get_texture_entity_filepath());
    }

    TEST_CASE_F(Write_GivenMeshObjectWithMultiValueFilenameParameter_DoesNotAddAnotherFilenameParameter, Fixture)
    {
        create_project();
        create_geometry_file("bunny.0.obj");
        create_geometry_file("bunny.1.obj");

        ParamArray filenames;
        filenames.insert("0", "bunny.0.obj");
        filenames.insert("1", "bunny.1.obj");

        ParamArray object_params;
        object_params.insert("filename", filenames);

        auto_release_ptr<Object> object(MeshObjectFactory::create("bunny", object_params));

        auto_release_ptr<Assembly> assembly(AssemblyFactory().create("assembly", ParamArray()));
        assembly->objects().insert(object);

        m_project->get_scene()->assemblies().insert(assembly);

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_base_output / "multivaluefilenameobject.appleseed").string().c_str());

        ASSERT_TRUE(success);

        EXPECT_FALSE(
            m_project->get_scene()
                ->assemblies().get_by_name("assembly")
                ->objects().get_by_name("bunny")
                    ->get_parameters().strings().exist("filename"));
    }
}
