
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

// appleseed.renderer headers.
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/projectfilewriter.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/texture/disktexture2d.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/test.h"

// boost headers.
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
          : m_base_output("unit tests/outputs/test_projectfilewriter/")
          , m_alternate_output("unit tests/outputs/test_projectfilewriter/alternate/")
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
            m_project->get_search_paths().push_back(filesystem::absolute(m_base_output).string());
        }

        void create_texture_entity(const string& filepath)
        {
            m_project->get_scene()->textures().insert(
                DiskTexture2dFactory().create(
                    "texture",
                    ParamArray().insert("filename", filepath),
                    SearchPaths()));
        }

        void create_texture_file(const filesystem::path& filepath)
        {
            const filesystem::path fullpath = m_base_output / filepath;
            filesystem::create_directories(fullpath.parent_path());
            filesystem::copy_file("unit tests/inputs/test_projectfilewriter_texture.png", fullpath);
        }
    };

    TEST_CASE_F(Write_TexturePathIsFilename_AndCopyAssetsIsTrue_AndOutputDirIsTheSame_LeavesFilenameParamUnchanged, Fixture)
    {
        create_project();
        create_texture_entity("texture.png");
        create_texture_file("texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_base_output / "texturepathisfilename.appleseed").string().c_str(),
                ProjectFileWriter::Defaults);

        ASSERT_TRUE(success);
        EXPECT_EQ("texture.png", m_project->get_scene()->textures().get_by_name("texture")->get_parameters().get<string>("filename"));
    }

    TEST_CASE_F(Write_TexturePathIsLocal_AndCopyAssetsIsTrue_AndOutputDirIsTheSame_LeavesFilenameParamUnchanged, Fixture)
    {
        create_project();
        create_texture_entity("tex/texture.png");
        create_texture_file("tex/texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_base_output / "texturepathislocal.appleseed").string().c_str(),
                ProjectFileWriter::Defaults);

        ASSERT_TRUE(success);
        EXPECT_EQ("tex/texture.png", m_project->get_scene()->textures().get_by_name("texture")->get_parameters().get<string>("filename"));
    }

    TEST_CASE_F(Write_TexturePathIsAbsolute_AndCopyAssetsIsTrue_AndOutputDirIsTheSame_CopiesTexture_AndFixesFilenameParam, Fixture)
    {
        create_project();
        create_texture_entity(filesystem::absolute(m_base_output / "tex" / "texture.png").string());
        create_texture_file("tex/texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_base_output / "texturepathisabsolute.appleseed").string().c_str(),
                ProjectFileWriter::Defaults);

        ASSERT_TRUE(success);
        EXPECT_TRUE(filesystem::exists(m_base_output / "texture.png"));
        EXPECT_EQ("texture.png", m_project->get_scene()->textures().get_by_name("texture")->get_parameters().get<string>("filename"));
    }

    TEST_CASE_F(Write_TexturePathIsFilename_AndCopyAssetsIsTrue_AndOutputDirIsDifferent_CopiesTexture_AndLeavesFilenameParamUnchanged, Fixture)
    {
        create_project();
        create_texture_entity("texture.png");
        create_texture_file("texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathisfilename.appleseed").string().c_str(),
                ProjectFileWriter::Defaults);

        ASSERT_TRUE(success);
        EXPECT_TRUE(filesystem::exists(m_alternate_output / "texture.png"));
        EXPECT_EQ("texture.png", m_project->get_scene()->textures().get_by_name("texture")->get_parameters().get<string>("filename"));
    }

    TEST_CASE_F(Write_TexturePathIsLocal_AndCopyAssetsIsTrue_AndOutputDirIsDifferent_CopiesTexture_AndLeavesFilenameParamUnchanged, Fixture)
    {
        create_project();
        create_texture_entity("tex/texture.png");
        create_texture_file("tex/texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathislocal.appleseed").string().c_str(),
                ProjectFileWriter::Defaults);

        ASSERT_TRUE(success);
        EXPECT_TRUE(filesystem::exists(m_alternate_output / "tex" / "texture.png"));
        EXPECT_EQ("tex/texture.png", m_project->get_scene()->textures().get_by_name("texture")->get_parameters().get<string>("filename"));
    }

    TEST_CASE_F(Write_TexturePathIsAbsolute_AndCopyAssetsIsTrue_AndOutputDirIsDifferent_CopiesTexture_AndFixesFilenameParam, Fixture)
    {
        create_project();
        create_texture_entity(filesystem::absolute(m_base_output / "tex" / "texture.png").string());
        create_texture_file("tex/texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathisabsolute.appleseed").string().c_str(),
                ProjectFileWriter::Defaults);

        ASSERT_TRUE(success);
        EXPECT_TRUE(filesystem::exists(m_alternate_output / "texture.png"));
        EXPECT_EQ("texture.png", m_project->get_scene()->textures().get_by_name("texture")->get_parameters().get<string>("filename"));
    }

    TEST_CASE_F(Write_TexturePathIsFilename_AndCopyAssetsIsFalse_AndOutputDirIsDifferent_FixesFilenameParam, Fixture)
    {
        create_project();
        create_texture_entity("texture.png");
        create_texture_file("texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathisfilename.appleseed").string().c_str(),
                ProjectFileWriter::OmitCopyingAssets);

        ASSERT_TRUE(success);
        EXPECT_TRUE(
            filesystem::equivalent(
                filesystem::absolute(m_base_output / "texture.png"),
                m_project->get_scene()->textures().get_by_name("texture")->get_parameters().get<string>("filename")));
    }

    TEST_CASE_F(Write_TexturePathIsFilename_AndCopyAssetsIsFalse_AndOutputDirIsDifferent_AndProjectHasNoSearchPathsSet_FixesFilenameParam, Fixture)
    {
        create_project();
        create_texture_entity("texture.png");
        create_texture_file("texture.png");

        m_project->get_search_paths().clear();

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathisfilename.appleseed").string().c_str(),
                ProjectFileWriter::OmitCopyingAssets);

        ASSERT_TRUE(success);
        EXPECT_TRUE(
            filesystem::equivalent(
                filesystem::absolute(m_base_output / "texture.png"),
                m_project->get_scene()->textures().get_by_name("texture")->get_parameters().get<string>("filename")));
    }

    TEST_CASE_F(Write_TexturePathIsLocal_AndCopyAssetsIsFalse_AndOutputDirIsDifferent_FixesFilenameParam, Fixture)
    {
        create_project();
        create_texture_entity("tex/texture.png");
        create_texture_file("tex/texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathislocal.appleseed").string().c_str(),
                ProjectFileWriter::OmitCopyingAssets);

        ASSERT_TRUE(success);
        EXPECT_TRUE(
            filesystem::equivalent(
                filesystem::absolute(m_base_output / "tex" / "texture.png"),
                m_project->get_scene()->textures().get_by_name("texture")->get_parameters().get<string>("filename")));
    }

    TEST_CASE_F(Write_TexturePathIsAbsolute_AndCopyAssetsIsFalse_AndOutputDirIsDifferent_LeavesFilenameParamUnchanged, Fixture)
    {
        create_project();
        create_texture_entity(filesystem::absolute(m_base_output / "tex" / "texture.png").string());
        create_texture_file("tex/texture.png");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathisabsolute.appleseed").string().c_str(),
                ProjectFileWriter::OmitCopyingAssets);

        ASSERT_TRUE(success);
        EXPECT_TRUE(
            filesystem::equivalent(
                filesystem::absolute(m_base_output / "tex" / "texture.png"),
                m_project->get_scene()->textures().get_by_name("texture")->get_parameters().get<string>("filename")));
    }

    TEST_CASE_F(Write_TexturePathIsFilename_AndCopyAssetsIsFalse_AndOutputDirIsDifferent_AndProjectHasNoPathSet_FixesFilenameParam, Fixture)
    {
        create_project();
        create_texture_entity("texture.png");
        create_texture_file("texture.png");

        m_project->set_path("");

        const bool success =
            ProjectFileWriter::write(
                m_project.ref(),
                (m_alternate_output / "texturepathisfilename.appleseed").string().c_str(),
                ProjectFileWriter::OmitCopyingAssets);

        ASSERT_TRUE(success);
        EXPECT_TRUE(
            filesystem::equivalent(
                filesystem::absolute(m_base_output / "texture.png"),
                m_project->get_scene()->textures().get_by_name("texture")->get_parameters().get<string>("filename")));
    }
}
