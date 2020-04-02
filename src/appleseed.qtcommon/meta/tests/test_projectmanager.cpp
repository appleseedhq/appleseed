
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.qtcommon headers.
#include "project/projectmanager.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/utility/test.h"

// Standard headers.
#include <string>

using namespace appleseed::qtcommon;
using namespace renderer;

void force_linking_qtcommon_project_projectmanager_tests() {}

TEST_SUITE(QtCommon_Project_ProjectManager)
{
    TEST_CASE(GetProject_GivenProjectManagerInDefaultState_ReturnsNull)
    {
        ProjectManager manager;

        const Project* project = manager.get_project();

        EXPECT_EQ(0, project);
    }

    TEST_CASE(CreateProject_GivenProjectManagerInDefaultState_CreatesNewProject)
    {
        ProjectManager manager;

        manager.create_project();

        const Project* project = manager.get_project();
        EXPECT_NEQ(0, project);
    }

    TEST_CASE(CloseProject_GivenProjectManagerWithOpenedProject_ClosesProject)
    {
        ProjectManager manager;
        manager.create_project();

        manager.close_project();

        const Project* project = manager.get_project();
        EXPECT_EQ(0, project);
    }

    TEST_CASE(IsProjectOpen_GivenProjectManagerInDefaultState_ReturnsFalse)
    {
        ProjectManager manager;

        const bool is_open = manager.is_project_open();

        EXPECT_FALSE(is_open);
    }

    TEST_CASE(IsProjectOpen_AfterCreateProject_ReturnsTrue)
    {
        ProjectManager manager;
        manager.create_project();

        const bool is_open = manager.is_project_open();

        EXPECT_TRUE(is_open);
    }

    TEST_CASE(IsProjectOpen_AfterCloseProject_ReturnsFalse)
    {
        ProjectManager manager;
        manager.create_project();
        manager.close_project();

        const bool is_open = manager.is_project_open();

        EXPECT_FALSE(is_open);
    }

    TEST_CASE(IsProjectDirty_GivenProjectManagerInDefaultState_ReturnsFalse)
    {
        ProjectManager manager;

        const bool is_dirty = manager.is_project_dirty();

        EXPECT_FALSE(is_dirty);
    }

    TEST_CASE(IsProjectDirty_AfterCreateProject_ReturnsFalse)
    {
        ProjectManager manager;
        manager.create_project();

        const bool is_dirty = manager.is_project_dirty();

        EXPECT_FALSE(is_dirty);
    }

    TEST_CASE(IsProjectDirty_AfterCloseProject_ReturnsFalse)
    {
        ProjectManager manager;
        manager.create_project();
        manager.close_project();

        const bool is_dirty = manager.is_project_dirty();

        EXPECT_FALSE(is_dirty);
    }

    TEST_CASE(GetProjectDisplayName_GivenProjectWithoutPath_ReturnsUntitled)
    {
        ProjectManager manager;
        manager.create_project();

        const std::string name = manager.get_project_display_name();

        EXPECT_EQ("Untitled", name);
    }

    TEST_CASE(GetProjectDisplayName_GivenProjectWithPath_ReturnsProjectFilename)
    {
        ProjectManager manager;
        manager.create_project();
        manager.get_project()->set_path("directory/filename.ext");

        const std::string name = manager.get_project_display_name();

        EXPECT_EQ("filename.ext", name);
    }
}
