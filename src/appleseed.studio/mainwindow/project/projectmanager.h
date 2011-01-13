
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTMANAGER_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTMANAGER_H

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/autoreleaseptr.h"

// Standard headers.
#include <string>

namespace appleseed {
namespace studio {

//
// Manages rendering projects within the UI.
//

class ProjectManager
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    ProjectManager();

    // Create a new project.
    // If there already is a project open, it is first closed.
    void create_project();

    // Load a project from disk.
    // If loading was successful, closes the current project, replaces it
    // with the project loaded from disk, and returns true.  Otherwise,
    // keeps the current project open and returns false.
    bool load_project(const std::string& filepath);

    // Load a built-in project.
    // If loading was successful, closes the current project, replaces it
    // with the built-in project, and returns true.  Otherwise, keeps the
    // current project open and returns false.
    bool load_builtin_project(const std::string& name);

    // Reload the project that is currently open. The project must already have
    // its path set.  If loading was successful, closes the project, reopens it,
    // and returns true.  Otherwise, keeps the project open and returns false.
    bool reload_project();

    // Save the current project to disk.
    // A project must be open, and it must have its path set.
    bool save_project();

    // Save the current project to disk.
    // A project must be open.
    bool save_project_as(const std::string& filepath);

    // Close the current project.
    void close_project();

    // Return the current project, or 0 if there is no project is open.
    renderer::Project* get_project() const;

    // Return true if a project is open, false otherwise.
    bool is_project_open() const;

    // Return a string that is appropriate to identify the project.
    // A project must be open.
    std::string get_project_display_name() const;

    // Clear, set or get the dirty flag.
    // The dirty flag indicates whether the project has changed since the last time
    // it was written to disk, or whether a project that has not yet been written
    // to disk has changed since it was created (freshly created projects have their
    // dirty flag cleared).
    void clear_project_dirty_flag();
    void set_project_dirty_flag();
    bool is_project_dirty() const;

  private:
    typedef foundation::auto_release_ptr<renderer::Project> ProjectAutoReleasePtr;

    ProjectAutoReleasePtr   m_project;
    bool                    m_dirty_flag;

    static std::string get_project_schema_filepath();

    bool try_set_project(ProjectAutoReleasePtr project);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTMANAGER_H
