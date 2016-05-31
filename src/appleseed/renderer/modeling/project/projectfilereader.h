
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_PROJECT_PROJECTFILEREADER_H
#define APPLESEED_RENDERER_MODELING_PROJECT_PROJECTFILEREADER_H

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace renderer  { class EventCounters; }
namespace renderer  { class Project; }

namespace renderer
{

//
// Project file reader.
//

class APPLESEED_DLLSYMBOL ProjectFileReader
{
  public:
    enum Options
    {
        Defaults                = 0,        // none of the flags below
        OmitReadingMeshFiles    = 1 << 0,   // do not read mesh files from disk
        OmitProjectFileUpdate   = 1 << 1    // do not update the project file format to the latest revision
    };

    // Read a project from disk (or load a built-in project).
    // Return 0 if reading or parsing the file failed.
    foundation::auto_release_ptr<Project> read(
        const char*             project_filepath,
        const char*             schema_filepath,
        const int               options = Defaults);

    // Load a built-in project.
    // Return 0 if the requested built-in project does not exist.
    foundation::auto_release_ptr<Project> load_builtin(
        const char*             project_name);

  private:
    foundation::auto_release_ptr<Project> load_project_file(
        const char*             project_filepath,
        const char*             schema_filepath,
        const int               options,
        EventCounters&          event_counters) const;

    foundation::auto_release_ptr<Project> construct_builtin_project(
        const char*             project_name,
        EventCounters&          event_counters) const;

    // Finish loading a project.
    void postprocess_project(
        Project&                project,
        EventCounters&          event_counters,
        const int               options = Defaults) const;

    // Check the validity of a project.
    void validate_project(
        const Project&          project,
        EventCounters&          event_counters) const;

    // Add missing entities to a valid project.
    void complete_project(
        Project&                project,
        EventCounters&          event_counters) const;

    // Update a project to the latest project format revision.
    void upgrade_project(
        Project&                project,
        EventCounters&          event_counters) const;

    void print_loading_results(
        const char*             project_name,
        const bool              builtin_project,
        const EventCounters&    event_counters,
        const double            loading_time) const;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_PROJECT_PROJECTFILEREADER_H
