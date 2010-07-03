
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
#include "application.h"

// appleseed.foundation headers.
#include "foundation/platform/path.h"
#include "foundation/utility/log.h"

// Standard headers.
#include <cstring>

using namespace boost;
using namespace foundation;
using namespace std;

namespace appleseed {
namespace shared {

//
// Application class implementation.
//

// Return true if the application is correctly installed, false otherwise.
bool Application::is_correctly_installed()
{
    const char* root_path = get_root_path();
    return strlen(root_path) > 0;
}

// Check if the application is correctly installed.
void Application::check_installation(Logger& logger)
{
    if (!is_correctly_installed())
    {
        // We need the path to the application's executable to construct the error message.
        const filesystem::path executable_path(Path::get_executable_path());

        // Issue a fatal error message.
        FOUNDATION_LOG_FATAL(
            logger,
            "The application failed to start because it is not properly installed. "
            "Please reinstall the application.\n\n"
            "Specifically, it was expected that %s would reside in a bin/ subdirectory "
            "inside the main directory of the application, but it appears not to be "
            "the case (%s seems to be located in %s).",
            executable_path.filename().c_str(),
            executable_path.filename().c_str(),
            executable_path.parent_path().directory_string().c_str());
    }
}

namespace
{
    // Compute the root path of the application.  Return true if the root path could be
    // determined, in which case it is stored in root_path, or false if the application
    // is not properly installed, in which case root_path is left unaltered.
    bool compute_root_path(filesystem::path& root_path)
    {
        // Retrieve the full path to the application's executable.
        const filesystem::path executable_path(Path::get_executable_path());

        // Remove the end of the path until /bin is reached.
        filesystem::path path = executable_path;
        while (path.has_parent_path() && path.filename() != "bin")
            path = path.parent_path();

        if (path.has_root_path())
        {
            root_path = path.parent_path();
            return true;
        }
        else
        {
            return false;
        }
    }

    void copy_directory_path_to_buffer(const filesystem::path& path, char* output)
    {
        const string path_string = path.directory_string();

        assert(path_string.size() <= FOUNDATION_MAX_PATH_LENGTH);

        strcpy(output, path_string.c_str());
        output[path_string.size()] = '\0';
    }
}

// Return the root path of the application.
const char* Application::get_root_path()
{
    static char root_path_buffer[FOUNDATION_MAX_PATH_LENGTH + 1];
    static bool root_path_initialized = false;

    if (!root_path_initialized)
    {
        filesystem::path root_path;

        if (compute_root_path(root_path))
        {
            copy_directory_path_to_buffer(root_path, root_path_buffer);
        }
        else
        {
            root_path_buffer[0] = '\0';
        }

        root_path_initialized = true;
    }

    return root_path_buffer;
}

// Return the root path of the application's tests.
const char* Application::get_tests_root_path()
{
    static char tests_root_path_buffer[FOUNDATION_MAX_PATH_LENGTH + 1];
    static bool tests_root_path_initialized = false;

    if (!tests_root_path_initialized)
    {
        filesystem::path root_path;

        if (compute_root_path(root_path))
        {
            root_path = root_path / filesystem::path("tests/");
            copy_directory_path_to_buffer(root_path, tests_root_path_buffer);
        }
        else
        {
            tests_root_path_buffer[0] = '\0';
        }

        tests_root_path_initialized = true;
    }

    return tests_root_path_buffer;
}

}   // namespace shared
}   // namespace appleseed
