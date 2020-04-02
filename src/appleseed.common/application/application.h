
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

#pragma once

// appleseed.foundation headers.
#include "foundation/log/logmessage.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"
#include "boost/system/error_code.hpp"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class Logger; }
namespace foundation    { class SearchPaths; }

namespace appleseed {
namespace common {

//
// The Application class gathers functionalities for checking if the application
// is correctly installed, finding out what is the application's root path, etc.
//

class Application
{
  public:
    // Return true if the application can run on the host machine, false otherwise.
    static bool is_compatible_with_host(const char** missing_feature);

    // Check if the application can run on the host machine. If it cannot, issue
    // a fatal error message through the provided foundation::Logger object.
    static void check_compatibility_with_host(foundation::Logger& logger);

    // Return true if the application is correctly installed, false otherwise.
    static bool is_correctly_installed();

    // Check if the application is correctly installed. If it is not, issue
    // a fatal error message through the provided foundation::Logger object.
    static void check_installation(foundation::Logger& logger);

    // Force the use of a given root path instead of automatically determining it.
    // get_root_path() will return the exact value that was set by this method.
    static void set_root_path(const char* path);

    // Return the root path of the application.
    // This is the path to the parent of the bin/ directory.
    // Returns nullptr if the application is not correctly installed.
    static const char* get_root_path();

    // Return the user settings path.
    // Returns nullptr if the notion of user settings path is not supported on this platform.
    static const char* get_user_settings_path();

    // Return the root path of the application's tests (unit tests, unit benchmarks, etc.).
    // On a developer's machine, this is path/to/appleseed/sandbox/tests/.
    // Returns nullptr if the application is not correctly installed.
    static const char* get_tests_root_path();

    // Return the output path of the application's unit tests.
    // On a developer's machine, this is path/to/appleseed/sandbox/tests/unit tests/outputs/.
    // Returns nullptr if the application is not correctly installed.
    static const char* get_unit_tests_output_path();

    // Load a settings file from appleseed's settings directory.
    // Returns true if settings could be loaded, false otherwise.
    static bool load_settings(
        const char*                             filename,
        foundation::Dictionary&                 settings,
        foundation::Logger&                     logger,
        const foundation::LogMessage::Category  category = foundation::LogMessage::Debug);

    // Save settings to a settings file in appleseed's settings directory.
    // Returns true if settings could be saved, false otherwise.
    static bool save_settings(
        const char*                             filename,
        const foundation::Dictionary&           settings,
        foundation::Logger&                     logger,
        const foundation::LogMessage::Category  category = foundation::LogMessage::Debug);

    // Initialize resource search paths (currently path to OSL headers).
    static void initialize_resource_search_paths(
        foundation::SearchPaths&                search_paths);

    // Change the current directory to the root path of the application's tests.
    // Returns the path to the current directory.
    static boost::filesystem::path change_current_directory_to_tests_root_path();

    // Create output directories for the application's unit tests.
    // Returns true on success, false otherwise.
    static bool create_unit_tests_output_directories();
};


//
// Application class implementation.
//

inline boost::filesystem::path Application::change_current_directory_to_tests_root_path()
{
    namespace bf = boost::filesystem;

    const bf::path old_current_path = bf::current_path();
    bf::current_path(get_tests_root_path());
    return old_current_path;
}

inline bool Application::create_unit_tests_output_directories()
{
    boost::system::error_code ec;
    boost::filesystem::create_directories(get_unit_tests_output_path(), ec);
    return !ec;
}

}   // namespace common
}   // namespace appleseed
