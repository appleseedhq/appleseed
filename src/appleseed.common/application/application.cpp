
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

// Interface header.
#include "application.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/log/log.h"
#include "foundation/platform/path.h"
#include "foundation/platform/system.h"
#include "foundation/string/string.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/settings.h"

// Boost headers.
#include "boost/filesystem/exception.hpp"

// Standard headers.
#include <cstring>
#include <string>

using namespace foundation;
namespace bf = boost::filesystem;

namespace appleseed {
namespace common {

//
// Application class implementation.
//

bool Application::is_correctly_installed()
{
    return !is_empty_string(get_root_path());
}

void Application::check_installation(Logger& logger)
{
    if (!is_correctly_installed())
    {
        logger.set_all_formats("{message}");
        LOG_FATAL(
            logger,
            "the application failed to start because it is not properly installed. "
            "please reinstall the application.");
    }
}

bool Application::is_compatible_with_host(const char** missing_feature)
{
#ifdef APPLESEED_X86
    System::X86CPUFeatures features;
    System::detect_x86_cpu_features(features);

#ifdef APPLESEED_USE_SSE
    if (!features.m_hw_sse)
    {
        if (missing_feature)
            *missing_feature = "SSE";
        return false;
    }
#endif

#ifdef APPLESEED_USE_SSE42
    if (!features.m_hw_sse42)
    {
        if (missing_feature)
            *missing_feature = "SSE4.2";
        return false;
    }
#endif

#ifdef APPLESEED_USE_AVX
    if (!features.m_hw_avx)
    {
        if (missing_feature)
            *missing_feature = "AVX";
        return false;
    }
#endif

#ifdef APPLESEED_USE_AVX2
    if (!features.m_hw_avx2)
    {
        if (missing_feature)
            *missing_feature = "AVX2";
        return false;
    }
#endif
#endif

    return true;
}

void Application::check_compatibility_with_host(Logger& logger)
{
    const char* missing_feature = nullptr;
    if (!is_compatible_with_host(&missing_feature))
    {
        logger.set_all_formats("{message}");
        LOG_FATAL(
            logger,
            "this executable requires a cpu with %s support.",
            lower_case(missing_feature).c_str());
    }
}

namespace
{
    static char g_root_path[FOUNDATION_MAX_PATH_LENGTH + 1] = { 0 };
    static bool g_is_root_path_set = false;

    // Compute the root path of the application. Return true if the root path could be
    // determined, in which case it is stored in root_path, or false if the application
    // is not properly installed, in which case root_path is left unaltered.
    bool compute_root_path(bf::path& root_path)
    {
        // Retrieve the full path to the application's executable.
        const bf::path executable_path(get_executable_path());

        // Remove the end of the path until /bin is reached.
        bf::path path = executable_path;
        while (path.has_parent_path() && path.filename() != "bin")
            path = path.parent_path();

        if (!path.has_root_path())
            return false;

        root_path = path.parent_path();
        return true;
    }

    void copy_directory_path_to_buffer(const bf::path& path, char* output)
    {
        const std::string path_string = path.string();

        assert(path_string.size() <= FOUNDATION_MAX_PATH_LENGTH);

        std::strncpy(output, path_string.c_str(), FOUNDATION_MAX_PATH_LENGTH);
        output[FOUNDATION_MAX_PATH_LENGTH] = '\0';
    }

    const char* nullptr_if_empty(const char* buffer)
    {
        return buffer[0] != '\0' ? buffer : nullptr;
    }
}

void Application::set_root_path(const char* path)
{
    std::strncpy(g_root_path, path, FOUNDATION_MAX_PATH_LENGTH);
    g_root_path[FOUNDATION_MAX_PATH_LENGTH] = '\0';

    g_is_root_path_set = true;
}

const char* Application::get_root_path()
{
    if (!g_is_root_path_set)
    {
        bf::path root_path;
        if (compute_root_path(root_path))
            copy_directory_path_to_buffer(root_path, g_root_path);

        g_is_root_path_set = true;
    }

    return nullptr_if_empty(g_root_path);
}

const char* Application::get_user_settings_path()
{
    static char buffer[FOUNDATION_MAX_PATH_LENGTH + 1] = { 0 };
    static bool buffer_initialized = false;

    if (!buffer_initialized)
    {
// Windows.
#if defined _WIN32

// macOS.
#elif defined __APPLE__

// Other Unices.
#elif defined __linux__ || defined __FreeBSD__

        bf::path p(get_home_directory());
        p /= ".appleseed/settings";
        copy_directory_path_to_buffer(p, buffer);

// Other platforms.
#else

        #error Unsupported platform.

#endif

        buffer_initialized = true;
    }

    return nullptr_if_empty(buffer);
}

const char* Application::get_tests_root_path()
{
    static char buffer[FOUNDATION_MAX_PATH_LENGTH + 1] = { 0 };
    static bool buffer_initialized = false;

    if (!buffer_initialized)
    {
        bf::path root_path;
        if (compute_root_path(root_path))
        {
            root_path = root_path / "tests";
            copy_directory_path_to_buffer(root_path, buffer);
        }

        buffer_initialized = true;
    }

    return nullptr_if_empty(buffer);
}

const char* Application::get_unit_tests_output_path()
{
    static char buffer[FOUNDATION_MAX_PATH_LENGTH + 1] = { 0 };
    static bool buffer_initialized = false;

    if (!buffer_initialized)
    {
        bf::path root_path;
        if (compute_root_path(root_path))
        {
            root_path = root_path / "tests" / "unit tests" / "outputs";
            copy_directory_path_to_buffer(root_path, buffer);
        }

        buffer_initialized = true;
    }

    return nullptr_if_empty(buffer);
}

bool Application::load_settings(
    const char*                 filename,
    Dictionary&                 settings,
    Logger&                     logger,
    const LogMessage::Category  category)
{
    const bf::path root_path(get_root_path());
    const bf::path schema_file_path = root_path / "schemas" / "settings.xsd";

    SettingsFileReader reader(logger);

    // First try to read settings from the user path.
    if (const char* user_path = get_user_settings_path())
    {
        const bf::path user_settings_file_path = safe_weakly_canonical(bf::path(user_path) / filename);
        if (bf::exists(user_settings_file_path) &&
            reader.read(
                user_settings_file_path.string().c_str(),
                schema_file_path.string().c_str(),
                settings))
        {
            LOG(logger, category, "successfully loaded settings from %s.", user_settings_file_path.string().c_str());
            return true;
        }
    }

    // As a fallback, try to read settings from appleseed's installation directory.
    const std::string settings_file_path = safe_weakly_canonical(root_path / "settings" / filename).string();
    if (reader.read(
            settings_file_path.c_str(),
            schema_file_path.string().c_str(),
            settings))
    {
        LOG(logger, category, "successfully loaded settings from %s.", settings_file_path.c_str());
        return true;
    }

    LOG(logger, LogMessage::Error, "failed to load settings from %s.", settings_file_path.c_str());
    return false;
}

bool Application::save_settings(
    const char*                 filename,
    const Dictionary&           settings,
    Logger&                     logger,
    const LogMessage::Category  category)
{
    SettingsFileWriter writer;

    // First try to write settings to the user path.
    if (const char* user_path = get_user_settings_path())
    {
        try
        {
            const bf::path user_settings_path(user_path);
            bf::create_directories(user_settings_path);

            const std::string user_settings_file_path = safe_weakly_canonical(user_settings_path / filename).string();
            if (writer.write(user_settings_file_path.c_str(), settings))
            {
                LOG(logger, category, "successfully saved settings to %s.", user_settings_file_path.c_str());
                return true;
            }
        }
        catch (const bf::filesystem_error&)
        {
        }
    }

    // As a fallback, try to write settings to appleseed's installation directory.
    const bf::path root_path(get_root_path());
    const std::string settings_file_path = safe_weakly_canonical(root_path / "settings" / filename).string();
    if (writer.write(settings_file_path.c_str(), settings))
    {
        LOG(logger, category, "successfully saved settings to %s.", settings_file_path.c_str());
        return true;
    }

    LOG(logger, LogMessage::Error, "failed to save settings to %s.", settings_file_path.c_str());
    return false;
}

void Application::initialize_resource_search_paths(SearchPaths& search_paths)
{
    search_paths.clear_explicit_paths();

    const bf::path root_path = Application::get_root_path();

    // OSL headers.
    search_paths.push_back_explicit_path((root_path / "shaders").string());
}

}   // namespace common
}   // namespace appleseed
