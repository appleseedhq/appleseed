
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

// appleseed.cli headers.
#include "commandline.h"
#include "superlogger.h"

// appleseed.shared headers.
#include "application/application.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/paramarray.h"
#include "renderer/api/project.h"
#include "renderer/api/rendering.h"

// appleseed.foundation headers.
#include "foundation/platform/path.h"
#include "foundation/platform/timer.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/benchmark.h"
#include "foundation/utility/filter.h"
#include "foundation/utility/log.h"
#include "foundation/utility/settings.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"

// boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <memory>
#include <string>
#include <vector>

using namespace appleseed::cli;
using namespace appleseed::shared;
using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace
{

    CommandLine     g_cl;
    ParamArray      g_settings;

    void load_settings(Logger& logger)
    {
        const filesystem::path root_path(Application::get_root_path());
        const filesystem::path settings_file_path = root_path / "settings/appleseed.cli.xml";
        const filesystem::path schema_file_path = root_path / "schemas/settings.xsd";

        SettingsFileReader reader(logger);
        reader.read(
            settings_file_path.file_string().c_str(),
            schema_file_path.file_string().c_str(),
            g_settings);
    }

    template <typename Result>
    void print_suite_case_result(Logger& logger, const Result& result)
    {
        const size_t suite_exec = result.get_suite_execution_count();
        const size_t suite_fail = result.get_suite_failure_count();
        const size_t case_exec = result.get_case_execution_count();
        const size_t case_fail = result.get_case_failure_count();
  
        FOUNDATION_LOG_INFO(
            logger,
            "  suites      : %s executed, %s failed (%s)\n"
            "  cases       : %s executed, %s failed (%s)\n",
            pretty_uint(suite_exec).c_str(),
            pretty_uint(suite_fail).c_str(),
            pretty_percent(suite_fail, suite_exec).c_str(),
            pretty_uint(case_exec).c_str(),
            pretty_uint(case_fail).c_str(),
            pretty_percent(case_fail, case_exec).c_str());
    }

    void print_unit_test_result(Logger& logger, const TestResult& result)
    {
        FOUNDATION_LOG_INFO(logger, "unit testing summary:\n");
        print_suite_case_result(logger, result);

        const size_t assert_exec = result.get_assertion_execution_count();
        const size_t assert_fail = result.get_assertion_failure_count();

        FOUNDATION_LOG_INFO(
            logger,
            "  assertions  : %s executed, %s failed (%s)\n",
            pretty_uint(assert_exec).c_str(),
            pretty_uint(assert_fail).c_str(),
            pretty_percent(assert_fail, assert_exec).c_str());
    }

    void print_unit_benchmark_result(Logger& logger, const BenchmarkResult& result)
    {
        FOUNDATION_LOG_INFO(logger, "unit benchmarking summary:\n");
        print_suite_case_result(logger, result);
    }

    // Run unit tests.
    void run_unit_tests(Logger& logger)
    {
        // Create a test listener that outputs to the logger.
        auto_release_ptr<ITestListener> listener(
            create_logger_test_listener(
                logger,
                g_cl.m_verbose_unit_tests.found()));

        TestResult result;

        const filesystem::path old_current_path =
            Application::change_current_directory_to_tests_root_path();

        // Run test suites.
        if (g_cl.m_run_unit_tests.values().empty())
            TestSuiteRepository::instance().run(*listener.get(), result);
        else
        {
            const char* regex = g_cl.m_run_unit_tests.values().front().c_str();
            const RegExFilter filter(regex, RegExFilter::CaseInsensitive);

            if (filter.is_valid())
                TestSuiteRepository::instance().run(filter, *listener.get(), result);
            else
            {
                FOUNDATION_LOG_ERROR(
                    logger,
                    "malformed regular expression '%s', disabling test filtering",
                    regex);
                TestSuiteRepository::instance().run(*listener.get(), result);
            }
        }

        // Restore the current directory.
        filesystem::current_path(old_current_path);

        print_unit_test_result(logger, result);
    }

    class XMLBenchmarkFileProvider
    {
      public:
        XMLBenchmarkFileProvider()
        {
            m_file = fopen(get_file_path().c_str(), "wt");
        }

        ~XMLBenchmarkFileProvider()
        {
            if (m_file)
                fclose(m_file);
        }

        string get_file_path() const
        {
            const string filename =
                "benchmark." + get_time_stamp_string() + ".xml";

            const filesystem::path path =
                  filesystem::path(Application::get_tests_root_path())
                / "benchmarks/"
                / filename;

            return path.file_string();
        }

        FILE* get_file()
        {
            return m_file;
        }

      private:
        FILE*   m_file;
    };

    // Run unit benchmarks.
    void run_unit_benchmarks(Logger& logger)
    {
        // Create a benchmark listener that outputs to the logger.
        auto_release_ptr<IBenchmarkListener>
            logger_listener(create_logger_benchmark_listener(logger));

        // Create a benchmark listener that outputs to a XML benchmark file.
        XMLBenchmarkFileProvider benchmark_file_provider;
        IBenchmarkListener* xmlfile_listener;
        if (benchmark_file_provider.get_file())
        {
            xmlfile_listener =
                create_xmlfile_benchmark_listener(
                    benchmark_file_provider.get_file());
        }
        else
        {
            RENDERER_LOG_WARNING(
                "automatic benchmark results archiving to %s failed: i/o error",
                benchmark_file_provider.get_file_path().c_str());
            xmlfile_listener = 0;
        }

        // Create a benchmark result.
        BenchmarkResult result;
        result.add_listener(logger_listener.get());
        if (xmlfile_listener)
            result.add_listener(xmlfile_listener);

        const filesystem::path old_current_path =
            Application::change_current_directory_to_tests_root_path();

        // Run benchmark suites.
        if (g_cl.m_run_unit_benchmarks.values().empty())
            BenchmarkSuiteRepository::instance().run(result);
        else
        {
            const char* regex = g_cl.m_run_unit_benchmarks.values().front().c_str();
            const RegExFilter filter(regex, RegExFilter::CaseInsensitive);

            if (filter.is_valid())
                BenchmarkSuiteRepository::instance().run(filter, result);
            else
            {
                FOUNDATION_LOG_ERROR(
                    logger,
                    "malformed regular expression '%s', disabling benchmark filtering",
                    regex);
                BenchmarkSuiteRepository::instance().run(result);
            }
        }

        // Restore the current directory.
        filesystem::current_path(old_current_path);

        // Print results.
        print_unit_benchmark_result(logger, result);

        // Destruct the XMLFileBenchmarkListener before the file gets closed.
        if (xmlfile_listener)
            xmlfile_listener->release();
    }

    // Apply command line options to a given project.
    void apply_command_line_options(ParamArray& params)
    {
        // Apply rendering threads option.
        if (g_cl.m_rendering_threads.found())
        {
            params.insert(
                "generic_frame_renderer.rendering_threads",
                g_cl.m_rendering_threads.string_values()[0].c_str());
        }

        // Apply window option.
        if (g_cl.m_window.found())
        {
            const string window =
                  g_cl.m_window.string_values()[0] + ' ' +
                  g_cl.m_window.string_values()[1] + ' ' +
                  g_cl.m_window.string_values()[2] + ' ' +
                  g_cl.m_window.string_values()[3];
            params.insert("generic_tile_renderer.crop_window", window);
        }

        // Apply samples option.
        if (g_cl.m_samples.found())
        {
            params.insert(
                "generic_tile_renderer.min_samples",
                g_cl.m_samples.string_values()[0].c_str());
            params.insert(
                "generic_tile_renderer.max_samples",
                g_cl.m_samples.string_values()[1].c_str());
        }

        // Apply shading override option.
        if (g_cl.m_override_shading.found())
        {
            params.insert(
                "shading_engine.override_shading.mode",
                g_cl.m_override_shading.string_values()[0].c_str());
        }

        // Apply custom parameters.
        for (size_t i = 0; i < g_cl.m_params.values().size(); ++i)
        {
            // Retrieve the assignment string (of the form name=value).
            const string& s = g_cl.m_params.values()[i];

            // Retrieve the name and the value of the parameter.
            const string::size_type equal_pos = s.find_first_of('=');
            const string path = s.substr(0, equal_pos);
            const string value = s.substr(equal_pos + 1);

            // Insert the parameter.
            params.insert_path(path, value);
        }
    }

    void apply_resolution_command_line_option(Project* project)
    {
        const Frame* frame = project->get_frame();
        assert(frame);

        const string frame_name = frame->get_name();
        ParamArray frame_params = frame->get_parameters();

        if (g_cl.m_resolution.found())
        {
            const string resolution =
                  g_cl.m_resolution.string_values()[0] + ' ' +
                  g_cl.m_resolution.string_values()[1];
            frame_params.insert("resolution", resolution);
        }

        auto_ptr<Frame> new_frame(
            new Frame(frame_name.c_str(), frame_params));

        project->set_frame(new_frame);
    }

#if defined __APPLE__ || defined _WIN32

    // Invoke a system command to open an image file.
    void display_frame(const string& path)
    {
        const string quoted_path = "\"" + path + "\"";

#if defined __APPLE__
        const string command = "open " + quoted_path;
#elif defined _WIN32
        const string command = quoted_path;
#else
#error Unsupported platform.
#endif

        RENDERER_LOG_DEBUG("executing '%s'", command.c_str());
        std::system(command.c_str());   // needs std:: qualifier
    }

#endif

    // Render one frame of a given project.
    void render_frame(
        Project&            project,
        const ParamArray&   params)
    {
        RENDERER_LOG_INFO("rendering frame...");

        // Create the master renderer.
        DefaultRendererController renderer_controller;
        MasterRenderer renderer(
            project,
            params,
            MasterRenderer::RenderOnce,
            &renderer_controller);

        // Render the frame.
        Stopwatch<DefaultWallclockTimer> stopwatch;
        stopwatch.start();
        renderer.render();
        stopwatch.measure();

        // Print rendering time.
        const double seconds = stopwatch.get_seconds();
        RENDERER_LOG_INFO(
            "rendering finished in %s",
            pretty_time(seconds, 3).c_str());

        // Construct the path to the archive directory.
        const filesystem::path autosave_path =
              filesystem::path(Application::get_root_path())
            / "images/autosave/";

        // Archive the frame to disk.
        RENDERER_LOG_INFO("archiving frame to disk...");
        char* archive_path;
        project.get_frame()->archive(
            autosave_path.directory_string().c_str(),
            &archive_path);

        // Write the frame to disk.
        if (g_cl.m_output.found())
        {
            RENDERER_LOG_INFO("writing frame to disk...");
            project.get_frame()->write(g_cl.m_output.values()[0].c_str());
        }

#if defined __APPLE__ || defined _WIN32

        // Display the output image.
        if (g_cl.m_display_output.found())
            display_frame(archive_path);

#endif

        // Deallocate the memory used by the path to the archived image.
        delete [] archive_path;
    }

    // Render a given project.
    void render_project(const string& project_filename)
    {
        auto_release_ptr<Project> project;

        const string builtin_prefix = "builtin:";
        if (project_filename.substr(0, builtin_prefix.size()) == builtin_prefix)
        {
            // Load the built-in project.
            ProjectFileReader reader;
            const string name = project_filename.substr(builtin_prefix.size());
            project = reader.load_builtin(name.c_str());
        }
        else
        {
            // Construct the schema filename.
            const filesystem::path schema_path =
                  filesystem::path(Application::get_root_path())
                / "schemas/project.xsd";

            // Load the project from disk.
            ProjectFileReader reader;
            project =
                reader.read(
                    project_filename.c_str(),
                    schema_path.file_string().c_str());
        }

        // Skip this project if loading failed.
        if (project.get() == 0)
            return;

        // Retrieve the name of the configuration to use.
        const string config_name = g_cl.m_configuration.found()
            ? g_cl.m_configuration.values()[0]
            : "final";

        // Retrieve the configuration.
        const Configuration* configuration =
            project->configurations().get(config_name.c_str());
        if (configuration == 0)
        {
            RENDERER_LOG_ERROR(
                "the configuration \"%s\" does not exist",
                config_name.c_str());
            return;
        }

        // Retrieve the parameters from the configuration.
        ParamArray params;
        if (configuration->get_base())
            params = configuration->get_base()->get_parameters();
        params.merge(g_settings);
        params.merge(configuration->get_parameters());

        // Apply command line options.
        apply_command_line_options(params);
        apply_resolution_command_line_option(project.get());

        // Render one frame of the project.
        render_frame(*project, params);
    }

}   // anonymous namespace


//
// Entry point of appleseed.cli.
//

int main(int argc, const char* argv[])
{
    SuperLogger logger;

    logger.get_log_target().set_formatting_flags(LogMessage::DisplayMessage);

    // Make sure the application is properly installed, bail out if not.
    Application::check_installation(logger);

    // Parse the command line.
    g_cl.parse(argc, argv, logger);

    // Read the application's settings from disk.
    load_settings(logger);

    if (g_settings.get_optional<bool>("message_coloring", false))
        logger.enable_message_coloring();

    // Run unit tests.
    if (g_cl.m_run_unit_tests.found())
        run_unit_tests(logger);

    // Run unit benchmarks.
    if (g_cl.m_run_unit_benchmarks.found())
        run_unit_benchmarks(logger);

    logger.get_log_target().reset_formatting_flags();

    global_logger().add_target(&logger.get_log_target());

    // Render the specified project.
    if (!g_cl.m_filenames.values().empty())
        render_project(g_cl.m_filenames.values().front());

    return 0;
}
