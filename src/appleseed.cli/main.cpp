
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

// appleseed.cli headers.
#include "commandlinehandler.h"
#include "stdouttilecallback.h"

// appleseed.common headers.
#include "application/application.h"
#include "application/progresstilecallback.h"
#include "application/superlogger.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/lighting.h"
#include "renderer/api/log.h"
#include "renderer/api/object.h"
#include "renderer/api/postprocessing.h"
#include "renderer/api/project.h"
#include "renderer/api/rendering.h"
#include "renderer/api/scene.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/log/log.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/console.h"
#include "foundation/platform/debugger.h"
#include "foundation/platform/thread.h"
#include "foundation/string/string.h"
#include "foundation/utility/benchmark.h"
#include "foundation/utility/filter.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/test.h"

// appleseed.main headers.
#include "main/allocator.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <memory>
#include <string>

using namespace appleseed::cli;
using namespace appleseed::common;
using namespace foundation;
using namespace renderer;
namespace bf = boost::filesystem;

namespace
{
    SuperLogger         g_logger;
    ParamArray          g_settings;
    CommandLineHandler  g_cl;

    template <typename Result>
    void print_suite_case_result(const Result& result)
    {
        const size_t suite_exec = result.get_suite_execution_count();
        const size_t suite_fail = result.get_suite_failure_count();
        const size_t case_exec = result.get_case_execution_count();
        const size_t case_fail = result.get_case_failure_count();

        LOG_INFO(
            g_logger,
            "  suites      : %s executed, %s failed (%s)\n"
            "  cases       : %s executed, %s failed (%s)",
            pretty_uint(suite_exec).c_str(),
            pretty_uint(suite_fail).c_str(),
            pretty_percent(suite_fail, suite_exec).c_str(),
            pretty_uint(case_exec).c_str(),
            pretty_uint(case_fail).c_str(),
            pretty_percent(case_fail, case_exec).c_str());
    }

    void print_unit_test_result(const TestResult& result)
    {
        LOG_INFO(g_logger, "unit testing summary:");
        print_suite_case_result(result);

        const size_t assert_exec = result.get_assertion_execution_count();
        const size_t assert_fail = result.get_assertion_failure_count();

        LOG_INFO(
            g_logger,
            "  assertions  : %s executed, %s failed (%s)",
            pretty_uint(assert_exec).c_str(),
            pretty_uint(assert_fail).c_str(),
            pretty_percent(assert_fail, assert_exec).c_str());
    }

    void print_unit_benchmark_result(const BenchmarkResult& result)
    {
        LOG_INFO(g_logger, "unit benchmarking summary:");
        print_suite_case_result(result);
    }

    bool run_unit_tests()
    {
        // Configure the renderer's logger: mute all log messages except warnings and errors.
        SaveLogFormatterConfig save_global_logger_config(global_logger());
        global_logger().set_all_formats(std::string());
        global_logger().reset_format(LogMessage::Warning);
        global_logger().reset_format(LogMessage::Error);
        global_logger().reset_format(LogMessage::Fatal);

        // Create a test listener that outputs to the logger.
        auto_release_ptr<ITestListener> listener(
            create_logger_test_listener(
                g_logger,
                g_cl.m_verbose_unit_tests.is_set()));

        // Change current directory to the tests' root directory.
        const bf::path old_current_path =
            Application::change_current_directory_to_tests_root_path();

        // Create unit tests output directories.
        if (!Application::create_unit_tests_output_directories())
        {
            LOG_ERROR(
                g_logger,
                "failed to create unit tests output directories %s, aborting test execution.",
                Application::get_unit_tests_output_path());
            return false;
        }

        TestResult result;

        // Run test suites.
        if (g_cl.m_run_unit_tests.values().empty())
            TestSuiteRepository::instance().run(listener.ref(), result);
        else
        {
            const char* regex = g_cl.m_run_unit_tests.value().c_str();
            const RegExFilter filter(regex, RegExFilter::CaseInsensitive);

            if (filter.is_valid())
                TestSuiteRepository::instance().run(filter, listener.ref(), result);
            else
            {
                LOG_ERROR(
                    g_logger,
                    "malformed regular expression '%s', disabling test filtering.",
                    regex);
                TestSuiteRepository::instance().run(listener.ref(), result);
            }
        }

        print_unit_test_result(result);

        // Restore the current directory.
        bf::current_path(old_current_path);

        return result.get_assertion_failure_count() == 0;
    }

    void run_unit_benchmarks()
    {
        // Configure the renderer's logger: mute all log messages except warnings and errors.
        SaveLogFormatterConfig save_global_logger_config(global_logger());
        global_logger().set_all_formats(std::string());
        global_logger().reset_format(LogMessage::Warning);
        global_logger().reset_format(LogMessage::Error);
        global_logger().reset_format(LogMessage::Fatal);

        BenchmarkResult result;

        // Add a benchmark listener that outputs to the logger.
        auto_release_ptr<IBenchmarkListener>
            logger_listener(create_logger_benchmark_listener(g_logger));
        result.add_listener(logger_listener.get());

        // Try to add a benchmark listener that outputs to a XML file.
        auto_release_ptr<XMLFileBenchmarkListener> xmlfile_listener(
            create_xmlfile_benchmark_listener());
        const std::string xmlfile_name = "benchmark." + get_time_stamp_string() + ".xml";
        const bf::path xmlfile_path =
              bf::path(Application::get_tests_root_path())
            / "unit benchmarks" / "results" / xmlfile_name;
        if (xmlfile_listener->open(xmlfile_path.string().c_str()))
            result.add_listener(xmlfile_listener.get());
        else
        {
            LOG_WARNING(
                g_logger,
                "automatic benchmark results archiving to %s failed: i/o error.",
                xmlfile_path.string().c_str());
        }

        const bf::path old_current_path =
            Application::change_current_directory_to_tests_root_path();

        // Run benchmark suites.
        if (g_cl.m_run_unit_benchmarks.values().empty())
            BenchmarkSuiteRepository::instance().run(result);
        else
        {
            const char* regex = g_cl.m_run_unit_benchmarks.value().c_str();
            const RegExFilter filter(regex, RegExFilter::CaseInsensitive);

            if (filter.is_valid())
                BenchmarkSuiteRepository::instance().run(filter, result);
            else
            {
                LOG_ERROR(
                    g_logger,
                    "malformed regular expression '%s', disabling benchmark filtering.",
                    regex);
                BenchmarkSuiteRepository::instance().run(result);
            }
        }

        // Restore the current directory.
        bf::current_path(old_current_path);

        // Print results.
        print_unit_benchmark_result(result);
    }

    void apply_rendering_settings_command_line_options(ParamArray& params)
    {
        if (g_cl.m_disable_autosave.is_set())
            params.insert_path("autosave", false);

        if (g_cl.m_threads.is_set())
        {
            params.insert_path(
                "rendering_threads",
                g_cl.m_threads.value());
        }

        if (g_cl.m_samples.is_set())
        {
            params.insert_path(
                "uniform_pixel_renderer.samples",
                g_cl.m_samples.value());
        }

        if (g_cl.m_passes.is_set())
        {
            params.insert_path(
                "passes",
                g_cl.m_passes.values()[0]);
        }

        if (g_cl.m_override_shading.is_set())
        {
            params.insert_path(
                "shading_engine.override_shading.mode",
                g_cl.m_override_shading.value());
        }
    }

    void apply_custom_parameter_command_line_options(ParamArray& params)
    {
        for (size_t i = 0; i < g_cl.m_params.values().size(); ++i)
        {
            // Retrieve the assignment string (of the form name=value).
            const std::string& s = g_cl.m_params.values()[i];

            // Retrieve the name and the value of the parameter.
            const std::string::size_type equal_pos = s.find_first_of('=');
            const std::string path = s.substr(0, equal_pos);
            const std::string value = s.substr(equal_pos + 1);

            // Insert the parameter.
            params.insert_path(path.c_str(), value);
        }
    }

    bool apply_frame_command_line_options(Project& project)
    {
        const Frame* frame = project.get_frame();
        assert(frame != nullptr);

        const ParamArray initial_params = frame->get_parameters();
        ParamArray new_params = initial_params;

        if (g_cl.m_resolution.is_set())
        {
            const std::string resolution =
                  foundation::to_string(g_cl.m_resolution.values()[0]) + ' ' +
                  foundation::to_string(g_cl.m_resolution.values()[1]);
            new_params.insert("resolution", resolution);
        }

        if (g_cl.m_window.is_set())
        {
            const std::string crop_window =
                  foundation::to_string(g_cl.m_window.values()[0]) + ' ' +
                  foundation::to_string(g_cl.m_window.values()[1]) + ' ' +
                  foundation::to_string(g_cl.m_window.values()[2]) + ' ' +
                  foundation::to_string(g_cl.m_window.values()[3]);
            new_params.insert("crop_window", crop_window);
        }

        if (g_cl.m_noise_seed.is_set())
        {
            const std::uint32_t noise_seed = static_cast<std::uint32_t>(g_cl.m_noise_seed.value());
            new_params.insert("noise_seed", noise_seed);
        }

        if (g_cl.m_output.is_set())
        {
            const char* file_path = g_cl.m_output.value().c_str();
            new_params.insert("output_path", file_path);
        }

        if (g_cl.m_checkpoint_create.is_set())
        {
            new_params.insert("checkpoint_create", true);

            if (g_cl.m_checkpoint_create.values().empty() && !g_cl.m_output.is_set())
            {
                LOG_ERROR(
                    g_logger,
                    "output path must be specified if no path is given for %s",
                    g_cl.m_checkpoint_create.get_name().c_str());
                return false;
            }

            new_params.insert(
                "checkpoint_create_path",
                !g_cl.m_checkpoint_create.values().empty()
                    ? g_cl.m_checkpoint_create.value()
                    : "");
        }

        if (g_cl.m_checkpoint_resume.is_set())
        {
            new_params.insert("checkpoint_resume", true);

            if (g_cl.m_checkpoint_resume.values().empty() && !g_cl.m_output.is_set())
            {
                LOG_ERROR(
                    g_logger,
                    "output path must be specified if no path is given for %s",
                    g_cl.m_checkpoint_resume.get_name().c_str());
                return false;
            }

            new_params.insert(
                "checkpoint_resume_path",
                !g_cl.m_checkpoint_resume.values().empty()
                    ? g_cl.m_checkpoint_resume.value()
                    : "");
        }

        if (g_cl.m_passes.is_set())
            new_params.insert_path("passes", g_cl.m_passes.values()[0]);

        if (new_params != initial_params)
        {
            LOG_DEBUG(
                g_logger,
                "command line parameters require frame \"%s\" to be recreated.",
                frame->get_name());

            auto_release_ptr<Frame> new_frame =
                FrameFactory::create(
                    frame->get_name(),
                    new_params,
                    frame->aovs(),
                    frame->lpe_aovs());

            // Recreate post-processing stages.
            for (PostProcessingStage& stage : frame->post_processing_stages())
            {
                const IPostProcessingStageFactory* stage_factory =
                    project.get_factory_registrar<PostProcessingStage>().lookup(stage.get_model());
                assert(stage_factory);
                new_frame->post_processing_stages().insert(
                    stage_factory->create(stage.get_name(), stage.get_parameters()));
            }

            project.set_frame(new_frame);
        }

        return true;
    }

    void apply_visibility_command_line_options_recursive(
        Assembly&           assembly,
        const RegExFilter&  show_filter,
        const RegExFilter&  hide_filter)
    {
        for (ObjectInstance& object_instance : assembly.object_instances())
        {
            if (!show_filter.accepts(object_instance.get_name()) ||
                hide_filter.accepts(object_instance.get_name()))
                object_instance.set_vis_flags(VisibilityFlags::Invisible);
        }

        for (Assembly& child_assembly : assembly.assemblies())
            apply_visibility_command_line_options_recursive(child_assembly, show_filter, hide_filter);
    }

    void apply_visibility_command_line_options(Project& project)
    {
        if (g_cl.m_show_object_instances.is_set() || g_cl.m_hide_object_instances.is_set())
        {
            const std::string show_regex =
                g_cl.m_show_object_instances.is_set()
                    ? g_cl.m_show_object_instances.value()
                    : ".*";     // match everything

            const std::string hide_regex =
                g_cl.m_hide_object_instances.is_set()
                    ? g_cl.m_hide_object_instances.value()
                    : "(?!)";   // match nothing

            const RegExFilter show_filter(show_regex.c_str(), RegExFilter::CaseSensitive);
            const RegExFilter hide_filter(hide_regex.c_str(), RegExFilter::CaseSensitive);

            for (Assembly& assembly : project.get_scene()->assemblies())
                apply_visibility_command_line_options_recursive(assembly, show_filter, hide_filter);
        }
    }

    bool apply_command_line_options(Project& project, ParamArray& params)
    {
        // Apply command line options that alter renderer settings.
        apply_rendering_settings_command_line_options(params);
        apply_custom_parameter_command_line_options(params);

        // Apply command line options that alter the project.
        if (!apply_frame_command_line_options(project))
            return false;
        apply_visibility_command_line_options(project);

        return true;
    }

#if defined __APPLE__ || defined _WIN32

    // Invoke a system command to open an image file.
    void display_frame(const std::string& path)
    {
        const std::string quoted_path = "\"" + path + "\"";

#if defined __APPLE__
        const std::string command = "open " + quoted_path;
#elif defined _WIN32
        const std::string command = quoted_path;
#endif

        LOG_DEBUG(g_logger, "executing '%s'", command.c_str());
        std::system(command.c_str());
    }

#endif

    auto_release_ptr<Project> load_project(const std::string& project_filepath)
    {
        // Construct the schema file path.
        const bf::path schema_filepath =
              bf::path(Application::get_root_path())
            / "schemas"
            / "project.xsd";

        // Load the project from disk.
        return
            ProjectFileReader::read(
                project_filepath.c_str(),
                schema_filepath.string().c_str());
    }

    bool configure_project(Project& project, ParamArray& params)
    {
        // Retrieve the name of the configuration to use.
        const std::string config_name = g_cl.m_configuration.is_set()
            ? g_cl.m_configuration.value()
            : "final";

        // Retrieve the configuration.
        const Configuration* configuration =
            project.configurations().get_by_name(config_name.c_str());
        if (configuration == nullptr)
        {
            LOG_ERROR(
                g_logger,
                "the configuration \"%s\" does not exist.",
                config_name.c_str());
            return false;
        }

        // Start with the parameters from the base configuration.
        if (configuration->get_base())
            params = configuration->get_base()->get_parameters();

        // Apply the application settings.
        params.merge(g_settings);

        // Merge the parameters from the configuration.
        params.merge(configuration->get_parameters());

        // Apply the command line options.
        return apply_command_line_options(project, params);
    }

    bool is_progressive_render(const ParamArray& params)
    {
        const std::string value = params.get_required<std::string>("frame_renderer", "generic");
        return value == "progressive";
    }

    bool render(const std::string& project_filename)
    {
        // Load the project.
        auto_release_ptr<Project> project = load_project(project_filename);
        if (project.get() == nullptr)
            return false;

        // Retrieve the rendering parameters.
        ParamArray params;
        if (!configure_project(project.ref(), params))
            return false;

        // Create the tile callback factory.
        std::unique_ptr<ITileCallbackFactory> tile_callback_factory;
        if (g_cl.m_send_to_stdout.is_set())
        {
            tile_callback_factory.reset(
                new StdOutTileCallbackFactory(
                    StdOutTileCallbackFactory::TileOutputOptions::AllAOVs));
        }
        else if (project->get_display() == nullptr)
        {
            // Create a default tile callback if needed.
            if (params.get_optional<std::string>("frame_renderer", "") != "progressive")
            {
                tile_callback_factory.reset(
                    new ProgressTileCallbackFactory(
                        global_logger(),
                        params.get_optional<size_t>("passes", 1)));
            }
        }

        SearchPaths resource_search_paths;
        Application::initialize_resource_search_paths(resource_search_paths);

        // Create the master renderer.
        DefaultRendererController renderer_controller;
        MasterRenderer renderer(
            project.ref(),
            params,
            resource_search_paths,
            tile_callback_factory.get());

        // Render the frame.
        LOG_INFO(g_logger, "rendering frame...");
        MasterRenderer::RenderingResult rendering_result;
        if (params.get_optional<bool>("background_mode", true))
        {
            ProcessPriorityContext background_context(ProcessPriorityLow, &g_logger);
            rendering_result = renderer.render(renderer_controller);
        }
        else
        {
            rendering_result = renderer.render(renderer_controller);
        }
        if (rendering_result.m_status != MasterRenderer::RenderingResult::Succeeded)
            return false;

        // Print rendering time.
        LOG_INFO(
            g_logger,
            "rendering finished in %s.",
            pretty_time(project->get_rendering_timer().get_seconds(), 3).c_str());

        bool success = true;

        // Optionally archive the frame to disk.
        char* archive_path = nullptr;
        if (params.get_optional<bool>("autosave", true))
        {
            // Construct the path to the archive directory.
            const bf::path autosave_path =
                  bf::path(Application::get_root_path())
                / "images" / "autosave";

            // Archive the frame to disk.
            LOG_INFO(g_logger, "archiving frame to disk...");
            if (!project->get_frame()->archive(
                    autosave_path.string().c_str(),
                    &archive_path))
                success = false;
        }

        // Optionally write the frame to disk.
        if (g_cl.m_output.is_set())
        {
            const char* file_path = g_cl.m_output.value().c_str();
            if (!project->get_frame()->write_main_image(file_path))
                success = false;
            if (!project->get_frame()->write_aov_images(file_path))
                success = false;
        }
        else
        {
            if (!project->get_frame()->write_main_and_aov_images())
                success = false;
        }

#if defined __APPLE__ || defined _WIN32

        // Display the output image.
        if (g_cl.m_display_output.is_set())
        {
            if (g_cl.m_output.is_set())
                display_frame(g_cl.m_output.value().c_str());
            else if (archive_path)
                display_frame(archive_path);
            else LOG_WARNING(g_logger, "cannot display output when no output is specified and autosave is disabled.");
        }

#endif

        // Deallocate the memory used by the path to the archived image.
        free_string(archive_path);

        // Optionally save recorded light paths to disk.
        if (g_cl.m_save_light_paths.is_set() &&
            project->get_light_path_recorder().get_light_path_count() > 0)
        {
            if (!project->get_light_path_recorder().write(g_cl.m_save_light_paths.value().c_str()))
                success = false;
        }

        return success;
    }

    bool benchmark_render(const std::string& project_filename)
    {
        // Configure our logger.
        SaveLogFormatterConfig save_g_logger_config(g_logger);
        g_logger.reset_all_formats();
        g_logger.set_format(LogMessage::Info, "{message}");

        // Configure the renderer's logger: mute all log messages except warnings and errors.
        SaveLogFormatterConfig save_global_logger_config(global_logger());
        global_logger().set_all_formats(std::string());
        global_logger().reset_format(LogMessage::Warning);
        global_logger().reset_format(LogMessage::Error);
        global_logger().reset_format(LogMessage::Fatal);

        // Load the project.
        auto_release_ptr<Project> project = load_project(project_filename);
        if (project.get() == nullptr)
            return false;

        // Figure out the rendering parameters.
        ParamArray params;
        if (!configure_project(project.ref(), params))
            return false;

        SearchPaths resource_search_paths;
        Application::initialize_resource_search_paths(resource_search_paths);

        // Create the master renderer.
        DefaultRendererController renderer_controller;
        MasterRenderer renderer(
            project.ref(),
            params,
            resource_search_paths);

        double total_time_seconds, render_time_seconds;
        {
            // Raise the process priority to reduce interruptions.
            ProcessPriorityContext benchmark_context(ProcessPriorityHigh, &g_logger);

            // Render a first time.
            auto result = renderer.render(renderer_controller);
            if (result.m_status != MasterRenderer::RenderingResult::Succeeded)
                return false;
            total_time_seconds = project->get_rendering_timer().get_seconds();

            // Render a second time.
            result = renderer.render(renderer_controller);
            if (result.m_status != MasterRenderer::RenderingResult::Succeeded)
                return false;
            render_time_seconds = project->get_rendering_timer().get_seconds();
        }

        // Write the frame to disk.
        if (g_cl.m_output.is_set())
        {
            const char* file_path = g_cl.m_output.value().c_str();
            project->get_frame()->write_main_image(file_path);
            project->get_frame()->write_aov_images(file_path);
        }

        // Print benchmark results.
        LOG_INFO(g_logger, "result=success");
        LOG_INFO(g_logger, "total_time=%.6f", total_time_seconds);
        LOG_INFO(g_logger, "setup_time=%.6f", total_time_seconds - render_time_seconds);
        LOG_INFO(g_logger, "render_time=%.6f", render_time_seconds);

        return true;
    }
}


//
// Entry point of appleseed.cli.
//

int main(int argc, char* argv[])
{
    // Enable memory tracking immediately as to catch as many leaks as possible.
    start_memory_tracking();

    // Make sure this build can run on this host.
    Application::check_compatibility_with_host(g_logger);

    // Make sure appleseed is correctly installed.
    Application::check_installation(g_logger);

    // Parse the command line.
    g_cl.parse(argc, argv, g_logger);

    // Load and apply settings from the settings file.
    Application::load_settings("appleseed.cli.xml", g_settings, g_logger);
    g_logger.configure_from_settings(g_settings);

    // Apply command line arguments.
    g_cl.apply(g_logger);

    // Configure the renderer's global logger.
    // Must be done after settings have been loaded and the command line
    // has been parsed, because these two operations may replace the log
    // target of the global logger.
    global_logger().initialize_from(g_logger);

    bool success = true;

    // Run unit tests.
    if (g_cl.m_run_unit_tests.is_set())
        success = success && run_unit_tests();

    // Run unit benchmarks.
    if (g_cl.m_run_unit_benchmarks.is_set())
        run_unit_benchmarks();

    // Render the specified project.
    if (!g_cl.m_filename.values().empty())
    {
        const std::string project_filename = g_cl.m_filename.value();

        if (g_cl.m_benchmark_mode.is_set())
            success = success && benchmark_render(project_filename);
        else success = success && render(project_filename);
    }

    const int return_code = success ? 0 : 1;
    LOG_DEBUG(g_logger, "returning code %d.", return_code);

    if (is_debugger_attached())
        Console::pause();

    return return_code;
}
