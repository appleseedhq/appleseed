
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

// appleseed.cli headers.
#include "commandlinehandler.h"
#include "continuoussavingtilecallback.h"
#include "houdinitilecallbacks.h"
#include "progresstilecallback.h"

// appleseed.renderer headers.
#include "renderer/api/color.h"
#include "renderer/api/frame.h"
#include "renderer/api/log.h"
#include "renderer/api/material.h"
#include "renderer/api/object.h"
#include "renderer/api/project.h"
#include "renderer/api/rendering.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/platform/thread.h"
#include "foundation/platform/timers.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/benchmark.h"
#include "foundation/utility/filter.h"
#include "foundation/utility/log.h"
#include "foundation/utility/settings.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"

// appleseed.shared headers.
#include "application/application.h"
#include "application/superlogger.h"

// appleseed.main headers.
#include "main/allocator.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cstddef>
#include <cstdlib>
#include <memory>
#include <string>

using namespace appleseed::cli;
using namespace appleseed::shared;
using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace
{
    SuperLogger         g_logger;
    CommandLineHandler  g_cl;
    ParamArray          g_settings;

    void load_settings()
    {
        const filesystem::path root_path(Application::get_root_path());
        const filesystem::path settings_file_path = root_path / "settings" / "appleseed.cli.xml";
        const filesystem::path schema_file_path = root_path / "schemas" / "settings.xsd";

        SettingsFileReader reader(g_logger);
        reader.read(
            settings_file_path.string().c_str(),
            schema_file_path.string().c_str(),
            g_settings);
    }

    void apply_settings()
    {
        if (g_settings.get_optional<bool>("message_coloring", false))
            g_logger.enable_message_coloring();
    }

    void configure_renderer_logger()
    {
        global_logger().add_target(&g_logger.get_log_target());

        for (size_t i = 0; i < LogMessage::NumMessageCategories; ++i)
        {
            const LogMessage::Category category = static_cast<LogMessage::Category>(i);
            global_logger().set_format(category, g_logger.get_format(category));
        }
    }

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
        // Configure our logger.
        SaveLogFormatterConfig save_g_logger_config(g_logger);
        g_logger.set_all_formats("{datetime-utc} | {message}");

        // Configure the renderer's logger: mute all log messages except warnings and errors.
        SaveLogFormatterConfig save_global_logger_config(global_logger());
        global_logger().set_all_formats(string());
        global_logger().reset_format(LogMessage::Warning);
        global_logger().reset_format(LogMessage::Error);
        global_logger().reset_format(LogMessage::Fatal);

        // Create a test listener that outputs to the logger.
        auto_release_ptr<ITestListener> listener(
            create_logger_test_listener(
                g_logger,
                g_cl.m_verbose_unit_tests.is_set()));

        TestResult result;

        const filesystem::path old_current_path =
            Application::change_current_directory_to_tests_root_path();

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

        // Restore the current directory.
        filesystem::current_path(old_current_path);

        print_unit_test_result(result);

        return result.get_assertion_failure_count() == 0;
    }

    void run_unit_benchmarks()
    {
        // Configure our logger.
        SaveLogFormatterConfig save_g_logger_config(g_logger);
        g_logger.set_all_formats("{datetime-utc} | {message}");

        // Configure the renderer's logger: mute all log messages except warnings and errors.
        SaveLogFormatterConfig save_global_logger_config(global_logger());
        global_logger().set_all_formats(string());
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
        const string xmlfile_name = "benchmark." + get_time_stamp_string() + ".xml";
        const filesystem::path xmlfile_path =
              filesystem::path(Application::get_tests_root_path())
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

        const filesystem::path old_current_path =
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
        filesystem::current_path(old_current_path);

        // Print results.
        print_unit_benchmark_result(result);
    }

    void set_frame_parameter(Project& project, const string& key, const string& value)
    {
        const Frame* frame = project.get_frame();
        assert(frame);

        ParamArray params = frame->get_parameters();
        params.insert(key, value);

        auto_release_ptr<Frame> new_frame(FrameFactory::create(frame->get_name(), params));

        project.set_frame(new_frame);
    }

    void apply_resolution_command_line_option(Project& project)
    {
        if (g_cl.m_resolution.is_set())
        {
            const string resolution =
                  foundation::to_string(g_cl.m_resolution.values()[0]) + ' ' +
                  foundation::to_string(g_cl.m_resolution.values()[1]);

            set_frame_parameter(project, "resolution", resolution);
        }
    }

    void apply_crop_window_command_line_option(Project& project)
    {
        if (g_cl.m_window.is_set())
        {
            const string crop_window =
                  foundation::to_string(g_cl.m_window.values()[0]) + ' ' +
                  foundation::to_string(g_cl.m_window.values()[1]) + ' ' +
                  foundation::to_string(g_cl.m_window.values()[2]) + ' ' +
                  foundation::to_string(g_cl.m_window.values()[3]);

            set_frame_parameter(project, "crop_window", crop_window);
        }
    }

    void apply_samples_command_line_option(ParamArray& params)
    {
        if (g_cl.m_samples.is_set())
        {
            params.insert_path(
                "uniform_pixel_renderer.samples",
                g_cl.m_samples.values()[1]);

            params.insert_path(
                "adaptive_pixel_renderer.min_samples",
                g_cl.m_samples.values()[0]);

            params.insert_path(
                "adaptive_pixel_renderer.max_samples",
                g_cl.m_samples.values()[1]);
        }
    }

    void apply_passes_command_line_option(ParamArray& params)
    {
        if (g_cl.m_passes.is_set())
        {
            params.insert_path(
                "generic_frame_renderer.passes",
                g_cl.m_passes.values()[0]);
        }
    }

    void apply_select_object_instances_command_line_option(Assembly& assembly, const RegExFilter& filter)
    {
        static const char* ColorName = "opaque_black-75AB13E8-D5A2-4D27-A64E-4FC41B55A272";
        static const char* BlackSurfaceShaderName = "opaque_black_surface_shader-75AB13E8-D5A2-4D27-A64E-4FC41B55A272";
        static const char* BlackMaterialName = "opaque_black_material-75AB13E8-D5A2-4D27-A64E-4FC41B55A272";

        ColorValueArray color_values;
        color_values.push_back(0.0f);

        assembly.colors().insert(
            ColorEntityFactory::create(
                ColorName,
                ParamArray()
                    .insert("color_space", "linear_rgb"),
                color_values));

        assembly.surface_shaders().insert(
            ConstantSurfaceShaderFactory().create(
                BlackSurfaceShaderName,
                ParamArray()
                    .insert("color", ColorName)));

        assembly.materials().insert(
            GenericMaterialFactory().create(
                BlackMaterialName,
                ParamArray()
                    .insert("surface_shader", BlackSurfaceShaderName)));

        for (each<ObjectInstanceContainer> i = assembly.object_instances(); i; ++i)
        {
            if (!filter.accepts(i->get_name()))
            {
                Object* object = i->find_object();
                const size_t slot_count = object->get_material_slot_count();

                for (size_t s = 0; s < slot_count; ++s)
                {
                    const char* slot = object->get_material_slot(s);
                    i->assign_material(slot, ObjectInstance::FrontSide, BlackMaterialName);
                    i->assign_material(slot, ObjectInstance::BackSide, BlackMaterialName);
                }
            }
        }

        for (each<AssemblyContainer> i = assembly.assemblies(); i; ++i)
            apply_select_object_instances_command_line_option(*i, filter);
    }

    void apply_select_object_instances_command_line_option(Project& project, const char* regex)
    {
        const RegExFilter filter(regex, RegExFilter::CaseSensitive);

        Scene* scene = project.get_scene();

        for (each<AssemblyContainer> i = scene->assemblies(); i; ++i)
            apply_select_object_instances_command_line_option(*i, filter);
    }

    void apply_parameter_command_line_options(ParamArray& params)
    {
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

    void apply_command_line_options(Project& project, ParamArray& params)
    {
        // Apply --disable-autosave option.
        if (g_cl.m_disable_autosave.is_set())
            params.insert_path("autosave", false);

        // Apply --threads option.
        if (g_cl.m_threads.is_set())
        {
            params.insert_path(
                "rendering_threads",
                g_cl.m_threads.value());
        }

        // Apply --resolution option.
        apply_resolution_command_line_option(project);

        // Apply --window option.
        apply_crop_window_command_line_option(project);

        // Apply --samples option.
        apply_samples_command_line_option(params);

        // Apply --passes option.
        apply_passes_command_line_option(params);

        // Apply --override-shading option.
        if (g_cl.m_override_shading.is_set())
        {
            params.insert_path(
                "shading_engine.override_shading.mode",
                g_cl.m_override_shading.value());
        }

        // Apply --select-object-instances option.
        if (g_cl.m_select_object_instances.is_set())
        {
            apply_select_object_instances_command_line_option(
                project,
                g_cl.m_select_object_instances.value().c_str());
        }

        // Apply --parameter options.
        apply_parameter_command_line_options(params);
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
#endif

        LOG_DEBUG(g_logger, "executing '%s'", command.c_str());
        std::system(command.c_str());   // needs std:: qualifier
    }

#endif

    auto_release_ptr<Project> load_project(const string& project_filepath)
    {
        // Construct the schema file path.
        const filesystem::path schema_filepath =
              filesystem::path(Application::get_root_path())
            / "schemas"
            / "project.xsd";

        // Load the project from disk.
        ProjectFileReader reader;
        return
            reader.read(
                project_filepath.c_str(),
                schema_filepath.string().c_str());
    }

    bool configure_project(Project& project, ParamArray& params)
    {
        // Retrieve the name of the configuration to use.
        const string config_name = g_cl.m_configuration.is_set()
            ? g_cl.m_configuration.value()
            : "final";

        // Retrieve the configuration.
        const Configuration* configuration =
            project.configurations().get_by_name(config_name.c_str());
        if (configuration == 0)
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
        apply_command_line_options(project, params);

        return true;
    }

    bool is_progressive_render(const ParamArray& params)
    {
        const string value = params.get_required<string>("frame_renderer", "generic");
        return value == "progressive";
    }

    void render(const string& project_filename)
    {
        // Load the project.
        auto_release_ptr<Project> project = load_project(project_filename);
        if (project.get() == 0)
            return;

        // Retrieve the rendering parameters.
        ParamArray params;
        if (!configure_project(project.ref(), params))
            return;

        // Create the tile callback factory.
        auto_ptr<ITileCallbackFactory> tile_callback_factory;
        if (g_cl.m_mplay_display.is_set())
        {
            tile_callback_factory.reset(
                new MPlayTileCallbackFactory(
                    project_filename.c_str(),
                    is_progressive_render(params),
                    g_logger));
        }
        else if (g_cl.m_hrmanpipe_display.is_set())
        {
            tile_callback_factory.reset(
                new HRmanPipeTileCallbackFactory(
                    g_cl.m_hrmanpipe_display.value(),
                    is_progressive_render(params),
                    g_logger));
        }
        else if (g_cl.m_output.is_set() && g_cl.m_continuous_saving.is_set())
        {
            tile_callback_factory.reset(
                new ContinuousSavingTileCallbackFactory(
                    g_cl.m_output.value().c_str(),
                    g_logger));
        }
        else
        {
            // Create a default tile callback if needed.
            if (project->get_display() == 0)
            {
                tile_callback_factory.reset(
                    new ProgressTileCallbackFactory(g_logger));
            }
        }

        // Create the master renderer.
        DefaultRendererController renderer_controller;
        MasterRenderer renderer(
            project.ref(),
            params,
            &renderer_controller,
            tile_callback_factory.get());

        // Render the frame.
        LOG_INFO(g_logger, "rendering frame...");
        Stopwatch<DefaultWallclockTimer> stopwatch;
        if (params.get_optional<bool>("background_mode", true))
        {
            ProcessPriorityContext background_context(
                ProcessPriorityLow,
                &g_logger);
            stopwatch.start();
            renderer.render();
            stopwatch.measure();
        }
        else
        {
            stopwatch.start();
            renderer.render();
            stopwatch.measure();
        }

        // Print rendering time.
        const double seconds = stopwatch.get_seconds();
        LOG_INFO(
            g_logger,
            "rendering finished in %s.",
            pretty_time(seconds, 3).c_str());

        // Archive the frame to disk.
        char* archive_path = 0;
        if (params.get_optional<bool>("autosave", true))
        {
            // Construct the path to the archive directory.
            const filesystem::path autosave_path =
                  filesystem::path(Application::get_root_path())
                / "images" / "autosave";

            // Archive the frame to disk.
            LOG_INFO(g_logger, "archiving frame to disk...");
            project->get_frame()->archive(
                autosave_path.string().c_str(),
                &archive_path);
        }

        // Write the frame to disk.
        if (g_cl.m_output.is_set() && !g_cl.m_continuous_saving.is_set())
        {
            LOG_INFO(g_logger, "writing frame to disk...");
            project->get_frame()->write_main_image(g_cl.m_output.value().c_str());
            project->get_frame()->write_aov_images(g_cl.m_output.value().c_str());
        }
        else
        {
            const Frame* frame = project->get_frame();
            const string output_filename =
                frame->get_parameters().get_optional<string>("output_filename");

            if (!output_filename.empty())
            {
                LOG_INFO(g_logger, "writing frame to disk...");
                frame->write_main_image(output_filename.c_str());

                if (frame->get_parameters().get_optional<bool>("output_aovs", false))
                    frame->write_aov_images(output_filename.c_str());
            }
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
    }

    void benchmark_render(const string& project_filename)
    {
        // Configure our logger.
        SaveLogFormatterConfig save_g_logger_config(g_logger);
        g_logger.reset_all_formats();
        g_logger.set_format(LogMessage::Info, "{message}");

        // Configure the renderer's logger: mute all log messages except warnings and errors.
        SaveLogFormatterConfig save_global_logger_config(global_logger());
        global_logger().set_all_formats(string());
        global_logger().reset_format(LogMessage::Warning);
        global_logger().reset_format(LogMessage::Error);
        global_logger().reset_format(LogMessage::Fatal);

        // Load the project.
        auto_release_ptr<Project> project = load_project(project_filename);
        if (project.get() == 0)
            return;

        // Figure out the rendering parameters.
        ParamArray params;
        if (!configure_project(project.ref(), params))
            return;

        // Create the master renderer.
        DefaultRendererController renderer_controller;
        MasterRenderer renderer(
            project.ref(),
            params,
            &renderer_controller);

        // Start the stopwatch.
        Stopwatch<DefaultWallclockTimer> stopwatch;
        stopwatch.start();

        // Render a first time.
        if (!renderer.render())
            return;
        stopwatch.measure();
        const double total_time_seconds = stopwatch.get_seconds();

        // Render a second time.
        if (!renderer.render())
            return;
        stopwatch.measure();
        const double render_time_seconds = stopwatch.get_seconds() - total_time_seconds;

        // Write the frame to disk.
        if (g_cl.m_output.is_set())
        {
            const char* file_path = g_cl.m_output.value().c_str();
            project->get_frame()->write_main_image(file_path);
            project->get_frame()->write_aov_images(file_path);
        }

        // Force-unload the project.
        project.reset();

        // Print benchmark results.
        LOG_INFO(g_logger, "result=success");
        LOG_INFO(g_logger, "setup_time=%.6f", total_time_seconds - render_time_seconds);
        LOG_INFO(g_logger, "render_time=%.6f", render_time_seconds);
        LOG_INFO(g_logger, "total_time=%.6f", total_time_seconds);
    }
}


//
// Entry point of appleseed.cli.
//

int main(int argc, const char* argv[])
{
    start_memory_tracking();

    Application::check_installation(g_logger);

    // Load and apply settings from the settings file.
    load_settings();
    apply_settings();

    // Parse the command line.
    g_cl.parse(argc, argv, g_logger);

    // Configure the renderer's global logger.
    configure_renderer_logger();

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
        const string project_filename = g_cl.m_filename.value();

        if (g_cl.m_benchmark_mode.is_set())
            benchmark_render(project_filename);
        else render(project_filename);
    }

    return success ? 0 : 1;
}
