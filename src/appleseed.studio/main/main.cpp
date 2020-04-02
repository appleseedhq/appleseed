
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

// appleseed.studio headers.
#include "commandlinehandler.h"
#include "mainwindow/mainwindow.h"
#include "python/pythoninterpreter.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.common headers.
#include "application/application.h"
#include "application/superlogger.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/log/log.h"
#include "foundation/platform/path.h"
#include "foundation/platform/python.h"
#include "foundation/utility/preprocessor.h"

// appleseed.main headers.
#include "main/allocator.h"

// Qt headers.
#include <QApplication>
#include <QImageReader>
#include <QLocale>
#include <QMessageBox>
#include <QString>
#include <QSurfaceFormat>

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <clocale>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <sstream>
#include <string>

using namespace appleseed::studio;
using namespace appleseed::common;
using namespace appleseed::qtcommon;
using namespace foundation;
namespace bf = boost::filesystem;

namespace
{
    // On Windows, messages emitted through this logger won't be visible unless appleseed.studio's subsystem
    // is set to Console (in appleseed.studio properties under Linker / System / SubSystem).
    SuperLogger g_logger;

    void check_compatibility()
    {
        const char* missing_feature = nullptr;
        if (!Application::is_compatible_with_host(&missing_feature))
        {
            QMessageBox msgbox;
            msgbox.setWindowTitle("System Incompatibility");
            msgbox.setIcon(QMessageBox::Critical);
            msgbox.setText(QString("This executable requires a CPU with %1 support.").arg(missing_feature));
            msgbox.setStandardButtons(QMessageBox::Ok);
            msgbox.setDefaultButton(QMessageBox::Ok);
            msgbox.exec();

            exit(EXIT_FAILURE);
        }
    }

    void check_installation()
    {
        if (!Application::is_correctly_installed())
        {
            QMessageBox msgbox;
            msgbox.setWindowTitle("Application Incorrectly Installed");
            msgbox.setIcon(QMessageBox::Critical);
            msgbox.setText(
                "The application failed to start because it is not properly installed. "
                "Please reinstall the application.");
            msgbox.setStandardButtons(QMessageBox::Ok);
            msgbox.setDefaultButton(QMessageBox::Ok);
            msgbox.exec();

            exit(EXIT_FAILURE);
        }
    }

    void configure_python()
    {
        const char* python_home_var = getenv("PYTHONHOME");

        if (python_home_var != nullptr && strlen(python_home_var) > 0)
        {
            // PYTHONHOME is set: the embedded Python interpreter will use the Python installation
            // the variable points to.
            LOG_INFO(
                g_logger,
                "PYTHONHOME environment variable set to %s: embedded Python interpreter will use "
                "the Python installation expected to exist at this path.",
                python_home_var);
        }
        else
        {
            // PYTHONHOME not set or empty: use the Python installation bundled with appleseed.studio.

#if defined _WIN32 || __APPLE__
            // On Windows and macOS, Python's standard libraries are located in python27/Lib/.
            bf::path python_path = bf::path(Application::get_root_path()) / "python27";
#else
            // On Linux, Python's standard libraries are located in lib/python2.7/.
            bf::path python_path = bf::path(Application::get_root_path());
#endif

            if (bf::is_directory(python_path))
            {
                const std::string python_path_str = safe_weakly_canonical(python_path).string();

                // The C string below must be declared static because Python just keeps a pointer to it.
                static char python_home[FOUNDATION_MAX_PATH_LENGTH + 1];
                assert(python_path_str.size() <= FOUNDATION_MAX_PATH_LENGTH);
                std::strncpy(python_home, python_path_str.c_str(), sizeof(python_home) - 1);

                LOG_INFO(
                    g_logger,
                    "PYTHONHOME environment variable not set or empty: embedded Python interpreter "
                    "will use Python installation expected to exist in %s.",
                    python_home);

                Py_SetPythonHome(python_home);
            }
            else
            {
                const std::string python_path_str = python_path.make_preferred().string();

                QMessageBox msgbox;
                msgbox.setWindowTitle("Python 2.7 Installation Not Found");
                msgbox.setIcon(QMessageBox::Critical);
                msgbox.setText(
                    QString(
                        "No Python 2.7 installation could be found in %1 where appleseed.studio expects one "
                        "to be, and the PYTHONHOME environment variable is not defined or is empty. "
                        "appleseed.studio may not work satisfactorily.").arg(QString::fromStdString(python_path_str)));
                msgbox.setStandardButtons(QMessageBox::Ok);
                msgbox.setDefaultButton(QMessageBox::Ok);
                set_minimum_width(msgbox, 600);
                msgbox.exec();
            }
        }
    }

    bool load_file(const std::string& filename, std::string& contents)
    {
        std::ifstream file(filename.c_str());

        if (!file.is_open())
            return false;

        std::stringstream sstr;
        sstr << file.rdbuf();
        contents = sstr.str();

        return true;
    }

    bool load_stylesheet(const std::string& stylesheet_path, std::string& stylesheet)
    {
        if (!load_file(stylesheet_path, stylesheet))
        {
            QMessageBox msgbox;
            msgbox.setWindowTitle("Failed to Load Default Stylesheet");
            msgbox.setIcon(QMessageBox::Warning);
            msgbox.setText(
                QString(
                    "The stylesheet %1 could not be loaded.\n\n"
                    "The application will use the default style.")
                    .arg(QString::fromStdString(safe_weakly_canonical(stylesheet_path).string())));
            msgbox.setStandardButtons(QMessageBox::Ok);
            msgbox.setDefaultButton(QMessageBox::Ok);
            msgbox.exec();

            return false;
        }

        Preprocessor preprocessor;

#ifdef _WIN32
        preprocessor.define_symbol("_WIN32");
#endif
#ifdef __APPLE__
        preprocessor.define_symbol("__APPLE__");
#endif
#ifdef __linux__
        preprocessor.define_symbol("__linux__");
#endif
#ifdef __FreeBSD__
        preprocessor.define_symbol("__FreeBSD__");
#endif

        preprocessor.process(stylesheet.c_str());

        if (preprocessor.failed())
        {
            QMessageBox msgbox;
            msgbox.setWindowTitle("Failed to Process Default Stylesheet");
            msgbox.setIcon(QMessageBox::Warning);
            msgbox.setText(
                QString("An error occurred while processing the stylesheet %1.\n\n"
                        "The application will use the default style.")
                    .arg(QString::fromStdString(stylesheet_path)));
            msgbox.setDetailedText(
                QString("Line %1: %3.")
                    .arg(preprocessor.get_error_location())
                    .arg(QString::fromStdString(preprocessor.get_error_message())));
            msgbox.setStandardButtons(QMessageBox::Ok);
            msgbox.setDefaultButton(QMessageBox::Ok);

            return false;
        }

        stylesheet = preprocessor.get_processed_text();

        return true;
    }

    void set_default_stylesheet(QApplication& application)
    {
        // Build the path to the default stylesheet file.
        const bf::path stylesheet_path =
              bf::path(Application::get_root_path())
            / "stylesheets"
            / "default.qss";

        // Load and apply the stylesheet.
        std::string stylesheet;
        if (load_stylesheet(stylesheet_path.string(), stylesheet))
        {
            application.setStyle("plastique");
            application.setStyleSheet(QString::fromStdString(stylesheet));
        }
    }

    QtMessageHandler g_previous_message_handler = nullptr;

    void message_handler(QtMsgType type, const QMessageLogContext& context, const QString& msg)
    {
#ifdef __APPLE__
        // Under certain circumstances (under an macOS virtual machine?), a bogus warning
        // message is repeatedly printed to the console. Disable this warning message.
        // See https://github.com/appleseedhq/appleseed/issues/254 for details.
        if (type == QtWarningMsg &&
            msg == "QCocoaView handleTabletEvent: This tablet device is unknown (received no proximity event for it). Discarding event.")
        {
            // Absorb the message.
            return;
        }
#endif

        // On Windows, there is a default message handler.
        if (g_previous_message_handler != nullptr)
        {
            g_previous_message_handler(type, context, msg);
            return;
        }

        switch (type)
        {
          case QtDebugMsg:
            fprintf(stderr, "Debug: %s\n", msg.toUtf8().constData());
            break;

          case QtWarningMsg:
            fprintf(stderr, "Warning: %s\n", msg.toUtf8().constData());
            break;

          case QtCriticalMsg:
            fprintf(stderr, "Critical: %s\n", msg.toUtf8().constData());
            break;

          case QtFatalMsg:
            fprintf(stderr, "Fatal: %s\n", msg.toUtf8().constData());
            abort();
        }
    }
}


//
// Entry point of appleseed.studio.
//

int main(int argc, char* argv[])
{
    // Enable memory tracking immediately as to catch as many leaks as possible.
    start_memory_tracking();

    // Our message handler must be set before the construction of QApplication.
    g_previous_message_handler = qInstallMessageHandler(message_handler);

    // Set default surface format before creating application instance. This is
    // required on macOS in order to use an OpenGL Core profile context.
    QSurfaceFormat default_format;
    default_format.setVersion(3, 3);
    default_format.setProfile(QSurfaceFormat::CoreProfile);
    QSurfaceFormat::setDefaultFormat(default_format);

    // Construct the Qt application.
    QApplication application(argc, argv);
    QApplication::setOrganizationName("appleseedhq");
    QApplication::setOrganizationDomain("appleseedhq.net");
    QApplication::setApplicationName("appleseed.studio");
    QApplication::setApplicationVersion(Appleseed::get_lib_version());
    QApplication::setWindowIcon(QIcon(make_app_path("icons/appleseed.png")));
    application.setAttribute(Qt::AA_DontUseNativeMenuBar, true);

    // The locale must be set after the construction of QApplication.
    QLocale::setDefault(QLocale::C);

    // Qt changes the locale when loading images from disk for the very first time.
    // The problem was tracked for both `QImage` and `QPixmap`: in their `load()`
    // functions, both classes call `QImageReader::read()` which causes the locale
    // to be changed to the system's one. The line that follows is a dirty fix that
    // consists in loading an image (any image) at the very beginning and resetting
    // the locale right after, thus preventing `QImageReader::read()` from changing
    // it again (as it happens only on the very first `read()`). Issue reported and
    // tracked on appleseed's GitHub under reference #1435.
    QImageReader(make_app_path("icons/icon.png")).read();   // any image

    // Force linking of resources provided by appleseed.qtcommon into the final binary.
    // See https://doc.qt.io/qt-5/resources.html#using-resources-in-a-library for details.
    Q_INIT_RESOURCE(qtcommonresources);

    // Make sure this build can run on this host.
    check_compatibility();

    // Make sure appleseed is correctly installed.
    check_installation();

    // Configure the embedded Python interpreter.
    configure_python();

    // Parse the command line.
    CommandLineHandler cl;
    cl.parse(argc, argv);

    // Configure the application to use our default stylesheet file.
    set_default_stylesheet(application);

    // Create the application's main window.
    MainWindow window;

    // QApplication and QMainWindow reset the C locale to the user's locale.
    // Fix this by setting the C locale back to "C" which is the startup locale.
    // Note that setting the C locale once at the start of the application is enough:
    // all modules link dynamically against the C runtime library so they will all
    // use this locale.
    std::setlocale(LC_ALL, "C");

    // Initialize the python interpreter and load plugins.
    PythonInterpreter::instance().set_main_window(&window);
    PythonInterpreter::instance().load_plugins();

    // If a project file was specified on the command line, open it and optionally start rendering.
    if (!cl.m_filename.values().empty())
    {
        const QString filename = QString::fromStdString(cl.m_filename.value());

        if (cl.m_render.is_set())
        {
            const QString configuration = QString::fromStdString(cl.m_render.value());
            window.open_and_render_project(filename, configuration);
        }
        else
        {
            window.open_project_async(filename);
        }
    }

    // Show the application's main window.
    window.show();

    return application.exec();
}
