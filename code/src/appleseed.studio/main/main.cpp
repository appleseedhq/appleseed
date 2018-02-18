
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "utility/miscellaneous.h"

// appleseed.shared headers.
#include "application/application.h"
#include "application/superlogger.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/platform/path.h"
#include "foundation/utility/log.h"
#include "foundation/utility/preprocessor.h"

// appleseed.main headers.
#include "main/allocator.h"

// Qt headers.
#include <QApplication>
#include <QLocale>
#include <QMessageBox>
#include <QString>
#include <QTextStream>

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <clocale>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <sstream>
#include <string>

// Qt headers
#include <QImageReader>

using namespace appleseed::studio;
using namespace appleseed::shared;
using namespace foundation;
using namespace std;
namespace bf = boost::filesystem;

namespace
{
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

#ifdef _WIN32
        // If the PYTHONHOME environment variable is defined, use the Python installation it points to.
        // If it is not defined, use the bundled Python installation.
        if (getenv("PYTHONHOME") == nullptr)
        {
            const string python_path =
                bf::canonical(
                    bf::path(Application::get_root_path()) / "python27"
                ).make_preferred().string();

            static char python_home[FOUNDATION_MAX_PATH_LENGTH + 1];

            assert(python_path.size() <= FOUNDATION_MAX_PATH_LENGTH);
            strncpy(python_home, python_path.c_str(), sizeof(python_home) - 1);

            Py_SetPythonHome(python_home);
        }
#endif
    }

    bool load_file(const string& filename, string& contents)
    {
        ifstream file(filename.c_str());

        if (!file.is_open())
            return false;

        stringstream sstr;
        sstr << file.rdbuf();
        contents = sstr.str();

        return true;
    }

    bool load_stylesheet(const string& stylesheet_path, string& stylesheet)
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
                    .arg(QString::fromStdString(stylesheet_path)));
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
        string stylesheet;
        if (load_stylesheet(stylesheet_path.string(), stylesheet))
        {
            application.setStyle("plastique");
            application.setStyleSheet(QString::fromStdString(stylesheet));
        }
    }

    QtMsgHandler g_previous_message_handler = nullptr;

    void message_handler(QtMsgType type, const char* msg)
    {
#ifdef __APPLE__
        // Under certain circumstances (under an macOS virtual machine?), a bogus warning
        // message is repeatedly printed to the console. Disable this warning message.
        // See https://github.com/appleseedhq/appleseed/issues/254 for details.
        if (type == QtWarningMsg &&
            strcmp(msg, "QCocoaView handleTabletEvent: This tablet device is unknown (received no proximity event for it). Discarding event.") == 0)
        {
            // Absorb the message.
            return;
        }
#endif

        // On Windows, there is a default message handler.
        if (g_previous_message_handler != nullptr)
        {
            g_previous_message_handler(type, msg);
            return;
        }

        switch (type)
        {
          case QtDebugMsg:
            fprintf(stderr, "Debug: %s\n", msg);
            break;

          case QtWarningMsg:
            fprintf(stderr, "Warning: %s\n", msg);
            break;

          case QtCriticalMsg:
            fprintf(stderr, "Critical: %s\n", msg);
            break;

          case QtFatalMsg:
            fprintf(stderr, "Fatal: %s\n", msg);
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
    g_previous_message_handler = qInstallMsgHandler(message_handler);

    QApplication application(argc, argv);
    QApplication::setOrganizationName("appleseedhq");
    QApplication::setOrganizationDomain("appleseedhq.net");
    QApplication::setApplicationName("appleseed.studio");
    QApplication::setApplicationVersion(Appleseed::get_lib_version());
    QApplication::setWindowIcon(QIcon(make_app_path("icons/appleseed.png")));
    application.setAttribute(Qt::AA_DontUseNativeMenuBar, true);

    // The locale must be set after the construction of QApplication.
    QLocale::setDefault(QLocale::C);

    // QApplication sets C locale to the user's locale, we need to fix this.
    std::setlocale(LC_ALL, "C");

    // QT changes locale when loading image from disk for the very first time.
    // The problem was tracked for both QImage and QPixmap.
    // Both classes in their `load()` function call `QImageReader.read()`
    // which results in change of the locale back to system settings.
    // This is a dirty fix which loads any image at the very beginning and
    // resets the locale right after, thus preventing the `QImageReader.read()`
    // to change it again (as it happens only on the very first `read`).
    // Issue reported and tracked on GitHub under reference #1435.
    QImageReader(make_app_path("icons/icon.png")).read();   // any image

    // Make sure this build can run on this host.
    check_compatibility();

    // Make sure appleseed is correctly installed.
    check_installation();

    // Parse the command line.
    SuperLogger logger;
#ifdef _WIN32
    // On Windows, we will display command line arguments in a message box
    // so we need to capture CommandLineHandler's output into a string.
    logger.set_log_target(create_string_log_target());
#endif
    CommandLineHandler cl;
    cl.parse(argc, const_cast<const char**>(argv), logger);

    // Configure the application to use our default stylesheet file.
    set_default_stylesheet(application);

    // Create the application's main window.
    appleseed::studio::MainWindow window;

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

    window.show();

    return application.exec();
}
