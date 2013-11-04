
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include <QPalette>

// boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cstdlib>
#include <fstream>
#include <locale>
#include <sstream>
#include <string>

using namespace appleseed::studio;
using namespace appleseed::shared;
using namespace boost;
using namespace foundation;
using namespace std;

namespace
{
    void display_incorrect_installation_error()
    {
        // We need the path to the application's executable to construct the error message.
        const filesystem::path executable_path(Path::get_executable_path());

        // Construct the error message.
        const string informative_text =
            "Specifically, it was expected that " + executable_path.filename().string() + " would "
            "reside in a " + filesystem::path("bin/").make_preferred().string() + " subdirectory "
            "inside the main directory of the application, but it appears not to be the case "
            "(" + executable_path.filename().string() +
            " seems to be located in " + executable_path.parent_path().string() + ").";

        // Display a message box.
        QMessageBox msgbox;
        msgbox.setWindowTitle("Application Incorrectly Installed");
        msgbox.setIcon(QMessageBox::Critical);
        msgbox.setText(
            "The application failed to start because it is not properly installed. "
            "Please reinstall the application.");
        msgbox.setInformativeText(QString::fromStdString(informative_text));
        msgbox.setStandardButtons(QMessageBox::Ok);
        msgbox.setDefaultButton(QMessageBox::Ok);
        msgbox.exec();
    }

    void check_installation()
    {
        if (!Application::is_correctly_installed())
        {
            display_incorrect_installation_error();
            exit(EXIT_FAILURE);
        }
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

    void display_stylesheet_load_error(const string& stylesheet_path)
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
    }

    void display_stylesheet_process_error(
        const string&   stylesheet_path,
        const string&   error_message,
        const size_t    error_location)
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
                .arg(error_location)
                .arg(QString::fromStdString(error_message)));
        msgbox.setStandardButtons(QMessageBox::Ok);
        msgbox.setDefaultButton(QMessageBox::Ok);
        msgbox.exec();
    }

    bool load_stylesheet(const string& stylesheet_path, string& stylesheet)
    {
        if (!load_file(stylesheet_path, stylesheet))
        {
            display_stylesheet_load_error(stylesheet_path);
            return false;
        }

        Preprocessor preprocessor;

#if defined __APPLE__
        preprocessor.define_symbol("__APPLE__");
#elif defined _WIN32
        preprocessor.define_symbol("_WIN32");
#endif

        preprocessor.process(stylesheet.c_str());

        if (preprocessor.failed())
        {
            display_stylesheet_process_error(
                stylesheet_path,
                preprocessor.get_error_message(),
                preprocessor.get_error_location());
            return false;
        }

        stylesheet = preprocessor.get_processed_text();

        return true;
    }

    void set_style(QApplication& application)
    {
        if (Application::is_correctly_installed())
        {

            application.setStyle("plastique");

            QPalette palette;
            QColor backGround(50, 50, 50);
            QColor light = QColor(70,70,70);
            QColor base = QColor(40,40,40);
            QColor dark = QColor(20,20,20);
            QColor text = QColor(220, 220, 220);

            palette.setBrush(QPalette::Active,   QPalette::Highlight, backGround);
            palette.setBrush(QPalette::Inactive, QPalette::Highlight, backGround);
            palette.setBrush(QPalette::Disabled, QPalette::Highlight, backGround);

            // Find the correct disabled text color
            palette.setBrush(QPalette::Disabled, QPalette::Text, text);

            palette.setBrush(QPalette::Window, backGround);
            // NOTE: on some version of Qt the WinowText attribute may not be honored
            // an equivalent qss rule is necissary as a workaround
            palette.setBrush(QPalette::WindowText, text);
            palette.setBrush(QPalette::ButtonText, text);
            palette.setBrush(QPalette::BrightText, text);
            palette.setBrush(QPalette::Text, text);
            palette.setBrush(QPalette::HighlightedText, text);

            palette.setBrush(QPalette::Mid, base);
            palette.setBrush(QPalette::Light, light);

            palette.setBrush(QPalette::Active, QPalette::Base, base);
            palette.setBrush(QPalette::Inactive, QPalette::Base, base);
            palette.setBrush(QPalette::Disabled, QPalette::Base, base);

            palette.setBrush(QPalette::Midlight, base);

            palette.setBrush(QPalette::All, QPalette::Dark, dark);
            palette.setBrush(QPalette::Disabled, QPalette::Dark, base);

            QColor button = backGround;

            palette.setBrush(QPalette::Button, button);

            QColor shadow = dark.darker(135);
            palette.setBrush(QPalette::Shadow, shadow);
            palette.setBrush(QPalette::Disabled, QPalette::Shadow, shadow.lighter(150));
            palette.setBrush(QPalette::HighlightedText, QColor(QRgb(0xffffffff)));

            application.setPalette(palette);

            // Build the path to the default stylesheet file.
            const filesystem::path stylesheet_path =
                  filesystem::path(Application::get_root_path())
                / "stylesheets"
                / "default.qss";

            // Load and apply the stylesheet.
            string stylesheet;
            if (load_stylesheet(stylesheet_path.string(), stylesheet))
                application.setStyleSheet(QString::fromStdString(stylesheet));
        }
    }

    void configure_application(QApplication& application)
    {
        application.setAttribute(Qt::AA_DontUseNativeMenuBar, true);
        set_style(application);
    }
}


//
// Entry point of appleseed.studio.
//

int main(int argc, char *argv[])
{
    start_memory_tracking();

    QApplication application(argc, argv);
    QApplication::setOrganizationName("appleseedhq");
    QApplication::setOrganizationDomain("appleseedhq.net");
    QApplication::setApplicationName("appleseed.studio");
    QApplication::setApplicationVersion(Appleseed::get_lib_version());

    // The locale must be set after the construction of QApplication.
    QLocale::setDefault(QLocale::C);
    setlocale(LC_ALL, "C");

    check_installation();

    SuperLogger logger;
#ifdef _WIN32
    logger.set_log_target(create_string_log_target());
#endif

    CommandLineHandler cl;
    cl.parse(argc, const_cast<const char**>(argv), logger);

    configure_application(application);

    appleseed::studio::MainWindow window;

    if (!cl.m_filename.values().empty())
    {
        const QString filename = QString::fromStdString(cl.m_filename.values().front());

        if (cl.m_render.is_set())
        {
            const QString configuration = QString::fromStdString(cl.m_render.values()[0]);
            window.open_and_render_project(filename, configuration);
        }
        else
        {
            window.open_project(filename);
        }
    }

    window.show();

    return application.exec();
}
