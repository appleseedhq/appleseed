
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

// appleseed.studio headers.
#include "mainwindow/mainwindow.h"

// appleseed.shared headers.
#include "application/application.h"

// appleseed.foundation headers.
#include "foundation/platform/path.h"

// Qt headers.
#include <QtGui/QApplication>
#include <QMessageBox>
#include <QString>
#include <QTextStream>

// boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cstdlib>
#include <fstream>
#include <sstream>
#include <string>

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
            "Specifically, it was expected that " + executable_path.filename() + " would "
            "reside in a bin/ subdirectory inside the main directory of the application, "
            "but it appears not to be the case (" + executable_path.filename() +
            " seems to be located in " + executable_path.parent_path().directory_string() + ").";

        // Display a message box.
        QMessageBox msgbox;
        msgbox.setWindowTitle("Application incorrectly installed");
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

    void display_stylesheet_load_error(const filesystem::path& stylesheet_path)
    {
        QMessageBox msgbox;
        msgbox.setWindowTitle("Failed to load default stylesheet");
        msgbox.setIcon(QMessageBox::Warning);
        msgbox.setText(
            QString::fromStdString(
                "The stylesheet " + stylesheet_path.file_string() + " could not be loaded; "
                "the application will use the default style."));
        msgbox.setStandardButtons(QMessageBox::Ok);
        msgbox.setDefaultButton(QMessageBox::Ok);
        msgbox.exec();
    }

    void set_default_stylesheet(QApplication& application)
    {
        if (Application::is_correctly_installed())
        {
            // Build the path to the default stylesheet file.
            const filesystem::path stylesheet_path =
                  filesystem::path(Application::get_root_path())
                / "stylesheets/default.qss";

            // Load and apply the stylesheet.
            string stylesheet;
            if (load_file(stylesheet_path.file_string(), stylesheet))
                application.setStyleSheet(QString::fromStdString(stylesheet));
            else display_stylesheet_load_error(stylesheet_path);
        }
    }

    void configure_application(QApplication& application)
    {
        application.setAttribute(Qt::AA_DontUseNativeMenuBar, true);

        set_default_stylesheet(application);
    }
}


//
// Application's entry point.
//

int main(int argc, char *argv[])
{
    QApplication application(argc, argv);

    check_installation();

    configure_application(application);

    appleseed::studio::MainWindow window;
    window.show();

    return application.exec();
}
