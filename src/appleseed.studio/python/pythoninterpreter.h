
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Gleb Mishchenko, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_PYTHON_PYTHONINTERPRETER_H
#define APPLESEED_STUDIO_PYTHON_PYTHONINTERPRETER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/python.h"

// Standard headers.
#include <string>

namespace appleseed {
namespace studio {

// Forward declarations.
class MainWindow;
class OutputRedirector;

class PythonInterpreter
  : public foundation::NonCopyable
{
  public:
    static PythonInterpreter& instance();

    void set_main_window(MainWindow* main_window);
    MainWindow* get_main_window() const;

    void initialize(OutputRedirector redirector);

    void load_plugins();

    boost::python::object execute(const std::string& command);

  private:
    PythonInterpreter();
    ~PythonInterpreter();

    void import_python_module(const char* module_name, const char* alias_name);

    MainWindow*             m_main_window;
    boost::python::object   m_main_namespace;
    bool                    m_is_initialized;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_PYTHON_PYTHONINTERPRETER_H
