
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Francois Beaune, The appleseedhq Organization
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

// Standard headers.
#include <memory>

// Forward declarations.
class QString;

namespace appleseed {
namespace bench {

class SystemInfo
{
  public:
    // Take a raw string returned by get_cpu_id() and clean it up for display.
    static QString cleanup_cpu_model_string(const QString& s);

    // Constructor.
    SystemInfo();

    // Destructor.
    ~SystemInfo();

    // Return a string identifying the CPU model.
    QString get_cpu_id() const;

    // Return the number of physical CPU cores.
    int get_cpu_core_count() const;

    // Return the number of enabled physical CPU cores.
    int get_enabled_cpu_core_count() const;

    // Return the number of CPU threads.
    int get_cpu_thread_count() const;

    // Return a string identifying the operating system.
    QString get_os_id() const;

  private:
    struct Impl;
    std::unique_ptr<Impl> impl;
};

}   // namespace bench
}   // namespace appleseed
