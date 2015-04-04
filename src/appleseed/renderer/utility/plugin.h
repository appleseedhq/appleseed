
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_UTILITY_PLUGIN_H
#define APPLESEED_RENDERER_UTILITY_PLUGIN_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/core/exceptions/exception.h"
#include "foundation/utility/autoreleaseptr.h"

namespace renderer
{

//
// Exception thrown when plugin initialization function fails.
//

class ExceptionPluginInitializationFailed
  : public foundation::Exception
{
  public:
    // Constructor.
    ExceptionPluginInitializationFailed();
};


//
// A renderer plugin.
//

class Plugin
  : public foundation::NonCopyable
{
  public:
    typedef bool (*InitPluginFnType)();
    typedef void (*UnInitPluginFnType)();

    // Delete this instance.
    void release();

    // Get a symbol from the plugin.
    void* get_symbol(const char* name, const bool no_throw = true) const;

    // Return the OS default file extension for plugins.
    static const char* get_default_file_extension();

  private:
    friend class PluginCache;

    struct Impl;
    Impl* impl;

    // Constructor.
    explicit Plugin(Impl* impl);

    // Destructor.
    ~Plugin();
};


//
// An application-wide plugin cache.
//

class PluginCache
{
  public:
    // Retrieve a plugin by its path. Thread-safe.
    static foundation::auto_release_ptr<Plugin> load(const char* path);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_UTILITY_PLUGIN_H
