
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.python headers.
#include "pyseed.h" // has to be first, to avoid redefinition warnings
#include "gillocks.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/log.h"

// Standard headers.
#include <cstddef>
#include <cstdio>
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

struct ILogTargetWrap
  : public ILogTarget
  , public bpy::wrapper<ILogTarget>
{
    ILogTargetWrap() {}
    ~ILogTargetWrap() {}

    virtual void release() APPLESEED_OVERRIDE
    {
        delete this;
    }

    virtual void write(
        const LogMessage::Category  category,
        const char*                 file,
        const size_t                line,
        const char*                 header,
        const char*                 message) APPLESEED_OVERRIDE
    {
        // Because this can be called from multiple threads
        // we need to lock Python here.
        ScopedGILLock lock;

        try
        {
            this->get_override("write")(category, file, line, header, message);
        }
        catch (bpy::error_already_set)
        {
            PyErr_Print();
        }
    }
};

// Work around a regression in Visual Studio 2015 Update 3.
#if defined(_MSC_VER) && _MSC_VER == 1900
namespace boost
{
    template <> ILogTargetWrap const volatile* get_pointer<ILogTargetWrap const volatile>(ILogTargetWrap const volatile* p) { return p; }
    template <> Logger const volatile* get_pointer<Logger const volatile>(Logger const volatile* p) { return p; }
}
#endif

namespace
{

    Logger* get_global_logger()
    {
        return &global_logger();
    }

    void logger_set_all_formats(Logger* logger, const std::string& format)
    {
        logger->set_all_formats(format);
    }

    void logger_set_format(Logger* logger, const LogMessage::Category category, const std::string& format)
    {
        logger->set_format(category, format);
    }

    void logger_add_target(Logger* logger, bpy::object target)
    {
        ILogTargetWrap* p = bpy::extract<ILogTargetWrap*>(target);
        assert(p);

        logger->add_target(p);
    }

    void logger_remove_target(Logger* logger, bpy::object target)
    {
        ILogTargetWrap* p = bpy::extract<ILogTargetWrap*>(target);
        assert(p);

        logger->remove_target(p);
    }
}

void bind_logger()
{
    bpy::class_<ILogTargetWrap, boost::shared_ptr<ILogTargetWrap>, boost::noncopyable>("ILogTarget")
        .def("write", bpy::pure_virtual(&ILogTarget::write))
        ;

    bpy::enum_<LogMessage::Category>("LogMessageCategory")
        .value("Info", LogMessage::Info)
        .value("Debug", LogMessage::Debug)
        .value("Warning", LogMessage::Warning)
        .value("Error", LogMessage::Error)
        .value("Fatal", LogMessage::Fatal)
        ;

    bpy::class_<Logger, boost::noncopyable>("Logger", bpy::no_init)
        .def("set_enabled", &Logger::set_enabled)
        .def("reset_all_formats", &Logger::reset_all_formats)
        .def("reset_format", &Logger::reset_format)
        .def("set_all_formats", logger_set_all_formats)
        .def("set_format", logger_set_format)
        .def("get_format", &Logger::get_format)
        .def("add_target", logger_add_target)
        .def("remove_target", logger_remove_target)
        ;

    bpy::def("global_logger", &get_global_logger, bpy::return_value_policy<bpy::reference_existing_object>());
}
