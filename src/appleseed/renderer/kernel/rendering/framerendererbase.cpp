
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

// Interface header.
#include "framerendererbase.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/platform/system.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// FrameRendererBase class implementation.
//

size_t FrameRendererBase::get_rendering_thread_count(const ParamArray& params)
{
    const size_t core_count = System::get_logical_cpu_core_count();

    static const char* ThreadCountParameterName = "rendering_threads";

    if (!params.strings().exist(ThreadCountParameterName))
        return core_count;

    const string thread_count_str = params.strings().get<string>(ThreadCountParameterName);

    if (thread_count_str == "auto")
        return core_count;

    bool conversion_failed = false;
    size_t thread_count;

    try
    {
        thread_count = from_string<size_t>(thread_count_str);
    }
    catch (const ExceptionStringConversionError&)
    {
        conversion_failed = true;
    }

    if (conversion_failed || thread_count == 0)
    {
        RENDERER_LOG_ERROR(
            "invalid value \"%s\" for parameter \"%s\", using default value \"%s\".",
            thread_count_str.c_str(),
            ThreadCountParameterName,
            "auto");

        return core_count;
    }

    return thread_count;
}

void FrameRendererBase::print_rendering_thread_count(const size_t thread_count)
{
    RENDERER_LOG_INFO(
        "using %s %s for rendering.",
        pretty_int(thread_count).c_str(),
        plural(thread_count, "thread").c_str());
}

}   // namespace renderer
