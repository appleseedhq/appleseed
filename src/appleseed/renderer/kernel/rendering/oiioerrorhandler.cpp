
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "oiioerrorhandler.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/utility/string.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// OIIOErrorHandler class implementation.
//

void OIIOErrorHandler::operator()(int errcode, const string& msg)
{
    const string modified_msg = prefix_all_lines(trim_both(msg), "osl: ");

    switch (errcode)
    {
      case EH_WARNING:
        RENDERER_LOG_WARNING("%s", modified_msg.c_str());
        break;

      case EH_ERROR:
        RENDERER_LOG_ERROR("%s", modified_msg.c_str());
        break;

      case EH_SEVERE:
        RENDERER_LOG_FATAL("%s", modified_msg.c_str());
        break;

      case EH_DEBUG:
        RENDERER_LOG_DEBUG("%s", modified_msg.c_str());
        break;

      default:
        RENDERER_LOG_DEBUG("%s", modified_msg.c_str());
        break;
    }
}

}   // namespace renderer
