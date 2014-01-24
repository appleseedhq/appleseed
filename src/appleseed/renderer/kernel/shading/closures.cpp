
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
#include "closures.h"

// appleseed.renderer headers
#include "renderer/global/globaltypes.h"
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"

// OSL headers
#include <OSL/genclosure.h>
#include <OSL/oslclosure.h>

// standard headers
#include <algorithm>

using namespace foundation;
using namespace renderer;

namespace renderer
{
namespace
{

//
// Closure Parameters.
//

struct EmptyClosureParams {};

struct LambertClosureParams
{
    OSL::Vec3 N;
};

} // unnamed

}   // namespace renderer

// We probably want to reuse OSL macros to declare closure params 
// and register the closures. We can use them only inside the OSL namespace
OSL_NAMESPACE_ENTER

void register_appleseed_closures(OSL::ShadingSystem& shading_system)
{    
    // Describe the memory layout of each closure type to the OSL runtime
    enum {MaxParams = 32};
    struct BuiltinClosures
    {
        const char* name;
        int id;
        ClosureParam params[MaxParams];
    };

    BuiltinClosures builtins[] =
    {
        {"diffuse"                    , LambertID,          {CLOSURE_VECTOR_PARAM(LambertClosureParams, N),
                                                             CLOSURE_FINISH_PARAM(LambertClosureParams)}},

        {NULL, 0, {}} // mark end of the array
    };

    for (int i = 0; builtins[i].name != 0; ++i)
    {
        shading_system.register_closure(builtins[i].name,
                                        builtins[i].id,
                                        builtins[i].params,
                                        0,
                                        0);

        RENDERER_LOG_INFO("registered OSL closure %s", builtins[i].name);
    }
}

OSL_NAMESPACE_EXIT

namespace renderer
{

void register_closures(OSL::ShadingSystem& shading_system)
{
    OSL::register_appleseed_closures(shading_system);
}

}   // namespace renderer
