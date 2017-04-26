
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

// Interface header.
#include "visibilityflags.h"

// appleseed.renderer headers.
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/countof.h"

namespace renderer
{

// The Names array must be kept in sync with the VisibilityFlags::Values enum and
// the order of the strings has to match the order of the enum.
const char* VisibilityFlags::Names[] =
{
    "camera",
    "light",
    "shadow",
    "transparency",
    "probe",
    "diffuse",
    "glossy",
    "specular",
    "subsurface"
};

const size_t VisibilityFlags::Count = countof(VisibilityFlags::Names);

VisibilityFlags::Type VisibilityFlags::parse(
    const ParamArray&       params,
    const MessageContext&   message_context)
{
    Type flags = 0;

    for (size_t i = 0; i < Count; ++i)
    {
        if (params.get_optional<bool>(Names[i], true, message_context))
            flags |= 1 << i;
    }

    return flags;
}

ParamArray VisibilityFlags::to_dictionary(const Type flags)
{
    ParamArray params;

    for (size_t i = 0; i < Count; ++i)
        params.insert(Names[i], (flags & (1 << i)) != 0);

    return params;
}

}   // namespace renderer
