
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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

const char* VisibilityFlags::Names[] =
{
    "camera",
    "light",
    "shadow",
    "transparency",
    "probe",
    "diffuse",
    "glossy",
    "specular"
};

const size_t VisibilityFlags::Count = countof(VisibilityFlags::Names);

VisibilityFlags::Type VisibilityFlags::parse(
    const ParamArray&       params,
    const MessageContext&   message_context)
{
    Type flags = 0;

    if (params.get_optional<bool>("camera", true, message_context))
        flags |= CameraRay;

    if (params.get_optional<bool>("light", true, message_context))
        flags |= LightRay;

    if (params.get_optional<bool>("shadow", true, message_context))
        flags |= ShadowRay;

    if (params.get_optional<bool>("transparency", true, message_context))
        flags |= TransparencyRay;

    if (params.get_optional<bool>("probe", true, message_context))
        flags |= ProbeRay;

    if (params.get_optional<bool>("diffuse", true, message_context))
        flags |= DiffuseRay;

    if (params.get_optional<bool>("glossy", true, message_context))
        flags |= GlossyRay;

    if (params.get_optional<bool>("specular", true, message_context))
        flags |= SpecularRay;

    return flags;
}

}   // namespace renderer
