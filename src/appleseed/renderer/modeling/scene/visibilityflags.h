
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

#ifndef APPLESEED_RENDERER_MODELING_SCENE_VISIBILITYFLAGS_H
#define APPLESEED_RENDERER_MODELING_SCENE_VISIBILITYFLAGS_H

// appleseed.foundation headers.
#include "foundation/platform/types.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class MessageContext; }
namespace renderer  { class ParamArray; }

namespace renderer
{

class APPLESEED_DLLSYMBOL VisibilityFlags
{
  public:
    typedef foundation::uint32 Type;

    enum Values
    {
        CameraRay       = 1 << 0,
        LightRay        = 1 << 1,
        ShadowRay       = 1 << 2,
        TransparencyRay = 1 << 3,
        ProbeRay        = 1 << 4,
        DiffuseRay      = 1 << 5,
        GlossyRay       = 1 << 6,
        SpecularRay     = 1 << 7,
        SubsurfaceRay   = 1 << 8,
        AllRays         = ~0
    };

    // Names follow the same order as values.
    static const size_t Count;
    static const char* Names[];

    // Parse visibility flags defined in a dictionary and combine them into a bitmask.
    static Type parse(
        const ParamArray&       params,
        const MessageContext&   message_context);

    // Convert visibility flags to a dictionary.
    static ParamArray to_dictionary(const Type flags);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SCENE_VISIBILITYFLAGS_H
