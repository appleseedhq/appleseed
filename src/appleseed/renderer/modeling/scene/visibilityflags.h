
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
        Invisible       = 0,
        CameraRay       = 1UL << 0,
        LightRay        = 1UL << 1,
        ShadowRay       = 1UL << 2,
        TransparencyRay = 1UL << 3,
        ProbeRay        = 1UL << 4,
        DiffuseRay      = 1UL << 5,
        GlossyRay       = 1UL << 6,
        SpecularRay     = 1UL << 7,
        SubsurfaceRay   = 1UL << 8,
        NPRRay          = 1UL << 9,
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

}   // namespace renderer
