
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_BSDFSAMPLE_H
#define APPLESEED_RENDERER_MODELING_BSDF_BSDFSAMPLE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.

namespace renderer
{

class APPLESEED_DLLSYMBOL BSDFSample
{
  public:
    // Scattering modes.
    enum ScatteringMode
    {
        Absorption          = 0,
        Diffuse             = 1 << 0,
        Glossy              = 1 << 1,
        Specular            = 1 << 2,
        AllScatteringModes  = Diffuse | Glossy | Specular
    };

    /*
    // Test for the presence of specific scattering modes.
    static bool has_diffuse(const ScatteringMode mode);
    static bool has_glossy(const ScatteringMode mode);
    static bool has_specular(const ScatteringMode mode);
    static bool has_diffuse_or_glossy(const ScatteringMode mode);
    static bool has_glossy_or_specular(const ScatteringMode mode);
    */
  private:
};


//
// BSDFSample class implementation.
//
/*
inline bool BSDFSample::has_diffuse(const ScatteringMode mode)
{
    return (mode & Diffuse) != 0;
}

inline bool BSDFSample::has_glossy(const ScatteringMode mode)
{
    return (mode & Glossy) != 0;
}

inline bool BSDFSample::has_specular(const ScatteringMode mode)
{
    return (mode & Specular) != 0;
}

inline bool BSDFSample::has_diffuse_or_glossy(const ScatteringMode mode)
{
    return (mode & (Diffuse | Glossy)) != 0;
}

inline bool BSDFSample::has_glossy_or_specular(const ScatteringMode mode)
{
    return (mode & (Glossy | Specular)) != 0;
}
*/

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_BSDFSAMPLE_H
