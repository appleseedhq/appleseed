
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace renderer  { class Project; }

namespace renderer
{

//
// Project file writer.
//

class APPLESEED_DLLSYMBOL ProjectFileWriter
{
  public:
    enum Options
    {
        Defaults                    = 0,            // none of the flags below
        OmitHeaderComment           = 1UL << 0,     // do not write the header comment
        OmitWritingGeometryFiles    = 1UL << 1,     // do not write geometry files to disk
        OmitHandlingAssetFiles      = 1UL << 2,     // do not change paths to asset files (such as texture files)
        CopyAllAssets               = 1UL << 3      // copy all asset files (by default copy asset files with relative paths only)
    };

    // Write a project to disk.
    // Returns true on success, false otherwise.
    static bool write(
        Project&        project,
        const char*     filepath,
        const int       options = Defaults,
        const char*     extra_comments = nullptr);
};

}   // namespace renderer
