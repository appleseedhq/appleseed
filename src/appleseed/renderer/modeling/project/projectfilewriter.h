
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_RENDERER_MODELING_PROJECT_PROJECTFILEWRITER_H
#define APPLESEED_RENDERER_MODELING_PROJECT_PROJECTFILEWRITER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// Forward declarations.
namespace renderer      { class Project; }

namespace renderer
{

//
// Project file writer.
//

class RENDERERDLL ProjectFileWriter
{
  public:
    // Write a project to disk. The project must be named.
    // Return true on success, false otherwise.
    static bool write(
        const Project&  project,
        const bool      omit_header_comment = false);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_PROJECT_PROJECTFILEWRITER_H
