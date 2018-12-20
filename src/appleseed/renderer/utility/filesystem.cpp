
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Kevin Masson, The appleseedhq Organization
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
#include "filesystem.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

namespace bf = boost::filesystem;
namespace bsys = boost::system;

namespace renderer
{

void create_parent_directories(const bf::path& file_path)
{
    const bf::path parent_path = file_path.parent_path();

    if (!parent_path.empty() && !bf::exists(parent_path))
    {
        bsys::error_code ec;
        if (!bf::create_directories(parent_path, ec))
        {
            RENDERER_LOG_ERROR(
                "could not create directory %s: %s",
                parent_path.c_str(),
                ec.message().c_str());
        }
    }
}

void create_parent_directories(const char* file_path)
{
    create_parent_directories(bf::path(file_path));
}

}       // namespace renderer
