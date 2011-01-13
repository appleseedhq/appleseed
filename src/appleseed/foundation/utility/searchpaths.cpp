
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "searchpaths.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <vector>

using namespace boost;
using namespace std;

namespace foundation
{

//
// SearchPaths class implementation.
//

struct SearchPaths::Impl
{
    typedef vector<string> PathCollection;

    PathCollection m_paths;
};

SearchPaths::SearchPaths()
  : impl(new Impl())
{
}

void SearchPaths::push_back(const char* path)
{
    assert(path);

    impl->m_paths.push_back(path);
}

bool SearchPaths::exist(const char* filepath) const
{
    assert(filepath);

    const filesystem::path fp(filepath);

    if (fp.is_complete())
        return filesystem::exists(fp);

    for (const_each<Impl::PathCollection> i = impl->m_paths; i; ++i)
    {
        const filesystem::path qualified_fp = filesystem::path(*i) / fp;

        if (filesystem::exists(qualified_fp))
            return true;
    }

    return false;
}

char* SearchPaths::qualify(const char* filepath) const
{
    assert(filepath);

    const filesystem::path fp(filepath);

    string result = fp.file_string();

    if (!fp.is_complete())
    {
        for (const_each<Impl::PathCollection> i = impl->m_paths; i; ++i)
        {
            const filesystem::path qualified_fp = filesystem::path(*i) / fp;

            if (filesystem::exists(qualified_fp))
            {
                result = qualified_fp.file_string();
                break;
            }
        }
    }

    return duplicate_string(result.c_str());
}

}   // namespace foundation
