
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

// Interface header.
#include "searchpaths.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

using namespace boost;
using namespace std;

namespace foundation
{

//
// SearchPaths class implementation.
//

// Insert a search path at the end of the collection.
void SearchPaths::push_back(const string& path)
{
    m_paths.push_back(path);
}

// Find a file in the search paths.
string SearchPaths::qualify(const string& file_path) const
{
    const filesystem::path fp(file_path);

    // Don't try to quality the file path if it's already a complete path.
    if (!fp.is_complete())
    {
        // Iterate over all search paths, in order.
        for (const_each<PathCollection> i = m_paths; i; ++i)
        {
            // Concatenate the file path and this search path.
            const filesystem::path path = filesystem::path(*i) / fp;

            // Check if the resulting path points to an existing file.
            if (filesystem::exists(path))
                return path.file_string();
        }
    }

    // Either the file path is already complete, or the file
    // couldn't be found in the search paths.
    return fp.file_string();
}

}   // namespace foundation
