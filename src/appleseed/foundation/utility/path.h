
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_UTILITY_PATH_H
#define APPLESEED_FOUNDATION_UTILITY_PATH_H

// appleseed.foundation headers.
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cstddef>
#include <string>

namespace foundation
{

// Find the next available file path by searching for the first path that does not
// refer to an existing file on disk, and where the path follows the given pattern
// where consecutive '#' characters have been replaced by increasing integer values
// starting with 1.
boost::filesystem::path find_next_available_path(const boost::filesystem::path& path);


//
// Implementation.
//

inline boost::filesystem::path find_next_available_path(const boost::filesystem::path& path)
{
    const std::string pattern = path.string();
    const size_t max_value = get_numbered_string_max_value(pattern);

    for (size_t value = 1; value <= max_value; ++value)
    {
        const boost::filesystem::path candidate(get_numbered_string(pattern, value));

        if (!boost::filesystem::exists(candidate))
            return candidate;
    }

    return boost::filesystem::path(get_numbered_string(pattern, 1));
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_PATH_H
