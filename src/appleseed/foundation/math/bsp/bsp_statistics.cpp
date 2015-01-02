
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "bsp_statistics.h"

// Standard headers.
#include <string>

namespace foundation {
namespace bsp {

//
// TraversalStatistics class implementation.
//

TraversalStatistics::TraversalStatistics()
  : m_traversal_count(0)
{
}

void TraversalStatistics::print(Logger& logger)
{
    LOG_DEBUG(
        logger,
        "  traversals       %s\n"
        "  visited interior avg %.1f  min %s  max %s  dev %.1f\n"
        "  visited leaves   avg %.1f  min %s  max %s  dev %.1f\n"
        "  tested items     avg %.1f  min %s  max %s  dev %.1f",
        pretty_uint(m_traversal_count).c_str(),
        m_visited_interior.get_mean(),
        pretty_uint(m_visited_interior.get_min()).c_str(),
        pretty_uint(m_visited_interior.get_max()).c_str(),
        m_visited_interior.get_dev(),
        m_visited_leaves.get_mean(),
        pretty_uint(m_visited_leaves.get_min()).c_str(),
        pretty_uint(m_visited_leaves.get_max()).c_str(),
        m_visited_leaves.get_dev(),
        m_intersected_items.get_mean(),
        pretty_uint(m_intersected_items.get_min()).c_str(),
        pretty_uint(m_intersected_items.get_max()).c_str(),
        m_intersected_items.get_dev());
}

}   // namespace bsp
}   // namespace foundation
