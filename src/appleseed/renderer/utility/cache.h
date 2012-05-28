
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_UTILITY_CACHE_H
#define APPLESEED_RENDERER_UTILITY_CACHE_H

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/utility/cache.h"

// Standard headers.
#include <string>

namespace renderer
{

template <typename DualStageCacheType>
void print_dual_stage_cache_stats(
    const DualStageCacheType&   cache,
    const char*                 title)
{
    const std::string combined_stats =
        foundation::format_cache_stats(
            cache.get_stage0_hit_count() + cache.get_stage1_hit_count(),
            cache.get_stage1_miss_count());

    const std::string s0_stats =
        foundation::format_cache_stats(
            cache.get_stage0_hit_count(),
            cache.get_stage0_miss_count());

    const std::string s1_stats =
        foundation::format_cache_stats(
            cache.get_stage1_hit_count(),
            cache.get_stage1_miss_count());

    RENDERER_LOG_DEBUG(
        "%s:\n"
        "  combined         %s\n"
        "  stage-0          %s\n"
        "  stage-1          %s\n",
        title,
        combined_stats.c_str(),
        s0_stats.c_str(),
        s1_stats.c_str());
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_UTILITY_CACHE_H
