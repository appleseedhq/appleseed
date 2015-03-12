
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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
#include "oiiocomponents.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/project/project.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/string.h"

// Standard headers.
#include <cstddef>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// OIIOComponents class implementation.
//

OIIOComponents::OIIOComponents(
    const Project&      project,
    const ParamArray&   params)
{
    m_texture_system = OIIO::TextureSystem::create(false);

    const size_t texture_cache_size_bytes =
        params.get_optional<size_t>("max_size", 256 * 1024 * 1024);

    RENDERER_LOG_INFO(
        "setting OpenImageIO texture cache size to %s.",
        pretty_size(texture_cache_size_bytes).c_str());

    const float texture_cache_size_mb =
        static_cast<float>(texture_cache_size_bytes) / (1024 * 1024);

    m_texture_system->attribute("max_memory_MB", texture_cache_size_mb);

    const string search_paths = project.make_search_path_string();

    if (!search_paths.empty())
        m_texture_system->attribute("searchpath", search_paths);

    m_texture_system->attribute("automip", 0);
    m_texture_system->attribute("accept_untiled", 1);
    m_texture_system->attribute("accept_unmipped", 1);
    m_texture_system->attribute("gray_to_rgb", 1);
    m_texture_system->attribute("latlong_up", "y");
}

OIIOComponents::~OIIOComponents()
{
    const string stats = m_texture_system->getstats();
    const string trimmed_stats = trim_right(stats, "\r\n");

    RENDERER_LOG_DEBUG("%s", trimmed_stats.c_str());

    OIIO::TextureSystem::destroy(m_texture_system);
}

OIIO::TextureSystem& OIIOComponents::get_texture_system()
{
    return *m_texture_system;
}

}   // namespace renderer
