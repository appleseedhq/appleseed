
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Francois Beaune, The appleseedhq Organization
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
#include "shadingray.h"

// Standard headers.
#include <cstddef>

// appleseed.renderer headers.
#include "renderer/modeling/material/material.h"

using namespace foundation;

namespace renderer
{

void ShadingRay::copy_media_from(const ShadingRay& source)
{
    assert(m_medium_count == 0);

    const uint8 n = source.m_medium_count;

    m_medium_count = n;

    for (uint8 i = 0; i < n; ++i)
        m_media[i] = source.m_media[i];
}

void ShadingRay::add_medium(
    const ShadingRay&       source,
    const ObjectInstance*   object_instance,
    const Material*         material,
    const float             ior)
{
    assert(m_medium_count == 0);

    const int8 medium_priority = object_instance->get_medium_priority();
    const uint8 n = source.m_medium_count;
    uint8 i = 0, j = 0;

    while (i < n && source.m_media[i].m_object_instance->get_medium_priority() >= medium_priority)
        m_media[j++] = source.m_media[i++];

    if (j < MaxMediumCount)
    {
        m_media[j].m_object_instance = object_instance;
        m_media[j].m_material = material;
        m_media[j].m_ior = ior;
        ++j;
    }

    while (i < n && j < MaxMediumCount)
        m_media[j++] = source.m_media[i++];

    m_medium_count = j;
}

void ShadingRay::remove_medium(
    const ShadingRay&       source,
    const ObjectInstance*   object_instance)
{
    assert(m_medium_count == 0);

    uint8 j = 0;

    for (uint8 i = 0, e = source.m_medium_count; i < e; ++i)
    {
        if (source.m_media[i].m_object_instance != object_instance)
            m_media[j++] = source.m_media[i];
    }

    m_medium_count = j;
}

const Volume* ShadingRay::Medium::get_volume() const
{
    if (m_material == nullptr)
        return nullptr;

    return m_material->get_render_data().m_volume;
}

}   // namespace renderer
