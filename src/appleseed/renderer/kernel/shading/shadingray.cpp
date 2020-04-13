
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/modeling/material/material.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

namespace renderer
{

void ShadingRay::scale_differentials(const ValueType s)
{
    m_rx_org = m_org + (m_rx_org - m_org) * s;
    m_ry_org = m_org + (m_ry_org - m_org) * s;
    m_rx_dir = m_dir + (m_rx_dir - m_dir) * s;
    m_ry_dir = m_dir + (m_ry_dir - m_dir) * s;
}

void ShadingRay::copy_media_from(const ShadingRay& source)
{
    assert(m_medium_count == 0);

    const std::uint8_t n = source.m_medium_count;

    m_medium_count = n;

    for (std::uint8_t i = 0; i < n; ++i)
        m_media[i] = source.m_media[i];
}

void ShadingRay::add_medium(
    const ShadingRay&       source,
    const ObjectInstance*   object_instance,
    const Material*         material,
    const float             ior)
{
    assert(m_medium_count == 0);

    const std::int8_t medium_priority = object_instance->get_medium_priority();
    const std::uint8_t n = source.m_medium_count;
    std::uint8_t i = 0, j = 0;

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

    std::uint8_t j = 0;

    for (std::uint8_t i = 0, e = source.m_medium_count; i < e; ++i)
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
