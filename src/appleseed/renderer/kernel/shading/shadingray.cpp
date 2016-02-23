
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Francois Beaune, The appleseedhq Organization
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

using namespace foundation;

namespace renderer
{

void ShadingRay::copy_volumes_from(const ShadingRay& source)
{
    assert(m_volume_count == 0);

    const uint8 n = source.m_volume_count;

    m_volume_count = n;

    for (uint8 i = 0; i < n; ++i)
        m_volumes[i] = source.m_volumes[i];
}

void ShadingRay::add_volume(
    const ShadingRay&       source,
    const ObjectInstance*   object_instance,
    const BSDF*             bsdf,
    const double            ior)
{
    assert(m_volume_count == 0);

    const uint8 volume_priority = object_instance->get_volume_priority();
    const uint8 n = source.m_volume_count;
    uint8 i = 0, j = 0;

    while (i < n && source.m_volumes[i].m_object_instance->get_volume_priority() >= volume_priority)
        m_volumes[j++] = source.m_volumes[i++];

    if (j < MaxVolumeCount)
    {
        m_volumes[j].m_object_instance = object_instance;
        m_volumes[j].m_bsdf = bsdf;
        m_volumes[j].m_ior = ior;
        ++j;
    }

    while (i < n && j < MaxVolumeCount)
        m_volumes[j++] = source.m_volumes[i++];

    m_volume_count = j;
}

void ShadingRay::remove_volume(
    const ShadingRay&       source,
    const ObjectInstance*   object_instance)
{
    assert(m_volume_count == 0);

    const uint8 n = source.m_volume_count;
    uint8 j = 0;

    for (uint8 i = 0; i < n; ++i)
    {
        if (source.m_volumes[i].m_object_instance != object_instance)
            m_volumes[j++] = source.m_volumes[i];
    }

    m_volume_count = j;
}

}   // namespace renderer
