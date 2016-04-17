
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_AOV_SHADINGFRAGMENTSTACK_H
#define APPLESEED_RENDERER_KERNEL_AOV_SHADINGFRAGMENTSTACK_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovsettings.h"
#include "renderer/kernel/aov/spectrumstack.h"
#include "renderer/kernel/shading/shadingfragment.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace renderer
{

//
// A small array of shading fragments.
//

class ShadingFragmentStack
  : public foundation::NonCopyable
{
  public:
    explicit ShadingFragmentStack(const size_t size);

    size_t size() const;

    ShadingFragment& operator[](const size_t index);
    const ShadingFragment& operator[](const size_t index) const;

    ShadingFragmentStack& operator+=(const ShadingFragmentStack& rhs);
    ShadingFragmentStack& operator*=(const float rhs);

    void set(const size_t index, const ShadingFragment& rhs);
    void add(const size_t index, const ShadingFragment& rhs);

    struct ColorProxy
    {
        ShadingFragmentStack& m_parent;

        explicit ColorProxy(ShadingFragmentStack& parent);

        void set(const float val);

        ColorProxy& operator=(const SpectrumStack& rhs);
        ColorProxy& operator*=(const float rhs);
    };

    struct AlphaProxy
    {
        ShadingFragmentStack& m_parent;

        explicit AlphaProxy(ShadingFragmentStack& parent);

        void set(const float val);

        AlphaProxy& operator*=(const float rhs);
    };

    ColorProxy      m_color;
    AlphaProxy      m_alpha;

  private:
    ShadingFragment m_fragments[MaxAOVCount];
    size_t          m_size;
};


//
// ShadingFragmentStack class implementation.
//

inline ShadingFragmentStack::ShadingFragmentStack(const size_t size)
  : m_color(*this)
  , m_alpha(*this)
  , m_size(size)
{
    assert(size <= MaxAOVCount);
}

inline size_t ShadingFragmentStack::size() const
{
    return m_size;
}

inline ShadingFragment& ShadingFragmentStack::operator[](const size_t index)
{
    assert(index < m_size);
    return m_fragments[index];
}

inline const ShadingFragment& ShadingFragmentStack::operator[](const size_t index) const
{
    assert(index < m_size);
    return m_fragments[index];
}

inline ShadingFragmentStack& ShadingFragmentStack::operator+=(const ShadingFragmentStack& rhs)
{
    assert(m_size == rhs.m_size);

    for (size_t i = 0; i < m_size; ++i)
        m_fragments[i] += rhs.m_fragments[i];

    return *this;
}

inline ShadingFragmentStack& ShadingFragmentStack::operator*=(const float rhs)
{
    for (size_t i = 0; i < m_size; ++i)
        m_fragments[i] *= rhs;

    return *this;
}

inline void ShadingFragmentStack::set(const size_t index, const ShadingFragment& rhs)
{
    if (index < m_size)
        m_fragments[index] = rhs;
}

inline void ShadingFragmentStack::add(const size_t index, const ShadingFragment& rhs)
{
    if (index < m_size)
        m_fragments[index] += rhs;
}

inline ShadingFragmentStack::ColorProxy::ColorProxy(ShadingFragmentStack& parent)
  : m_parent(parent)
{
}

inline void ShadingFragmentStack::ColorProxy::set(const float val)
{
    for (size_t i = 0; i < m_parent.m_size; ++i)
        m_parent.m_fragments[i].m_color.set(val);
}

inline ShadingFragmentStack::ColorProxy& ShadingFragmentStack::ColorProxy::operator=(const SpectrumStack& rhs)
{
    assert(m_parent.m_size == rhs.size());

    for (size_t i = 0; i < m_parent.m_size; ++i)
        m_parent.m_fragments[i].m_color = rhs[i];

    return *this;
}

inline ShadingFragmentStack::ColorProxy& ShadingFragmentStack::ColorProxy::operator*=(const float rhs)
{
    for (size_t i = 0; i < m_parent.m_size; ++i)
        m_parent.m_fragments[i].m_color *= rhs;

    return *this;
}

inline ShadingFragmentStack::AlphaProxy::AlphaProxy(ShadingFragmentStack& parent)
  : m_parent(parent)
{
}

inline void ShadingFragmentStack::AlphaProxy::set(const float val)
{
    for (size_t i = 0; i < m_parent.m_size; ++i)
        m_parent.m_fragments[i].m_alpha.set(val);
}

inline ShadingFragmentStack::AlphaProxy& ShadingFragmentStack::AlphaProxy::operator*=(const float rhs)
{
    for (size_t i = 0; i < m_parent.m_size; ++i)
        m_parent.m_fragments[i].m_alpha *= rhs;

    return *this;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_AOV_SHADINGFRAGMENTSTACK_H
