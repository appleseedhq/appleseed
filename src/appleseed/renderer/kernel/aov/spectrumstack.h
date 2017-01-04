
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

#ifndef APPLESEED_RENDERER_KERNEL_AOV_SPECTRUMSTACK_H
#define APPLESEED_RENDERER_KERNEL_AOV_SPECTRUMSTACK_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovsettings.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace renderer
{

//
// A small array of spectra.
//

class SpectrumStack
  : public foundation::NonCopyable
{
  public:
    explicit SpectrumStack(const size_t size);
    SpectrumStack(const size_t size, const float val);

    size_t size() const;

    void set(const float val);

    Spectrum& operator[](const size_t index);
    const Spectrum& operator[](const size_t index) const;

    SpectrumStack& operator+=(const SpectrumStack& rhs);
    SpectrumStack& operator*=(const Spectrum& rhs);
    SpectrumStack& operator*=(const float rhs);

    void add(const size_t index, const Spectrum& rhs);

  private:
    const size_t    m_size;
    Spectrum        m_spectra[MaxAOVCount];
};


//
// SpectrumStack class implementation.
//

inline SpectrumStack::SpectrumStack(const size_t size)
  : m_size(size)
{
    assert(size <= MaxAOVCount);
}

inline SpectrumStack::SpectrumStack(const size_t size, const float val)
  : m_size(size)
{
    assert(size <= MaxAOVCount);
    set(val);
}

inline size_t SpectrumStack::size() const
{
    return m_size;
}

inline void SpectrumStack::set(const float val)
{
    for (size_t i = 0; i < m_size; ++i)
        m_spectra[i].set(val);
}

inline Spectrum& SpectrumStack::operator[](const size_t index)
{
    assert(index < m_size);
    return m_spectra[index];
}

inline const Spectrum& SpectrumStack::operator[](const size_t index) const
{
    assert(index < m_size);
    return m_spectra[index];
}

inline SpectrumStack& SpectrumStack::operator+=(const SpectrumStack& rhs)
{
    assert(m_size == rhs.m_size);

    for (size_t i = 0; i < m_size; ++i)
        m_spectra[i] += rhs.m_spectra[i];

    return *this;
}

inline SpectrumStack& SpectrumStack::operator*=(const Spectrum& rhs)
{
    for (size_t i = 0; i < m_size; ++i)
        m_spectra[i] *= rhs;

    return *this;
}

inline SpectrumStack& SpectrumStack::operator*=(const float rhs)
{
    for (size_t i = 0; i < m_size; ++i)
        m_spectra[i] *= rhs;

    return *this;
}

inline void SpectrumStack::add(const size_t index, const Spectrum& rhs)
{
    if (index < m_size)
        m_spectra[index] += rhs;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_AOV_SPECTRUMSTACK_H
