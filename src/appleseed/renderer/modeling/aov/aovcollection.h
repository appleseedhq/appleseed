
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_RENDERER_MODELING_AOV_AOVCOLLECTION_H
#define APPLESEED_RENDERER_MODELING_AOV_AOVCOLLECTION_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace renderer
{

//
// This is nothing more than an array of spectra.
//

class AOVCollection
  : public foundation::NonCopyable
{
  public:
    static const size_t MaxSize = 8;

    AOVCollection();
    explicit AOVCollection(const size_t size);

    void set_size(const size_t size);
    size_t size() const;

    void set(const float val);

    Spectrum& operator[](const size_t index);
    const Spectrum& operator[](const size_t index) const;

    AOVCollection& operator+=(const AOVCollection& rhs);
    AOVCollection& operator*=(const Spectrum& rhs);
    AOVCollection& operator*=(const float rhs);
    AOVCollection& operator/=(const float rhs);

    void add(const size_t index, const Spectrum& rhs);

  private:
    Spectrum    m_aovs[MaxSize];
    size_t      m_size;
};


//
// AOVCollection class implementation.
//

inline AOVCollection::AOVCollection()
  : m_size(0)
{
}

inline AOVCollection::AOVCollection(const size_t size)
  : m_size(size)
{
    assert(size <= MaxSize);
}

inline void AOVCollection::set_size(const size_t size)
{
    assert(size <= MaxSize);
    m_size = size;
}

inline size_t AOVCollection::size() const
{
    return m_size;
}

inline Spectrum& AOVCollection::operator[](const size_t index)
{
    assert(index < m_size);
    return m_aovs[index];
}

inline const Spectrum& AOVCollection::operator[](const size_t index) const
{
    assert(index < m_size);
    return m_aovs[index];
}

inline void AOVCollection::add(const size_t index, const Spectrum& rhs)
{
    if (index < m_size)
        m_aovs[index] += rhs;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_AOV_AOVCOLLECTION_H
