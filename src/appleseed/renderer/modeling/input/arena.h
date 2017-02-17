
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

#ifndef APPLESEED_RENDERER_MODELING_INPUT_ARENA_H
#define APPLESEED_RENDERER_MODELING_INPUT_ARENA_H

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cstddef>

namespace renderer
{

//
// An arena is the storage for inputs values and additional data such as precomputed values.
//

class Arena
{
  public:
    enum { DataSize = 32 * 1024 };  // bytes

    const foundation::uint8* data(const size_t offset = 0) const;
    foundation::uint8* data(const size_t offset = 0);

    template <typename T> const T& as(const size_t offset = 0) const;
    template <typename T> T& as(const size_t offset = 0);

  private:
    APPLESEED_SIMD4_ALIGN foundation::uint8 m_data[DataSize];
};


//
// Arena class implementation.
//

inline const foundation::uint8* Arena::data(const size_t offset) const
{
    return m_data + offset;
}

inline foundation::uint8* Arena::data(const size_t offset)
{
    return m_data + offset;
}

template <typename T>
inline const T& Arena::as(const size_t offset) const
{
    return *reinterpret_cast<const T*>(data(offset));
}

template <typename T>
inline T& Arena::as(const size_t offset)
{
    return *reinterpret_cast<T*>(data(offset));
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_INPUT_ARENA_H
