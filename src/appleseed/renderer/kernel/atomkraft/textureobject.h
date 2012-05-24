
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

#ifndef APPLESEED_RENDERER_KERNEL_TEXTURING_EWA_TEXTURESAMPLER_H
#define APPLESEED_RENDERER_KERNEL_TEXTURING_EWA_TEXTURESAMPLER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace foundation    { class Image; }

namespace renderer
{

//
// The interface between appleseed.foundation and AtomKraft-specific code.
//

class TextureObject
  : public foundation::NonCopyable
{
  public:
    explicit TextureObject(foundation::Image& texture);

    int width() const;
    int height() const;

    void get(
        const int       x,
        const int       y,
        float           texel[]) const;

    void put(
        const int       x,
        const int       y,
        const float     texel[]);

  private:
    foundation::Image&  m_texture;
    const size_t        m_width;
    const size_t        m_height;
    const size_t        m_channel_count;
    std::vector<float>  m_texels;
};


//
// TextureObject class implementation.
//

inline int TextureObject::width() const
{
    return static_cast<int>(m_width);
}

inline int TextureObject::height() const
{
    return static_cast<int>(m_height);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_TEXTURING_EWA_TEXTURESAMPLER_H
