
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_PIXELCONTEXT_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_PIXELCONTEXT_H

// appleseed.foundation headers.
#include "foundation/math/vector.h"

namespace renderer
{

//
// This class identifies the pixel currently being rendered throughout the
// Tile Renderer -> Pixel Renderer -> Sample Renderer -> Surface Shader -> Lighting Engine chain.

class PixelContext
{
  public:
    // Constructor.
    PixelContext(const int x, const int y);

    // Return pixel coordinates.
    int get_pixel_x() const;
    int get_pixel_y() const;
    foundation::Vector2i get_pixel_coordinates() const;

  private:
    const int m_x;
    const int m_y;
};


//
// PixelContext class implementation.
//

inline PixelContext::PixelContext(const int x, const int y)
  : m_x(x)
  , m_y(y)
{
}

inline int PixelContext::get_pixel_x() const
{
    return m_x;
}

inline int PixelContext::get_pixel_y() const
{
    return m_y;
}

inline foundation::Vector2i PixelContext::get_pixel_coordinates() const
{
    return foundation::Vector2i(m_x, m_y);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_PIXELCONTEXT_H
