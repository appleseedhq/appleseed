
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_DEVELOPEDPIXELBUFFER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_DEVELOPEDPIXELBUFFER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/math/aabb.h"

// Standard headers.
#include <cstddef>
#include <vector>

namespace renderer
{

//
// A buffer to store fully developed, color-space corrected pixels.
// An additional row and column are added at the top and on the left.
// All pixel coordinates must be in [-1,-1] x [width-1, height-1].
//

class DevelopedPixelBuffer
  : public foundation::NonCopyable
{
  public:
    DevelopedPixelBuffer(
        const int                       width,
        const int                       height,
        const size_t                    aov_count,
        const foundation::AABB2u&       tile_bbox,
        const foundation::ColorSpace    color_space,
        const float                     max_contrast);

    void set_pixel_color(
        const int                       x,
        const int                       y,
        const foundation::Color4f&      linear_rgba);

    void set_pixel_aov(
        const int                       x,
        const int                       y,
        const size_t                    aov_index,
        const foundation::Color3f&      linear_rgb);

    bool check_contrast(
        const int                       ix,
        const int                       iy,
        const int                       tx,
        const int                       ty) const;

  private:
    const int                           m_width;
    const int                           m_height;
    const size_t                        m_channel_count;
    const foundation::AABB2u            m_tile_bbox;
    const foundation::ColorSpace        m_color_space;
    const float                         m_max_contrast;
    std::vector<float>                  m_buffer;

    float* pixel(
        const int   x,
        const int   y);

    const float* pixel(
        const int   x,
        const int   y) const;

    bool check_contrast_between(
        const int   x1,
        const int   y1,
        const int   x2,
        const int   y2) const;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_DEVELOPEDPIXELBUFFER_H
