
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Tiago Chaves, The appleseedhq Organization
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

#pragma once

// appleseed.renderer headers.
#include "renderer/modeling/postprocessingstage/effect/imageeffectapplier.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Image; }

namespace renderer
{


//
// Image resampling applier.
//
// Fills an image's tiles given a source image as reference (src_image).
//
// Two different sampling filters are used, depending on the specified SamplingMode:
//   * DOWN: Assumes we want to shrink src_image into the given image
//   * UP: Assumes we want to enlarge src_image onto the given image
//
// References on image resizing/rescaling and resampling:
//
//   http://www.imagemagick.org/Usage/resize/#resize
//   http://www.imagemagick.org/Usage/filter/#filter
//   http://www.imagemagick.org/Usage/filter/nicolas/#detailed
//   https://en.wikipedia.org/wiki/Image_scaling#Mathematical
//

class ResampleApplier
  : public ImageEffectApplier
{
  public:
    typedef enum { DOWN, UP }       SamplingMode;

    // Constructor.
    explicit ResampleApplier(

        // Context.
        const foundation::Image&    src_image,

        // Settings.
        const SamplingMode          mode);

    // Delete this instance.
    void release() override;

    // Fill the given image tile by sampling from src_image pixels.
    void apply(
        foundation::Image&          image,
        const std::size_t           tile_x,
        const std::size_t           tile_y) const override;

  private:
    const SamplingMode              m_mode;
    const std::size_t               m_src_width;
    const std::size_t               m_src_height;
    const std::size_t               m_border_size;
    const foundation::Image         m_src_image_with_border;

    // Sample m_src_image_with_border at coordinates (fx, fy), based on m_mode.
    const foundation::Color3f sample(
        const float                 fx,
        const float                 fy) const;
};

}   // namespace renderer
