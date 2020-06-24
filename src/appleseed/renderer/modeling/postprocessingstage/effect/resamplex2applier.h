
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
// Image resampling x2 applier.
//

typedef enum { HALVE, DOUBLE } SamplingX2Mode;

class ResampleX2Applier
  : public ImageEffectApplier
{
  public:
    // Constructor.
    explicit ResampleX2Applier(
        const foundation::Image&    src_image,
        const SamplingX2Mode        mode);

    // Delete this instance.
    void release() override;

    // Fill the given image tile by sampling from src_image pixels.
    // Note: its dimensions should be either half or double the size of src_image's.
    void apply(
        foundation::Image&          image,
        const std::size_t           tile_x,
        const std::size_t           tile_y) const override;

  private:
    // Settings.
    const SamplingX2Mode            m_mode;

    // Context.
    const std::size_t               m_src_width;
    const std::size_t               m_src_height;
    const foundation::Image&        m_src_image;
};

}   // namespace renderer
