
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
// Fills an image's tiles given a source image as reference (src_image), similar to ResampleApplier.
// However, it assumes that the source image's dimensions will either be halved or doubled, so that
// smaller sampling filters are used, leading to not as good visual results, but faster processing.
//
// The SamplingMode values are:
//   * HALVE: performs downsampling by box filtering
//   * DOUBLE: performs upsampling by bilinear filtering
//
// References:
//
//   https://en.wikipedia.org/wiki/Image_scaling#Box_sampling
//   https://en.wikipedia.org/wiki/Image_scaling#Bilinear_and_bicubic_algorithms
//

class ResampleX2Applier
  : public ImageEffectApplier
{
  public:
    typedef enum { HALVE, DOUBLE }  SamplingMode;

    // Constructor.
    explicit ResampleX2Applier(

        // Context.
        const foundation::Image&    src_image,

        // Settings.
        const SamplingMode          mode);

    // Delete this instance.
    void release() override;

    // Fill the given image tile by sampling from src_image pixels.
    // Note: its dimensions should be either half or double the size of src_image's.
    void apply(
        foundation::Image&          image,
        const std::size_t           tile_x,
        const std::size_t           tile_y) const override;

  private:
    const SamplingMode              m_mode;
    const std::size_t               m_src_width;
    const std::size_t               m_src_height;
    const foundation::Image&        m_src_image;

    // Sample m_src_image at coordinates (fx, fy), based on m_mode.
    const foundation::Color3f sample(
        const float                 fx,
        const float                 fy) const;
};

}   // namespace renderer
