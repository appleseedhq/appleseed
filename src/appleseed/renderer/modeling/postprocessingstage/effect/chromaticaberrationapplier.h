
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
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

namespace renderer
{


//
// Chromatic aberration applier.
//
// Simulates transverse (lateral) chromatic aberration of lenses by resampling
// pixels with a slight shift for each color channel, to introduce color fringing.
//
// Reference:
//
//   https://en.wikipedia.org/wiki/Chromatic_aberration#Types
//   https://github.com/keijiro/KinoFringe/
//

class ChromaticAberrationApplier
  : public ImageEffectApplier
{
  public:
    // Constructor.
    explicit ChromaticAberrationApplier(

        // Context.
        const foundation::Image&    src_image,

        // Settings.
        const float                 offset,
        const std::size_t           min_shift,
        const std::size_t           max_shift);

    // Delete this instance.
    void release() override;

    // Apply chromatic aberration to the given image tile.
    void apply(
        foundation::Image&          image,
        const std::size_t           tile_x,
        const std::size_t           tile_y) const override;

  private:
    const float                     m_offset;
    const std::size_t               m_min_shift;
    const std::size_t               m_max_shift;
    const foundation::Image         m_src_image;

    foundation::Color3f sample_at(
        const std::size_t           pixel_x,
        const std::size_t           pixel_y) const;
};

}   // namespace renderer