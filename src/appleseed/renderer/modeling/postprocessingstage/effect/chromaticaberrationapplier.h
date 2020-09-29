
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
// Simulates transverse (lateral) chromatic aberration of lenses by sampling pixels
// with slightly different shifts for each color channel, to introduce color fringing.
//
// References:
//
//   http://loopit.dk/rendering_inside.pdf (slides 19-20)
//   https://en.wikipedia.org/wiki/Chromatic_aberration#Types
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
        const float                 strength,       // controls intensity by (cubically) increasing distortion towards the edges
        const std::size_t           sample_count);  // 3 = classic "three color split" (red, green, blue), while higher values use
                                                    // more colors, leading to a "rainbow-like" effect (see the first reference above)

    // Delete this instance.
    void release() override;

    // Apply chromatic aberration to the given image tile.
    void apply(
        foundation::Image&          image,
        const std::size_t           tile_x,
        const std::size_t           tile_y) const override;

  private:
    const float                     m_strength;
    const std::size_t               m_sample_count;
    const foundation::Image         m_src_image;
    const foundation::Vector2f      m_resolution;

    // Samples a pixel from m_src_image given uv coordinates in the [0, 1] range.
    foundation::Color3f sample_at(
        const foundation::Vector2f& uv) const;
};

}   // namespace renderer
