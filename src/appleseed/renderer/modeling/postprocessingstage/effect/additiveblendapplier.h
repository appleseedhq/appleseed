
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

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Image; }

namespace renderer
{


//
// Image color blending applier.
//
// Blends the colors of a given image's tile with the matching tile of src_image,
// using the specified weight factors to compute a weighted average between them, i.e.:
//
//   "image.tile(x, y) = dst_weight * image.tile(x, y) + src_weight * src_image.tile(x, y)"
//
// Note that, for this, both images must share the same layout.
//

class AdditiveBlendApplier
  : public ImageEffectApplier
{
  public:
    // Constructor.
    explicit AdditiveBlendApplier(

        // Context.
        const foundation::Image&    src_image,

        // Settings.
        const float                 src_weight = 1.0f,
        const float                 dst_weight = 1.0f);

    // Delete this instance.
    void release() override;

    // Blend colors of the given tile with the corresponding tile of src_image.
    void apply(
        foundation::Image&          image,
        const std::size_t           tile_x,
        const std::size_t           tile_y) const override;

  private:
    const float                     m_src_weight;
    const float                     m_dst_weight;
    const foundation::Image&        m_src_image;
};

}   // namespace renderer
