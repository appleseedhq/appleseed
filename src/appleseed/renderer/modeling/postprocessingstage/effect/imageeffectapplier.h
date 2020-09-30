
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

// appleseed.foundation headers.
#include "foundation/core/concepts/iunknown.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Image; }

namespace renderer
{

//
// Image effect algorithm applier.
//

class ImageEffectApplier
  : public foundation::IUnknown
{
  public:
    // Apply the image effect to a given tile.
    virtual void apply(
        foundation::Image&      image,
        const std::size_t       tile_x,
        const std::size_t       tile_y) const = 0;

    // Apply this effect on a given image, in parallel, by scheduling a job for each tile.
    void apply_on_tiles(
        foundation::Image&      image,
        const std::size_t       thread_count) const;
};

}   // namespace renderer
