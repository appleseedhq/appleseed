
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
#include "foundation/utility/job.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace foundation    { class Image; }

namespace renderer
{

//
// Image effect applier job.
//

class ImageEffectJob
  : public foundation::IJob
{
  public:
    // Constructor.
    ImageEffectJob(
        const ImageEffectApplier&   effect_applier,
        foundation::Image&          image,
        const std::size_t           tile_x,
        const std::size_t           tile_y);

    // Execute the job.
    void execute(const std::size_t  thread_index);

  private:
    const ImageEffectApplier&       m_effect_applier;
    foundation::Image&              m_image;
    const std::size_t               m_tile_x;
    const std::size_t               m_tile_y;
};


//
// Creates jobs to apply an image effect to a complete image.
//

class ImageEffectJobFactory
{
  public:
    typedef std::vector<ImageEffectJob*> EffectJobVector;

    // Create effect jobs for a given image.
    EffectJobVector create(
        foundation::Image&          image,
        const ImageEffectApplier&   effect_applier) const;
};

}   // namespace renderer
