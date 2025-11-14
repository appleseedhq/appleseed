
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Stephen Agyemang, The appleseedhq Organization
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
#include "renderer/kernel/rendering/ishadingresultframebufferfactory.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/math/aabb.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace renderer  { class Frame; }
namespace renderer  { class VarianceTrackingShadingResultFrameBuffer; }

namespace renderer
{

class VarianceTrackingShadingResultFrameBufferFactory
  : public IShadingResultFrameBufferFactory
{
  public:
    // Constructor.
    explicit VarianceTrackingShadingResultFrameBufferFactory(
        const Frame&                frame);

    // Destructor.
    ~VarianceTrackingShadingResultFrameBufferFactory() override;

    // Delete this instance.
    void release() override;

    ShadingResultFrameBuffer* create(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y,
        const foundation::AABB2u&   tile_bbox) override;

    void destroy(
        ShadingResultFrameBuffer*   framebuffer) override;

    size_t get_total_channel_count(
        const size_t aov_count) const override;

    void clear();

    // Return estimate of the mean pixel variance.
    float estimator_variance() const;

    // Transfer pixel variance estimates to image and return estimate of the mean pixel variance.
    float estimator_variance_to_image(
        foundation::Image&          image) const;

  private:
    std::vector<VarianceTrackingShadingResultFrameBuffer*> m_framebuffers;
};

}   // namespace renderer
