
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_AOV_AOVACCUMULATOR_H
#define APPLESEED_RENDERER_KERNEL_AOV_AOVACCUMULATOR_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovsettings.h"
#include "renderer/modeling/aov/aovcontainer.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"
#include "foundation/utility/autoreleaseptr.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class Camera; }
namespace renderer  { class Frame; }
namespace renderer  { class PixelContext; }
namespace renderer  { class ShadingComponents; }
namespace renderer  { class ShadingPoint; }
namespace renderer  { class ShadingResult; }

namespace renderer
{

//
// AOV accumulator base class.
//

class AOVAccumulator
  : public foundation::NonCopyable
{
  public:
    // Destructor.
    virtual ~AOVAccumulator();

    // Delete this instance.
    void release();

    // This method is called before a tile gets rendered.
    virtual void on_tile_begin(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y);

    // This method is called after a tile gets rendered.
    virtual void on_tile_end(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y);

    // Write a value to the accumulator.
    virtual void write(
        const PixelContext&         pixel_context,
        const ShadingPoint&         shading_point,
        const ShadingComponents&    shading_components,
        ShadingResult&              shading_result);
};


//
// A collection of AOV accumulators.
//

class AOVAccumulatorContainer
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    AOVAccumulatorContainer();

    // Constructor.
    explicit AOVAccumulatorContainer(const AOVContainer& aovs);

    // Destructor.
    ~AOVAccumulatorContainer();

    // This method is called before a tile gets rendered.
    void on_tile_begin(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y);

    // This method is called after a tile gets rendered.
    void on_tile_end(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y);

    // Write a sample to all accumulators.
    void write(
        const PixelContext&         pixel_context,
        const ShadingPoint&         shading_point,
        const ShadingComponents&    shading_components,
        ShadingResult&              shading_result);

  private:
    void init();
    bool insert(foundation::auto_release_ptr<AOVAccumulator> aov_accum);

    enum { MaxAovAccumulators = MaxAOVCount + 1 };  // MaxAOVCount + Beauty

    size_t          m_size;
    AOVAccumulator* m_accumulators[MaxAovAccumulators];
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_AOV_AOVACCUMULATOR_H
