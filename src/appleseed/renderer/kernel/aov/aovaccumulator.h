
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/kernel/aov/aovsettings.h"
#include "renderer/modeling/aov/aovcontainer.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"

// OSL headers.
#include "OSL/accum.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Image; }
namespace foundation    { class Tile; }
namespace renderer      { class AOVComponents; }
namespace renderer      { class Frame; }
namespace renderer      { class PixelContext; }
namespace renderer      { class ShadingComponents; }
namespace renderer      { class ShadingPoint; }
namespace renderer      { class ShadingResult; }

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
        const size_t                tile_y,
        const size_t                max_spp);

    // This method is called after a tile gets rendered.
    virtual void on_tile_end(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y);

    // This method is called before a pixel gets rendered.
    virtual void on_pixel_begin(
        const foundation::Vector2i& pi);

    // This method is called after a pixel gets rendered.
    virtual void on_pixel_end(
        const foundation::Vector2i& pi);

    // This method is called before a sample gets rendered.
    virtual void on_sample_begin(
        const PixelContext&         pixel_context);

    // This method is called after a sample gets rendered.
    virtual void on_sample_end(
        const PixelContext&         pixel_context);

    // Write a value to the accumulator.
    virtual void write(
        const PixelContext&         pixel_context,
        const ShadingPoint&         shading_point,
        const ShadingComponents&    shading_components,
        const AOVComponents&        aov_components,
        ShadingResult&              shading_result);
};


//
// Unfiltered AOV accumulator base class.
//

class UnfilteredAOVAccumulator
  : public AOVAccumulator
{
  public:
    // Constructor.
    explicit UnfilteredAOVAccumulator(foundation::Image& image);

    // This method is called before a tile gets rendered.
    void on_tile_begin(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y,
        const size_t                max_spp) override;

    // This method is called after a tile gets rendered.
    void on_tile_end(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y) override;

  protected:
    foundation::Image&              m_image;
    foundation::Tile*               m_tile;
    size_t                          m_tile_origin_x;
    size_t                          m_tile_origin_y;
    foundation::AABB2i              m_cropped_tile_bbox;
};


//
// A collection of AOV accumulators.
//

class AOVAccumulatorContainer
  : public foundation::NonCopyable
{
  public:
    // Constructors.
    AOVAccumulatorContainer();
    explicit AOVAccumulatorContainer(const Frame& frame);

    // Destructor.
    ~AOVAccumulatorContainer();

    // This method is called before a tile gets rendered.
    void on_tile_begin(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y,
        const size_t                max_spp);

    // This method is called after a tile gets rendered.
    void on_tile_end(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y);

    // This method is called before a pixel gets rendered.
    void on_pixel_begin(
        const foundation::Vector2i& pi);

    // This method is called after a pixel gets rendered.
    void on_pixel_end(
        const foundation::Vector2i& pi);

    // This method is called before a sample gets rendered.
    void on_sample_begin(
        const PixelContext&         pixel_context);

    // This method is called after a sample gets rendered.
    void on_sample_end(
        const PixelContext&         pixel_context);

    // Write a sample to all accumulators.
    void write(
        const PixelContext&         pixel_context,
        const ShadingPoint&         shading_point,
        const ShadingComponents&    shading_components,
        const AOVComponents&        aov_components,
        ShadingResult&              shading_result);

  private:
    void init();
    bool insert(foundation::auto_release_ptr<AOVAccumulator> aov_accum);

    enum { MaxAOVAccumulatorCount = MaxAOVCount + 1 };  // MaxAOVCount + Beauty

    size_t          m_size;
    AOVAccumulator* m_accumulators[MaxAOVAccumulatorCount];

    OSL::AccumAutomata                  m_automata;
    std::unique_ptr<OSL::Accumulator>   m_accum_ptr;
};

}   // namespace renderer
