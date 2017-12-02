
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
#include "foundation/math/vector.h"
#include "foundation/utility/autoreleaseptr.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Image; }
namespace foundation    { class Tile; }
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

    // This method is called before a sample gets rendered.
    virtual void on_sample_begin();

    // This method is called after a sample gets rendered.
    virtual void on_sample_end();

    // Write a value to the accumulator.
    virtual void write(
        const PixelContext&         pixel_context,
        const ShadingPoint&         shading_point,
        const ShadingComponents&    shading_components,
        ShadingResult&              shading_result);
};


//
// Unfiltered AOV accumulator base class.
//

class UnfilteredAOVAccumulator
  : public AOVAccumulator
{
  public:
    explicit UnfilteredAOVAccumulator(foundation::Image& image);

    void on_tile_begin(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y,
        const size_t                max_spp) override;

  protected:
    foundation::Image&  m_image;
    foundation::Tile*   m_tile;
    int                 m_tile_origin_x;
    int                 m_tile_origin_y;
    int                 m_tile_end_x;
    int                 m_tile_end_y;

    foundation::Tile& get_tile() const;

    bool outside_tile(const foundation::Vector2i& pi) const;

    static float square_distance_to_pixel_center(const foundation::Vector2d& ps);
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
        const size_t                tile_y,
        const size_t                max_spp);

    // This method is called after a tile gets rendered.
    void on_tile_end(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y);

    // This method is called before a sample gets rendered.
    void on_sample_begin();

    // This method is called after a sample gets rendered.
    void on_sample_end();

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


//
// UnfilteredAOVAccumulator class implementation.
//

inline foundation::Tile& UnfilteredAOVAccumulator::get_tile() const
{
    return *m_tile;
}

inline bool UnfilteredAOVAccumulator::outside_tile(
    const foundation::Vector2i&     pi) const
{
    return
        pi.x < m_tile_origin_x ||
        pi.y < m_tile_origin_y ||
        pi.x > m_tile_end_x ||
        pi.y > m_tile_end_y;
}

inline float UnfilteredAOVAccumulator::square_distance_to_pixel_center(
    const foundation::Vector2d&     ps)
{
    return static_cast<float>(
        foundation::square(ps.y - 0.5) + foundation::square(ps.y - 0.5));
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_AOV_AOVACCUMULATOR_H
