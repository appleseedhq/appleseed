
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_IMAGE_ICANVAS_H
#define APPLESEED_FOUNDATION_IMAGE_ICANVAS_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <cstddef>

//
// On Windows, define FOUNDATIONDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol FOUNDATIONDLL is
// defined to evaluate to nothing.
//

#ifndef FOUNDATIONDLL
#ifdef _WIN32
#ifdef APPLESEED_FOUNDATION_EXPORTS
#define FOUNDATIONDLL __declspec(dllexport)
#else
#define FOUNDATIONDLL __declspec(dllimport)
#endif
#else
#define FOUNDATIONDLL
#endif
#endif

namespace foundation
{

//
// Canvas interface.
//

class FOUNDATIONDLL ICanvas
  : public NonCopyable
{
  public:
    // Destructor.
    virtual ~ICanvas() {}

    // Access canvas properties.
    virtual const CanvasProperties& properties() const = 0;

    // Direct access to a given tile.
    virtual Tile& tile(
        const size_t    tile_x,
        const size_t    tile_y) = 0;
    virtual const Tile& tile(
        const size_t    tile_x,
        const size_t    tile_y) const = 0;

    // Direct access to a given pixel.
    uint8* pixel(
        const size_t    x,
        const size_t    y);
    const uint8* pixel(
        const size_t    x,
        const size_t    y) const;

    // Structured write access to a given pixel, with conversion if necessary.
    template <typename T>
    void set_pixel(
        const size_t    x,
        const size_t    y,
        const T&        val);                       // pixel value

    // Structured read access to a given pixel, with conversion if necessary.
    template <typename T>
    void get_pixel(
        const size_t    x,
        const size_t    y,
        T&              val) const;                 // [out] pixel value

    // Set all pixels to a given color.
    // This causes all tiles to be accessed, and created if necessary.
    template <typename T>
    void clear(
        const T&        val);                       // pixel value
};


//
// ICanvas class implementation.
//

// Direct access to a given pixel.
inline uint8* ICanvas::pixel(
    const size_t        x,
    const size_t        y)
{
    // Retrieve canvas properties.
    const CanvasProperties& props = properties();

    assert(x < props.m_canvas_width);
    assert(y < props.m_canvas_height);

    // Compute the coordinates of the tile containing the pixel (x, y).
    const size_t tile_x = truncate<size_t>(x * props.m_rcp_tile_width);
    const size_t tile_y = truncate<size_t>(y * props.m_rcp_tile_height);
    assert(tile_x < props.m_tile_count_x);
    assert(tile_y < props.m_tile_count_y);

    // Retrieve the tile.
    Tile& t = tile(tile_x, tile_y);

    // Compute the tile space coordinates of the pixel (x, y).
    const size_t pixel_x = x - tile_x * props.m_tile_width;
    const size_t pixel_y = y - tile_y * props.m_tile_height;
    assert(pixel_x < t.get_width());
    assert(pixel_y < t.get_height());

    // Access the pixel.
    return t.pixel(pixel_x, pixel_y);
}
inline const uint8* ICanvas::pixel(
    const size_t        x,
    const size_t        y) const
{
    // Retrieve canvas properties.
    const CanvasProperties& props = properties();

    assert(x < props.m_canvas_width);
    assert(y < props.m_canvas_height);

    // Compute the coordinates of the tile containing the pixel (x, y).
    const size_t tile_x = truncate<size_t>(x * props.m_rcp_tile_width);
    const size_t tile_y = truncate<size_t>(y * props.m_rcp_tile_height);
    assert(tile_x < props.m_tile_count_x);
    assert(tile_y < props.m_tile_count_y);

    // Retrieve the tile.
    const Tile& t = tile(tile_x, tile_y);

    // Compute the tile space coordinates of the pixel (x, y).
    const size_t pixel_x = x - tile_x * props.m_tile_width;
    const size_t pixel_y = y - tile_y * props.m_tile_height;
    assert(pixel_x < t.get_width());
    assert(pixel_y < t.get_height());

    // Access the pixel.
    return t.pixel(pixel_x, pixel_y);
}

// Structured write access to a given pixel, with conversion if necessary.
template <typename T>
inline void ICanvas::set_pixel(
    const size_t        x,
    const size_t        y,
    const T&            val)                        // pixel value
{
    // Retrieve canvas properties.
    const CanvasProperties& props = properties();

    Pixel::convert_to_format(
        &val[0],                                    // source begin
        &val[0] + props.m_channel_count,            // source end
        1,                                          // source stride
        props.m_pixel_format,                       // destination format
        pixel(x, y),                                // destination
        1);                                         // destination stride
}

// Structured read access to a given pixel, with conversion if necessary.
template <typename T>
inline void ICanvas::get_pixel(
    const size_t        x,
    const size_t        y,
    T&                  val) const                  // [out] pixel value
{
    // Retrieve canvas properties.
    const CanvasProperties& props = properties();

    const uint8* src = pixel(x, y);

    Pixel::convert_from_format(
        props.m_pixel_format,                       // source format
        src,                                        // source begin
        src + props.m_pixel_size,                   // source end
        1,                                          // source stride
        &val[0],                                    // destination
        1);                                         // destination stride
}

// Set all pixels to a given color.
template <typename T>
inline void ICanvas::clear(
    const T&            val)                        // pixel value
{
    // Retrieve canvas properties.
    const CanvasProperties& props = properties();

    // Clear all tiles.
    for (size_t tile_y = 0; tile_y < props.m_tile_count_y; ++tile_y)
    {
        for (size_t tile_x = 0; tile_x < props.m_tile_count_x; ++tile_x)
            tile(tile_x, tile_y).clear(val);
    }
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_ICANVAS_H
