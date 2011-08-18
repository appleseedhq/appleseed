
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_IMAGE_IMAGE_H
#define APPLESEED_FOUNDATION_IMAGE_IMAGE_H

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/icanvas.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"

// Standard headers.
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
// An image whose tiles are lazily constructed.
//
// Tiles are initially blank.
//

class FOUNDATIONDLL Image
  : public ICanvas
{
  public:
    // Constructors.
    Image(
        const size_t        image_width,        // image width, in pixels
        const size_t        image_height,       // image height, in pixels
        const size_t        tile_width,         // tile width, in pixels
        const size_t        tile_height,        // tile height, in pixels
        const size_t        channel_count,      // number of channels
        const PixelFormat   pixel_format);      // pixel format
    explicit Image(const CanvasProperties& props);

    // Copy constructor.
    Image(const Image&      rhs);

    // Destructor.
    virtual ~Image();

    // Access canvas properties.
    virtual const CanvasProperties& properties() const;

    // Direct access to a given tile.
    virtual Tile& tile(
        const size_t        tile_x,
        const size_t        tile_y);
    virtual const Tile& tile(
        const size_t        tile_x,
        const size_t        tile_y) const;

    // Set a given tile. Ownership of the tile is transfered to the Image class.
    // If a tile already exists at the given coordinates, it gets replaced.
    void set_tile(
        const size_t        tile_x,
        const size_t        tile_y,
        Tile*               tile);

    // Set all pixels of all tiles to a given color.
    template <typename T>
    void clear(const T&     val);               // pixel value

  protected:
    CanvasProperties        m_props;            // canvas properties
    Tile**                  m_tiles;            // tile array
};


//
// Image class implementation.
//

inline const CanvasProperties& Image::properties() const
{
    return m_props;
}

template <typename T>
void Image::clear(const T& val)
{
    for (size_t ty = 0; ty < m_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < m_props.m_tile_count_x; ++tx)
            tile(tx, ty).clear(val);
    }
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_IMAGE_H
