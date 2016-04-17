
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "foundation/core/concepts/iunknown.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/icanvas.h"
#include "foundation/image/pixel.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Tile; }

namespace foundation
{

//
// An image whose tiles are lazily constructed.
//
// Tiles are initially blank.
//

class APPLESEED_DLLSYMBOL Image
  : public ICanvas
  , public IUnknown
{
  public:
    // Construct an empty image.
    Image(
        const size_t        image_width,        // image width, in pixels
        const size_t        image_height,       // image height, in pixels
        const size_t        tile_width,         // tile width, in pixels
        const size_t        tile_height,        // tile height, in pixels
        const size_t        channel_count,
        const PixelFormat   pixel_format);

    // An alternative way to construct an empty image.
    explicit Image(const CanvasProperties& props);

    // Copy constructor, duplicates both the layout and the data of the source image.
    Image(const Image& rhs);

    // Copy image data but allow to tweak the layout.
    Image(
        const Image&        source,
        const size_t        tile_width,         // tile width, in pixels
        const size_t        tile_height,        // tile height, in pixels
        const PixelFormat   pixel_format);

    // Destructor.
    virtual ~Image();

    // Delete this instance.
    virtual void release() APPLESEED_OVERRIDE;

    // Access canvas properties.
    virtual const CanvasProperties& properties() const APPLESEED_OVERRIDE;

    // Direct access to a given tile.
    virtual Tile& tile(
        const size_t        tile_x,
        const size_t        tile_y) APPLESEED_OVERRIDE;
    virtual const Tile& tile(
        const size_t        tile_x,
        const size_t        tile_y) const APPLESEED_OVERRIDE;

    // Set a given tile. Ownership of the tile is transfered to the Image class.
    // If a tile already exists at the given coordinates, it gets replaced.
    void set_tile(
        const size_t        tile_x,
        const size_t        tile_y,
        Tile*               tile);

  protected:
    CanvasProperties        m_props;
    Tile**                  m_tiles;
};


//
// Image class implementation.
//

inline const CanvasProperties& Image::properties() const
{
    return m_props;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_IMAGE_H
