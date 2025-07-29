
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/image/canvasproperties.h"
#include "foundation/image/icanvas.h"
#include "foundation/image/pixel.h"

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

    // Construct an image by converting an existing image to a given pixel format,
    // and allowing reordering, deletion of channels.
    Image(
        const Image&        source,             // source image
        const PixelFormat   pixel_format,       // new pixel format
        const size_t*       shuffle_table);     // channel shuffling table

    // Destructor.
    ~Image() override;

    // Delete this instance.
    void release() override;

    // Access canvas properties.
    const CanvasProperties& properties() const override;

    // Direct access to a given tile.
    // It is safe to access distinct tiles from multiple threads concurrently
    // (however it is not safe to access the same tile from multiple threads).
    Tile& tile(
        const size_t        tile_x,
        const size_t        tile_y) override;
    const Tile& tile(
        const size_t        tile_x,
        const size_t        tile_y) const override;

    // Set a given tile. Ownership of the tile is transferred to the Image class.
    // If a tile already exists at the given coordinates, it gets replaced.
    void set_tile(
        const size_t        tile_x,
        const size_t        tile_y,
        Tile*               tile);

    // Copy the contents of another image of identical geometry (but possibly with a different pixel format).
    void copy_from(const Image& source);

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

}   // namespace foundation
