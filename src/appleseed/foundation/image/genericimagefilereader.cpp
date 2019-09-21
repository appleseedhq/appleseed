
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

// Interface header.
#include "genericimagefilereader.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/genericprogressiveimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"

// Standard headers.
#include <cstddef>
#include <memory>

namespace foundation
{

//
// GenericImageFileReader class implementation.
//

Image* GenericImageFileReader::read(
    const char*         filename,
    ImageAttributes*    image_attributes)
{
    GenericProgressiveImageFileReader reader;
    reader.open(filename);

    CanvasProperties props;
    reader.read_canvas_properties(props);

    if (image_attributes)
        reader.read_image_attributes(*image_attributes);

    std::unique_ptr<Image> image(
        new Image(
            props.m_canvas_width,
            props.m_canvas_height,
            props.m_tile_width,
            props.m_tile_height,
            props.m_channel_count,
            props.m_pixel_format));

    for (size_t tile_y = 0; tile_y < props.m_tile_count_y; ++tile_y)
    {
        for (size_t tile_x = 0; tile_x < props.m_tile_count_x; ++tile_x)
        {
            std::unique_ptr<Tile> tile(reader.read_tile(tile_x, tile_y));
            image->set_tile(tile_x, tile_y, tile.release());
        }
    }

    reader.close();

    return image.release();
}

}   // namespace foundation
