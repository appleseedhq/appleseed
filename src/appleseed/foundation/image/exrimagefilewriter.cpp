
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

// Interface header.
#include "exrimagefilewriter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/exceptionunsupportedimageformat.h"
#include "foundation/image/exrutils.h"
#include "foundation/image/icanvas.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"

// OpenEXR headers.
#include "foundation/platform/exrheaderguards.h"
BEGIN_EXR_INCLUDES
#include "OpenEXR/IexBaseExc.h"
#include "OpenEXR/ImathBox.h"
#include "OpenEXR/ImathVec.h"
#include "OpenEXR/ImfChannelList.h"
#include "OpenEXR/ImfFrameBuffer.h"
#include "OpenEXR/ImfHeader.h"
#include "OpenEXR/ImfPixelType.h"
#include "OpenEXR/ImfTileDescription.h"
#include "OpenEXR/ImfTiledOutputFile.h"
END_EXR_INCLUDES

// Standard headers.
#include <cassert>
#include <cstddef>

using namespace Iex;
using namespace Imath;
using namespace Imf;
using namespace std;

namespace foundation
{

//
// EXRImageFileWriter class implementation.
//

namespace
{
    const char* ChannelName[] = { "R", "G", "B", "A" };
}

void EXRImageFileWriter::write(
    const char*             filename,
    const ICanvas&          image,
    const ImageAttributes&  image_attributes)
{
    initialize_openexr();

    try
    {
        // Retrieve canvas properties.
        const CanvasProperties& props = image.properties();

        // todo: lift this limitation.
        assert(props.m_channel_count <= 4);

        // Figure out the pixel type, based on the pixel format of the image.
        PixelType pixel_type = FLOAT;
        switch (props.m_pixel_format)
        {
          case PixelFormatUInt32: pixel_type = UINT; break;
          case PixelFormatHalf: pixel_type = HALF; break;
          case PixelFormatFloat: pixel_type = FLOAT; break;
          default: throw ExceptionUnsupportedImageFormat();
        }

        // Construct TileDescription object.
        const TileDescription tile_desc(
            static_cast<unsigned int>(props.m_tile_width),
            static_cast<unsigned int>(props.m_tile_height),
            ONE_LEVEL);

        // Construct ChannelList object.
        ChannelList channels;
        for (size_t c = 0; c < props.m_channel_count; ++c)
            channels.insert(ChannelName[c], Channel(pixel_type));

        // Construct Header object.
        Header header(
            static_cast<int>(props.m_canvas_width),
            static_cast<int>(props.m_canvas_height));
        header.setTileDescription(tile_desc);
        header.channels() = channels;

        // Add image attributes to the Header object.
        add_attributes(image_attributes, header);

        // Create the output file.
        TiledOutputFile file(filename, header);

        // Write tiles.
        for (size_t y = 0; y < props.m_tile_count_y; ++y)
        {
            for (size_t x = 0; x < props.m_tile_count_x; ++x)
            {
                const int ix              = static_cast<int>(x);
                const int iy              = static_cast<int>(y);
                const Box2i range         = file.dataWindowForTile(ix, iy);
                const Tile& tile          = image.tile(x, y);
                const size_t channel_size = Pixel::size(tile.get_pixel_format());
                const size_t stride_x     = channel_size * props.m_channel_count;
                const size_t stride_y     = stride_x * tile.get_width();
                const size_t tile_origin  = range.min.x * stride_x + range.min.y * stride_y;
                const char* tile_base     = reinterpret_cast<const char*>(tile.pixel(0, 0)) - tile_origin;

                // Construct FrameBuffer object.
                FrameBuffer framebuffer;
                for (size_t c = 0; c < props.m_channel_count; ++c)
                {
                    const char* base = tile_base + c * channel_size;
                    framebuffer.insert(
                        ChannelName[c],
                        Slice(
                            pixel_type,
                            const_cast<char*>(base),
                            stride_x,
                            stride_y));
                }

                // Write tile.
                file.setFrameBuffer(framebuffer);
                file.writeTile(ix, iy);
            }
        }
    }
    catch (const BaseExc& e)
    {
        // I/O error.
        throw ExceptionIOError(e.what());
    }
}

}   // namespace foundation
