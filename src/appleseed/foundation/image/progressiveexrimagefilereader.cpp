
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "progressiveexrimagefilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/exceptionunsupportedimageformat.h"
#include "foundation/image/exrutils.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"
#include "foundation/utility/log.h"
#include "foundation/utility/memory.h"

// OpenEXR headers.
#include "foundation/platform/exrheaderguards.h"
BEGIN_EXR_INCLUDES
#include "OpenEXR/IexBaseExc.h"
#include "OpenEXR/ImathBox.h"
#include "OpenEXR/ImathVec.h"
#include "OpenEXR/ImfChannelList.h"
#include "OpenEXR/ImfFrameBuffer.h"
#include "OpenEXR/ImfHeader.h"
#include "OpenEXR/ImfInputFile.h"
#include "OpenEXR/ImfPixelType.h"
#include "OpenEXR/ImfTileDescription.h"
#include "OpenEXR/ImfTiledInputFile.h"
END_EXR_INCLUDES

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstring>
#include <memory>
#include <vector>

using namespace Iex;
using namespace Imath;
using namespace Imf;
using namespace std;

namespace foundation
{

//
// ProgressiveEXRImageFileReader class implementation.
//

struct ProgressiveEXRImageFileReader::Impl
{
    Logger*                     m_logger;
    size_t                      m_default_tile_width;
    size_t                      m_default_tile_height;
    auto_ptr<InputFile>         m_scanline_file;
    auto_ptr<TiledInputFile>    m_tiled_file;
    bool                        m_is_tiled;
    const Channel*              m_red;
    const Channel*              m_green;
    const Channel*              m_blue;
    const Channel*              m_alpha;
    Box2i                       m_dw;
    CanvasProperties            m_props;
    vector<uint8>               m_scanlines;
    size_t                      m_last_tile_y;
};

ProgressiveEXRImageFileReader::ProgressiveEXRImageFileReader(Logger* logger)
  : impl(new Impl())
{
    impl->m_logger = logger;
    impl->m_default_tile_width = 32;
    impl->m_default_tile_height = 32;
}

ProgressiveEXRImageFileReader::ProgressiveEXRImageFileReader(
    const size_t        default_tile_width,
    const size_t        default_tile_height)
  : impl(new Impl())
{
    impl->m_logger = 0;
    impl->m_default_tile_width = default_tile_width;
    impl->m_default_tile_height = default_tile_height;
}

ProgressiveEXRImageFileReader::ProgressiveEXRImageFileReader(
    Logger*             logger,
    const size_t        default_tile_width,
    const size_t        default_tile_height)
  : impl(new Impl())
{
    impl->m_logger = logger;
    impl->m_default_tile_width = default_tile_width;
    impl->m_default_tile_height = default_tile_height;
}

ProgressiveEXRImageFileReader::~ProgressiveEXRImageFileReader()
{
    if (is_open())
        close();

    delete impl;
}

void ProgressiveEXRImageFileReader::open(const char* filename)
{
    assert(filename);
    assert(!is_open());

    initialize_openexr();

    try
    {
        try
        {
            // Open the file for reading, assuming the file is tiled.
            impl->m_tiled_file.reset(new TiledInputFile(filename));
            impl->m_is_tiled = true;
        }
        catch (const ArgExc&)
        {
            // The file is not tiled, open it as a scanline file.
            impl->m_scanline_file.reset(new InputFile(filename));
            impl->m_is_tiled = false;

            // Print a warning message if a logger was provided.
            if (impl->m_logger)
            {
                LOG_WARNING(
                    *impl->m_logger,
                    "the texture file %s is not tiled, performances might suffer.",
                    filename);
            }
        }

        // Retrieve the file header, and perform basic sanity checks on it.
        const Header& header = impl->m_is_tiled
            ? impl->m_tiled_file->header()
            : impl->m_scanline_file->header();
        header.sanityCheck(impl->m_is_tiled);

        // Retrieve the image channels.
        const ChannelList& channels = header.channels();
        impl->m_red = channels.findChannel("R");
        impl->m_green = channels.findChannel("G");
        impl->m_blue = channels.findChannel("B");
        impl->m_alpha = channels.findChannel("A");
        if (!(impl->m_red && impl->m_green && impl->m_blue))
            throw ExceptionUnsupportedImageFormat();

        // Make sure all channels use the same pixel format.
        if (!(impl->m_red->type == impl->m_green->type && impl->m_red->type == impl->m_blue->type))
            throw ExceptionUnsupportedImageFormat();
        if (impl->m_alpha)
        {
            if (impl->m_red->type != impl->m_alpha->type)
                throw ExceptionUnsupportedImageFormat();
        }

        // Figure out the pixel format, based on the pixel type of the file.
        PixelFormat pixel_format = PixelFormatFloat;
        switch (impl->m_red->type)
        {
          case UINT: pixel_format = PixelFormatUInt32; break;
          case HALF: pixel_format = PixelFormatHalf; break;
          case FLOAT: pixel_format = PixelFormatFloat; break;
          default: throw ExceptionUnsupportedImageFormat();
        }

        // Retrieve the dimensions of the image.
        impl->m_dw = header.dataWindow();

        // Compute the dimensions of the canvas.
        const size_t canvas_width = static_cast<size_t>(impl->m_dw.max.x - impl->m_dw.min.x + 1);
        const size_t canvas_height = static_cast<size_t>(impl->m_dw.max.y - impl->m_dw.min.y + 1);

        // Retrieve the dimensions of the tiles.
        size_t tile_width, tile_height;
        if (impl->m_is_tiled)
        {
            assert(header.hasTileDescription());
            const TileDescription& td = header.tileDescription();
            tile_width = static_cast<size_t>(td.xSize);
            tile_height = static_cast<size_t>(td.ySize);
        }
        else
        {
            tile_width = impl->m_default_tile_width;
            tile_height = impl->m_default_tile_height;
        }

        // Retrieve the number of tiles in each direction.
        size_t tile_count_x, tile_count_y;
        if (impl->m_is_tiled)
        {
            tile_count_x = static_cast<size_t>(impl->m_tiled_file->numXTiles());
            tile_count_y = static_cast<size_t>(impl->m_tiled_file->numYTiles());
        }
        else
        {
            const double nx = ceil(static_cast<double>(canvas_width) / tile_width);
            const double ny = ceil(static_cast<double>(canvas_height) / tile_height);
            tile_count_x = truncate<size_t>(nx);
            tile_count_y = truncate<size_t>(ny);
        }

        // Set canvas properties.
        impl->m_props.m_canvas_width = canvas_width;
        impl->m_props.m_canvas_height = canvas_height;
        impl->m_props.m_rcp_canvas_width = 1.0 / impl->m_props.m_canvas_width;
        impl->m_props.m_rcp_canvas_height = 1.0 / impl->m_props.m_canvas_height;
        impl->m_props.m_pixel_count = impl->m_props.m_canvas_width * impl->m_props.m_canvas_height;
        impl->m_props.m_tile_width = tile_width;
        impl->m_props.m_tile_height = tile_height;
        impl->m_props.m_rcp_tile_width = 1.0 / impl->m_props.m_tile_width;
        impl->m_props.m_rcp_tile_height = 1.0 / impl->m_props.m_tile_height;
        impl->m_props.m_tile_count_x = tile_count_x;
        impl->m_props.m_tile_count_y = tile_count_y;
        impl->m_props.m_tile_count = impl->m_props.m_tile_count_x * impl->m_props.m_tile_count_y;
        impl->m_props.m_pixel_format = pixel_format;
        impl->m_props.m_channel_count = impl->m_alpha ? 4 : 3;
        impl->m_props.m_pixel_size = impl->m_props.m_channel_count * Pixel::size(impl->m_props.m_pixel_format);

        // Allocate memory to store scanlines, if the file is not tiled.
        if (!impl->m_is_tiled)
        {
            const size_t size =
                  impl->m_props.m_canvas_width
                * impl->m_props.m_pixel_size
                * tile_height;
            impl->m_scanlines.resize(size);
        }

        // No tile previously accessed.
        impl->m_last_tile_y = ~0;
    }
    catch (const BaseExc& e)
    {
        // I/O error.
        throw ExceptionIOError(e.what());
    }
}

void ProgressiveEXRImageFileReader::close()
{
    assert(is_open());

    impl->m_scanline_file.reset();
    impl->m_tiled_file.reset();

    clear_release_memory(impl->m_scanlines);
}

bool ProgressiveEXRImageFileReader::is_open() const
{
    return
        impl->m_is_tiled
            ? impl->m_tiled_file.get() != 0
            : impl->m_scanline_file.get() != 0;
}

void ProgressiveEXRImageFileReader::read_canvas_properties(
    CanvasProperties&   props)
{
    assert(is_open());
    props = impl->m_props;
}

void ProgressiveEXRImageFileReader::read_image_attributes(
    ImageAttributes&    attrs)
{
    assert(is_open());
}

Tile* ProgressiveEXRImageFileReader::read_tile(
    const size_t        tile_x,
    const size_t        tile_y)
{
    assert(is_open());

    try
    {
        const int ix = static_cast<int>(tile_x);
        const int iy = static_cast<int>(tile_y);
        const int tw = static_cast<int>(impl->m_props.m_tile_width);
        const int th = static_cast<int>(impl->m_props.m_tile_height);

        // Retrieve the data window and true dimensions of the tile.
        Box2i dw;
        size_t tile_width, tile_height;
        if (impl->m_is_tiled)
        {
            dw = impl->m_tiled_file->dataWindowForTile(ix, iy);

            tile_width = dw.max.x - dw.min.x + 1;
            tile_height = dw.max.y - dw.min.y + 1;
        }
        else
        {
            dw.min.x = impl->m_dw.min.x;
            dw.min.y = impl->m_dw.min.y + iy * th;
            dw.max.x = impl->m_dw.max.x;
            dw.max.y = min(dw.min.y + th - 1, impl->m_dw.max.y);

            const int min_x = impl->m_dw.min.x + ix * tw;
            tile_width = min(min_x + tw - 1, impl->m_dw.max.x) - min_x + 1;
            tile_height = dw.max.y - dw.min.y + 1;
        }

        // Create a new tile.
        Tile* tile =
            new Tile(
                tile_width,
                tile_height,
                impl->m_props.m_channel_count,
                impl->m_props.m_pixel_format);

        const size_t channel_size = Pixel::size(impl->m_props.m_pixel_format);
        const size_t stride_x = impl->m_props.m_pixel_size;
        const size_t stride_y = impl->m_is_tiled
            ? stride_x * tile_width
            : stride_x * impl->m_props.m_canvas_width;
        const int origin = static_cast<int>(dw.min.x * stride_x + dw.min.y * stride_y);
        char* base = impl->m_is_tiled
            ? reinterpret_cast<char*>(tile->pixel(0, 0)) - origin
            : reinterpret_cast<char*>(&impl->m_scanlines[0]) - origin;

        // Construct the FrameBuffer object.
        FrameBuffer framebuffer;
        framebuffer.insert(
            "R",
            Slice(
                impl->m_red->type,
                base + 0 * channel_size,
                stride_x,
                stride_y));
        framebuffer.insert(
            "G",
            Slice(
                impl->m_green->type,
                base + 1 * channel_size,
                stride_x,
                stride_y));
        framebuffer.insert(
            "B",
            Slice(
                impl->m_blue->type,
                base + 2 * channel_size,
                stride_x,
                stride_y));
        if (impl->m_alpha)
        {
            framebuffer.insert(
                "A",
                Slice(
                    impl->m_alpha->type,
                    base + 3 * channel_size,
                    stride_x,
                    stride_y));
        }

        if (impl->m_is_tiled)
        {
            // Read the tile.
            impl->m_tiled_file->setFrameBuffer(framebuffer);
            impl->m_tiled_file->readTile(ix, iy);
        }
        else
        {
            if (impl->m_last_tile_y != tile_y)
            {
                // Read the scanlines intersecting the tile.
                impl->m_scanline_file->setFrameBuffer(framebuffer);
                impl->m_scanline_file->readPixels(dw.min.y, dw.max.y);
                impl->m_last_tile_y = tile_y;
            }

            // Extract the tile from the scanlines.
            for (size_t y = 0; y < tile_height; ++y)
            {
                const size_t index = (impl->m_props.m_canvas_width * y + ix * tw) * stride_x;
                memcpy(
                    tile->pixel(0, y),
                    &impl->m_scanlines[index],
                    tile_width * stride_x);
            }
        }

        return tile;
    }
    catch (const BaseExc& e)
    {
        // I/O error.
        throw ExceptionIOError(e.what());
    }
}

}   // namespace foundation
