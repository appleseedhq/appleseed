
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
#include "progressiveexrimagefilewriter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/exceptionunsupportedimageformat.h"
#include "foundation/image/exrutils.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"

// OpenEXR headers.
#include "foundation/platform/exrheaderguards.h"
BEGIN_EXR_INCLUDES
#include "OpenEXR/ImathBox.h"
#include "OpenEXR/ImathVec.h"
#include "OpenEXR/IexBaseExc.h"
#include "OpenEXR/ImfChannelList.h"
#include "OpenEXR/ImfFrameBuffer.h"
#include "OpenEXR/ImfHeader.h"
#include "OpenEXR/ImfLineOrder.h"
#include "OpenEXR/ImfPixelType.h"
#include "OpenEXR/ImfTileDescription.h"
#include "OpenEXR/ImfTiledOutputFile.h"
END_EXR_INCLUDES

// Standard headers.
#include <cassert>
#include <memory>

using namespace Iex;
using namespace Imath;
using namespace Imf;
using namespace std;

namespace foundation
{

//
// ProgressiveEXRImageFileWriter class implementation.
//

namespace
{
    const char* ChannelName[] = { "R", "G", "B", "A" };
}

struct ProgressiveEXRImageFileWriter::Impl
{
    Logger*                         m_logger;
    int                             m_thread_count;
    auto_ptr<Imf::TiledOutputFile>  m_file;
    CanvasProperties                m_props;
    PixelType                       m_pixel_type;
};

ProgressiveEXRImageFileWriter::ProgressiveEXRImageFileWriter(Logger* logger)
  : impl(new Impl())
{
    impl->m_logger = logger;
    impl->m_thread_count = 1;
}

ProgressiveEXRImageFileWriter::ProgressiveEXRImageFileWriter(const size_t thread_count)
  : impl(new Impl())
{
    impl->m_logger = 0;
    impl->m_thread_count = static_cast<int>(thread_count);
}

ProgressiveEXRImageFileWriter::ProgressiveEXRImageFileWriter(
    Logger*                 logger,
    const size_t            thread_count)
  : impl(new Impl())
{
    impl->m_logger = logger;
    impl->m_thread_count = static_cast<int>(thread_count);
}

ProgressiveEXRImageFileWriter::~ProgressiveEXRImageFileWriter()
{
    if (is_open())
        close();

    delete impl;
}

void ProgressiveEXRImageFileWriter::open(
    const char*             filename,
    const CanvasProperties& props,
    const ImageAttributes&  attrs)
{
    assert(filename);
    assert(!is_open());

    initialize_openexr();

    try
    {
        // todo: lift this limitation.
        assert(props.m_channel_count <= 4);

        // Figure out the pixel type, based on the pixel format of the image.
        impl->m_pixel_type = FLOAT;
        switch (props.m_pixel_format)
        {
          case PixelFormatUInt32: impl->m_pixel_type = UINT; break;
          case PixelFormatHalf: impl->m_pixel_type = HALF; break;
          case PixelFormatFloat: impl->m_pixel_type = FLOAT; break;
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
            channels.insert(ChannelName[c], Channel(impl->m_pixel_type));

        // Construct Header object.
        Header header(
            static_cast<int>(props.m_canvas_width),
            static_cast<int>(props.m_canvas_height),
            static_cast<float>(props.m_canvas_width) / props.m_canvas_height);
        header.setTileDescription(tile_desc);
        header.lineOrder() = RANDOM_Y;
        header.channels() = channels;

        // Add image attributes to the Header object.
        add_attributes(attrs, header);

        // Create the output file.
        impl->m_file.reset(
            new TiledOutputFile(
                filename,
                header,
                impl->m_thread_count));

        // Store the canvas properties.
        impl->m_props = props;
    }
    catch (const BaseExc& e)
    {
        // I/O error.
        throw ExceptionIOError(e.what());
    }
}

void ProgressiveEXRImageFileWriter::close()
{
    assert(is_open());
    impl->m_file.reset();
}

bool ProgressiveEXRImageFileWriter::is_open() const
{
    return impl->m_file.get() != 0;
}

void ProgressiveEXRImageFileWriter::write_tile(
    const Tile&             tile,
    const size_t            tile_x,
    const size_t            tile_y)
{
    assert(is_open());

    try
    {
        const int ix              = static_cast<int>(tile_x);
        const int iy              = static_cast<int>(tile_y);
        const Box2i range         = impl->m_file->dataWindowForTile(ix, iy);
        const size_t channel_size = Pixel::size(tile.get_pixel_format());
        const size_t stride_x     = channel_size * impl->m_props.m_channel_count;
        const size_t stride_y     = stride_x * tile.get_width();
        const size_t tile_origin  = range.min.x * stride_x + range.min.y * stride_y;
        const char* tile_base     = reinterpret_cast<const char*>(tile.pixel(0, 0)) - tile_origin;

        // Construct FrameBuffer object.
        FrameBuffer framebuffer;
        for (size_t c = 0; c < impl->m_props.m_channel_count; ++c)
        {
            const char* base = tile_base + c * channel_size;
            framebuffer.insert(
                ChannelName[c],
                Slice(
                    impl->m_pixel_type,
                    const_cast<char*>(base),
                    stride_x,
                    stride_y));
        }

        // Write tile.
        impl->m_file->setFrameBuffer(framebuffer);
        impl->m_file->writeTile(ix, iy);
    }
    catch (const BaseExc& e)
    {
        // I/O error.
        throw ExceptionIOError(e.what());
    }
}

}   // namespace foundation
