
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "genericprogressiveimagefilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/exceptionunsupportedimageformat.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/imageio.h"
#include "OpenImageIO/typedesc.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstring>
#include <memory>
#include <string>

using namespace std;

namespace foundation
{

//
// GenericProgressiveImageFileReader class implementation.
//

struct GenericProgressiveImageFileReader::Impl
{
    Logger*                                 m_logger;
    string                                  m_filename;
    OIIO::ImageInput*                       m_input;
    bool                                    m_supports_random_access;
    bool                                    m_is_tiled;
    CanvasProperties                        m_props;

    void open()
    {
        m_input = OIIO::ImageInput::open(m_filename);

        if (m_input == nullptr)
            throw ExceptionIOError(OIIO::geterror().c_str());

        m_supports_random_access = m_input->supports("random_access") != 0;

        const OIIO::ImageSpec& spec = m_input->spec();

        m_is_tiled = spec.tile_width > 0 && spec.tile_height > 0 && spec.tile_depth > 0;

        size_t tile_width, tile_height;
        if (m_is_tiled)
        {
            // Tiled image.
            tile_width = static_cast<size_t>(spec.tile_width);
            tile_height = static_cast<size_t>(spec.tile_height);
        }
        else
        {
            // Scanline image.
            tile_width = static_cast<size_t>(spec.width);
            tile_height = static_cast<size_t>(spec.height);
        }

        PixelFormat pixel_format;
        switch (spec.format.basetype)
        {
          case OIIO::TypeDesc::UINT8:
          case OIIO::TypeDesc::INT8:
            pixel_format = PixelFormatUInt8;
            break;

          case OIIO::TypeDesc::UINT16:
          case OIIO::TypeDesc::INT16:
            pixel_format = PixelFormatUInt16;
            break;

          case OIIO::TypeDesc::UINT32:
          case OIIO::TypeDesc::INT32:
            pixel_format = PixelFormatUInt32;
            break;

          case OIIO::TypeDesc::HALF:
            pixel_format = PixelFormatHalf;
            break;

          case OIIO::TypeDesc::FLOAT:
            pixel_format = PixelFormatFloat;
            break;

          case OIIO::TypeDesc::DOUBLE:
            pixel_format = PixelFormatDouble;
            break;

          default:
            throw ExceptionUnsupportedImageFormat();
        }

        m_props = CanvasProperties(
            static_cast<size_t>(spec.width),
            static_cast<size_t>(spec.height),
            tile_width,
            tile_height,
            static_cast<size_t>(spec.nchannels),
            pixel_format);
    }
};

GenericProgressiveImageFileReader::GenericProgressiveImageFileReader(Logger* logger)
  : impl(new Impl())
{
    impl->m_logger = logger;
    impl->m_input = nullptr;
    impl->m_supports_random_access = false;
    impl->m_is_tiled = false;
}

GenericProgressiveImageFileReader::~GenericProgressiveImageFileReader()
{
    if (is_open())
        close();

    delete impl;
}

void GenericProgressiveImageFileReader::open(const char* filename)
{
    assert(filename);
    assert(!is_open());

    impl->m_filename = filename;
    impl->open();
}

void GenericProgressiveImageFileReader::close()
{
    assert(is_open());

    impl->m_input->close();

    // todo: we should really be calling OIIO::ImageInput::destroy(impl->m_input)
    // but OpenImageIO 1.5.20 (the version included in appleseed-deps at the time
    // of writing) is too old to have this method. Since on Windows we link to
    // OpenImageIO statically, this should be safe anyway.
    delete impl->m_input;

    impl->m_input = nullptr;
}

bool GenericProgressiveImageFileReader::is_open() const
{
    return impl->m_input != nullptr;
}

void GenericProgressiveImageFileReader::read_canvas_properties(
    CanvasProperties&   props)
{
    assert(is_open());

    props = impl->m_props;
}

void GenericProgressiveImageFileReader::read_image_attributes(
    ImageAttributes&    attrs)
{
    assert(is_open());

    // todo: implement.
}

Tile* GenericProgressiveImageFileReader::read_tile(
    const size_t        tile_x,
    const size_t        tile_y)
{
    assert(is_open());

    if (impl->m_is_tiled)
    {
        //
        // Tiled image.
        //
        // In appleseed, for images whose width or height are not multiples
        // of the tile's width or height, border tiles are actually smaller.
        // In OpenImageIO, border tiles have the same size as other tiles,
        // and the image's pixel data window defines which pixels of those
        // tiles actually belong to the image.
        //
        // Since in appleseed we don't propagate pixel data windows through
        // the whole image pipeline, we make sure here to return tiles of
        // the correct dimensions, at some expenses.
        //

        unique_ptr<Tile> source_tile(
            new Tile(
                impl->m_props.m_tile_width,
                impl->m_props.m_tile_height,
                impl->m_props.m_channel_count,
                impl->m_props.m_pixel_format));

        const size_t origin_x = tile_x * impl->m_props.m_tile_width;
        const size_t origin_y = tile_y * impl->m_props.m_tile_height;

        if (!impl->m_input->read_tile(
                static_cast<int>(origin_x),
                static_cast<int>(origin_y),
                0, // z
                impl->m_input->spec().format,
                source_tile->get_storage()))
            throw ExceptionIOError(impl->m_input->geterror().c_str());

        const size_t tile_width = min(impl->m_props.m_tile_width, impl->m_props.m_canvas_width - origin_x);
        const size_t tile_height = min(impl->m_props.m_tile_height, impl->m_props.m_canvas_height - origin_y);

        if (tile_width == impl->m_props.m_tile_width && tile_height == impl->m_props.m_tile_height)
            return source_tile.release();

        unique_ptr<Tile> shrunk_tile(
            new Tile(
                tile_width,
                tile_height,
                impl->m_props.m_channel_count,
                impl->m_props.m_pixel_format));

        for (size_t y = 0; y < tile_height; ++y)
        {
            memcpy(
                shrunk_tile->pixel(0, y),
                source_tile->pixel(0, y),
                tile_width * impl->m_props.m_pixel_size);
        }

        return shrunk_tile.release();
    }
    else
    {
        //
        // Scanline image.
        //

        unique_ptr<Tile> tile(
            new Tile(
                impl->m_props.m_canvas_width,
                impl->m_props.m_canvas_height,
                impl->m_props.m_channel_count,
                impl->m_props.m_pixel_format));

        if (!impl->m_supports_random_access)
        {
            close();
            impl->open();
        }

        if (!impl->m_input->read_image(
                impl->m_input->spec().format,
                tile->get_storage()))
            throw ExceptionIOError(impl->m_input->geterror().c_str());

        return tile.release();
    }
}

}   // namespace foundation
