
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
#include "genericprogressiveimagefilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/exceptionunsupportedimageformat.h"
#include "foundation/image/imageattributes.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/imageio.h"
#include "OpenImageIO/paramlist.h"
#include "OpenImageIO/typedesc.h"
#include "OpenImageIO/version.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstring>
#include <memory>
#include <string>

namespace foundation
{

//
// GenericProgressiveImageFileReader class implementation.
//

struct GenericProgressiveImageFileReader::Impl
{
    Logger*                                 m_logger;
    std::string                             m_filename;

#if OIIO_VERSION < 20000
    OIIO::ImageInput*                       m_input;
#else
    std::unique_ptr<OIIO::ImageInput>       m_input;
#endif

    bool                                    m_supports_random_access;
    bool                                    m_is_tiled;
    CanvasProperties                        m_props;

    void open()
    {
        m_input = OIIO::ImageInput::open(m_filename);

        if (m_input == nullptr)
            throw ExceptionIOError(OIIO::geterror().c_str());

        m_supports_random_access = m_input->supports("random_access") != 0;
        read_spec(m_input->spec());
    }

    void read_spec(const OIIO::ImageSpec& spec)
    {
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

#if OIIO_VERSION < 20000
    OIIO::ImageInput::destroy(impl->m_input);
#endif

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
    assert(attrs.empty());

    const OIIO::ImageSpec& spec = impl->m_input->spec();

    for (size_t i = 0; i < spec.extra_attribs.size(); ++i)
    {
        const OIIO::ParamValue& p(spec.extra_attribs[i]);

        switch (p.type().basetype)
        {
          case OIIO::TypeDesc::STRING:
            attrs.insert(p.name().c_str(), *static_cast<char* const *>(p.data()));
            break;

          case OIIO::TypeDesc::FLOAT:
            attrs.insert(p.name().c_str(), *static_cast<const float *>(p.data()));
            break;

          case OIIO::TypeDesc::DOUBLE:
            attrs.insert(p.name().c_str(), *static_cast<const double *>(p.data()));
            break;

          case OIIO::TypeDesc::INT:
            attrs.insert(p.name().c_str(), *static_cast<const int *>(p.data()));
            break;

          case OIIO::TypeDesc::UINT:
            attrs.insert(p.name().c_str(), *static_cast<const unsigned int *>(p.data()));
            break;

          assert_otherwise;
        }
        // todo: handle more types if required.
    }
}

bool GenericProgressiveImageFileReader::choose_subimage(const size_t subimage) const
{
    OIIO::ImageSpec spec;
    const bool success = impl->m_input->seek_subimage(
        static_cast<int>(subimage),
        0,
        spec);

    if (success)
        impl->read_spec(spec);

    return success;
}

Tile* GenericProgressiveImageFileReader::read_tile(
    const size_t        tile_x,
    const size_t        tile_y)
{
    assert(is_open());

    // Compute tile's dimensions.
    const size_t tile_width = impl->m_is_tiled
        ? std::min(impl->m_props.m_tile_width, impl->m_props.m_canvas_width - (tile_x * impl->m_props.m_tile_width))
        : impl->m_props.m_canvas_width;
    const size_t tile_height = impl->m_is_tiled
        ? std::min(impl->m_props.m_tile_height, impl->m_props.m_canvas_height - (tile_y * impl->m_props.m_tile_height))
        : impl->m_props.m_canvas_height;

    // Create the tile.
    Tile* output_tile = new Tile(
        tile_width,
        tile_height,
        impl->m_props.m_channel_count,
        impl->m_props.m_pixel_format);

    // Read the tile from the file.
    read_tile(tile_x, tile_y, output_tile);

    return output_tile;
}

void GenericProgressiveImageFileReader::read_tile(
    const size_t        tile_x,
    const size_t        tile_y,
    Tile*               output_tile)
{
    assert(is_open());
    assert(output_tile);
    assert(output_tile->get_channel_count() == impl->m_props.m_channel_count);
    assert(output_tile->get_pixel_format() == impl->m_props.m_pixel_format);

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

        const size_t origin_x = tile_x * impl->m_props.m_tile_width;
        const size_t origin_y = tile_y * impl->m_props.m_tile_height;
        const size_t tile_width = std::min(impl->m_props.m_tile_width, impl->m_props.m_canvas_width - origin_x);
        const size_t tile_height = std::min(impl->m_props.m_tile_height, impl->m_props.m_canvas_height - origin_y);

        if (tile_width == impl->m_props.m_tile_width && tile_height == impl->m_props.m_tile_height)
        {
            // The tile fits perfectly into the canvas.
            assert(output_tile->get_width() == impl->m_props.m_tile_width);
            assert(output_tile->get_height() == impl->m_props.m_tile_height);

            if (!impl->m_input->read_tile(
                    static_cast<int>(origin_x),
                    static_cast<int>(origin_y),
                    0, // z
                    impl->m_input->spec().format,
                    output_tile->get_storage()))
                throw ExceptionIOError(impl->m_input->geterror().c_str());
        }
        else
        {
            assert(output_tile->get_width() == tile_width);
            assert(output_tile->get_height() == tile_height);

            std::unique_ptr<Tile> source_tile(
                new Tile(
                    impl->m_props.m_tile_width,
                    impl->m_props.m_tile_height,
                    impl->m_props.m_channel_count,
                    impl->m_props.m_pixel_format));

            if (!impl->m_input->read_tile(
                    static_cast<int>(origin_x),
                    static_cast<int>(origin_y),
                    0, // z
                    impl->m_input->spec().format,
                    source_tile->get_storage()))
                throw ExceptionIOError(impl->m_input->geterror().c_str());

            for (size_t y = 0; y < tile_height; ++y)
            {
                memcpy(
                    output_tile->pixel(0, y),
                    source_tile->pixel(0, y),
                    tile_width * impl->m_props.m_pixel_size);
            }
        }
    }
    else
    {
        //
        // Scanline image.
        //

        assert(output_tile->get_width() == impl->m_props.m_canvas_width);
        assert(output_tile->get_height() == impl->m_props.m_canvas_height);

        if (!impl->m_supports_random_access)
        {
            close();
            impl->open();
        }

        if (!impl->m_input->read_image(
                impl->m_input->spec().format,
                output_tile->get_storage()))
            throw ExceptionIOError(impl->m_input->geterror().c_str());
    }
}

}   // namespace foundation
