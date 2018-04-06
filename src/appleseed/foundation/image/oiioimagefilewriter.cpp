
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
#include "oiioimagefilewriter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/icanvas.h"

// Standard headers.
#include <memory>
#include <stdexcept>

namespace foundation
{

struct OIIOImageFileWriter::OIIOImages
{
    std::vector<const ICanvas*>     m_canvas;
    std::vector<OIIO::ImageSpec>    m_spec;
};

OIIOImageFileWriter::OIIOImageFileWriter() :
    m_images{ new OIIOImages{} }
{

}

OIIOImageFileWriter::~OIIOImageFileWriter()
{
    delete m_images;
}

void OIIOImageFileWriter::create(const char* filename)
{
    assert(filename);

    m_filename = filename;

    m_writer = OIIO::ImageOutput::create(m_filename);
    if (m_writer == nullptr)
    {
        const std::string msg = OpenImageIO::geterror();
        throw ExceptionIOError(msg.c_str());
    }
}

void OIIOImageFileWriter::append_image(const ICanvas* image)
{
    m_images->m_canvas.push_back(image);
    m_images->m_spec.push_back(OIIO::ImageSpec{});
}

size_t OIIOImageFileWriter::get_image_count() const
{
    return m_images->m_canvas.size();
}

OIIO::TypeDesc convert_pixel_format(PixelFormat format)
{
    switch (format)
    {
    case PixelFormatUInt8:
        return OIIO::TypeDesc::UINT8;

    case PixelFormatUInt16:
        return OIIO::TypeDesc::UINT16;

    case PixelFormatUInt32:
        return OIIO::TypeDesc::UINT32;

    case PixelFormatHalf:
        return OIIO::TypeDesc::HALF;

    case PixelFormatFloat:
        return OIIO::TypeDesc::FLOAT;

    case PixelFormatDouble:
        return OIIO::TypeDesc::DOUBLE;

    default:
        return OIIO::TypeDesc::UNKNOWN;
    }
}

bool OIIOImageFileWriter::check_tile_validity(const CanvasProperties& props)
{
    return (props.m_canvas_width % props.m_tile_width) == 0 &&
           (props.m_canvas_height % props.m_tile_height) == 0 &&
           m_writer->supports("tiles") != 0;
}

void OIIOImageFileWriter::set_image_spec(
    const CanvasProperties& props,
    const size_t		    channel_count,
    const char**		    channel_names,
    const PixelFormat       output_pixel_format)
{
    assert(channel_names);

    if (get_image_count() == 0)
    {
        destroy();
        throw ExceptionIOError("No images available!");
    }

    OIIO::ImageSpec& spec = m_images->m_spec.back();

    // Size of the data of the image.
    spec.width = static_cast<int>(props.m_canvas_width);
    spec.height = static_cast<int>(props.m_canvas_height);

    // Origin of the pixel data of the image.
    spec.x = 0;
    spec.y = 0;

    // Full size of the data of the image.
    spec.full_width = spec.width;
    spec.full_height = spec.height;

    // Origin of the pixel data of the full image.
    spec.full_x = spec.x;
    spec.full_y = spec.y;

    // Size of a tile.
    if (check_tile_validity(props))
    {
        spec.tile_width = static_cast<int>(props.m_tile_width);
        spec.tile_height = static_cast<int>(props.m_tile_height);
    }
    else
    {
        spec.tile_width = 0;
        spec.tile_height = 0;
    }

    // Number of channels.
    spec.nchannels = static_cast<int>(channel_count);
    for (size_t i = 0; i < channel_count; i++)
    {
        const char* name = channel_names[i];

        spec.channelnames.push_back(name);

        if (strcmp(name, "A"))
            spec.alpha_channel = static_cast<int>(i);
    }

    // Format of the pixel data.
    spec.set_format(convert_pixel_format(output_pixel_format));
}

void OIIOImageFileWriter::set_image_spec(
    const CanvasProperties& props,
    const PixelFormat       output_pixel_format)
{
    std::vector<const char*> channel_names;

    channel_names.push_back("R");
    channel_names.push_back("G");
    channel_names.push_back("B");

    if (props.m_channel_count == 4)
        channel_names.push_back("A");

    set_image_spec(props, props.m_channel_count, channel_names.data(), output_pixel_format);
}

void OIIOImageFileWriter::set_image_attribute(
    const char* key, 
    const char* value)
{
    assert(key);
    assert(value);

    if (get_image_count() == 0)
    {
        destroy();
        throw ExceptionIOError("No images available!");
    }

    m_images->m_spec.back().attribute(key, value);
}

void OIIOImageFileWriter::write_tiles(const ICanvas* image)
{
    assert(image);

    // Retrieves canvas properties
    const CanvasProperties& props = image->properties();

    // Loops over the columns of tiles
    for (size_t tile_y = 0; tile_y < props.m_tile_count_y; tile_y++)
    {
        // Loops over the rows of tiles
        for (size_t tile_x = 0; tile_x < props.m_tile_count_x; tile_x++)
        {
            // Computes the offset of the tile in pixels from the origin (origin: x=0;y=0).
            const size_t tile_offset_x = tile_x * props.m_tile_width;
            const size_t tile_offset_y = tile_y * props.m_tile_height;

            assert(tile_offset_x <= props.m_canvas_width);
            assert(tile_offset_y <= props.m_canvas_height);

            // Retrieves the (tile_x, tile_y) tile.
            const Tile& tile = image->tile(tile_x, tile_y);

            // Writes the tile.
            if (!m_writer->write_tile(
                    static_cast<int>(tile_offset_x),
                    static_cast<int>(tile_offset_y),
                    0,
                    convert_pixel_format(props.m_pixel_format),
                    tile.get_storage()))
            {
                const std::string msg = m_writer->geterror();
                close_file();
                destroy();
                throw ExceptionIOError(msg.c_str());
            }
        }
    }
}

void OIIOImageFileWriter::write_scanlines(const ICanvas* image)
{
    assert(image);

    // Retrieves canvas properties
    const CanvasProperties& props = image->properties();

    // Constructs the temporary buffer holding one row of tiles in target format.
    std::unique_ptr<uint8> buffer(new uint8[props.m_canvas_width * props.m_tile_height * props.m_pixel_size]);
    uint8* __restrict buffer_ptr = buffer.get();

    // Loops over the rows of tiles
    for (size_t tile_y = 0; tile_y < props.m_tile_count_y; tile_y++)
    {
        // Loops over the columns of tiles
        for (size_t tile_x = 0; tile_x < props.m_tile_count_x; tile_x++)
        {
            // Retrieves the (tile_x, tile_y) tile.
            const Tile& tile = image->tile(tile_x, tile_y);

            // Checks if tile height is equal than props tile height.
            assert(tile.get_height() <= props.m_tile_height);

            // Loops over the row pixels of the current tile
            for (size_t y = 0; y < tile.get_height(); y++)
            {
                // Checks if tile width is equal than props tile width.
                assert(tile.get_width() <= props.m_tile_width);

                // Loops over the column pixels of the current tile
                for (size_t x = 0; x < tile.get_width(); x++)
                {
                    // Horizontal coordinate of the pixel in the temporary buffer.
                    const size_t buffer_x = tile_x * props.m_tile_width + x;

                    // Index of the pixel in the temporary buffer.
                    const size_t buffer_index = (y * props.m_canvas_width + buffer_x) * props.m_pixel_size;

                    // Checks if tile channel count is equal than props channel count.
                    assert(tile.get_channel_count() == props.m_channel_count);
                    // Checks if pixel format is the same.
                    assert(tile.get_pixel_format() == props.m_pixel_format);

                    const uint8* __restrict pixel = tile.pixel(x, y);
                    memcpy(&(buffer_ptr[buffer_index]), pixel, props.m_pixel_size);
                }
            }
        }

        // Computes 
        const size_t y_begin = tile_y;
        const size_t y_end = y_begin + props.m_tile_height;

        if (!m_writer->write_scanlines(
                static_cast<int>(y_begin), 
                static_cast<int>(y_end), 
                0, 
                convert_pixel_format(props.m_pixel_format),
                buffer_ptr))
        {
            const std::string msg = m_writer->geterror();
            close_file();
            destroy();
            throw ExceptionIOError(msg.c_str());
        }
    }
}

void OIIOImageFileWriter::write(const ICanvas* image)
{
    assert(image);

    if (check_tile_validity(image->properties()))
        write_tiles(image);
    else
        write_scanlines(image);
}

void OIIOImageFileWriter::write_single_image()
{
    if (!m_writer->open(m_filename, m_images->m_spec.back()))
    {
        const std::string msg = m_writer->geterror();
        destroy();
        throw ExceptionIOError(msg.c_str());
    }

    write(m_images->m_canvas.back());

    close_file();
}

void OIIOImageFileWriter::write_multi_images()
{
    if (!m_writer->supports("multiimage"))
    {
        destroy();
        throw ExceptionIOError("File format is unable to write multiple image!");
    }

    if (!m_writer->open(m_filename, static_cast<int>(get_image_count()), m_images.m_spec.data()))
    {
        const std::string msg = m_writer->geterror();
        destroy();
        throw ExceptionIOError(msg.c_str());
    }
    
    for (size_t i = 0; i < get_image_count(); i++)
    {
        if (i > 0)
        {
            if (!m_writer->open(m_filename, m_images->m_spec[i], OIIO::ImageOutput::AppendSubimage))
            {
                const std::string msg = m_writer->geterror();
                close_file();
                destroy();
                throw ExceptionIOError(msg.c_str());
            }
        }
    
        write(m_images->m_canvas[i]);
    
    }
    
    close_file();
}

void OIIOImageFileWriter::write()
{
    if (get_image_count() == 0)
        return;

    else if (get_image_count() == 1)
        write_single_image();

    else
        write_multi_images();
}

void OIIOImageFileWriter::close_file()
{
    // Closes the image file.
    if (!m_writer->close())
    {
        const std::string msg = m_writer->geterror();
        destroy();
        throw ExceptionIOError(msg.c_str());
    }
}

void OIIOImageFileWriter::destroy()
{
    // Destroy the ImageOutput stucture.
    OIIO::ImageOutput::destroy(m_writer);

    m_writer = nullptr;
    m_filename = nullptr;
    m_images->m_canvas.clear();
    m_images->m_spec.clear();
}

}   // namespace foundation
