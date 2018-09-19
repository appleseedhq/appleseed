
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
// Copyright (c) 2018 Thomas Manceau, The appleseedhq Organization
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
#include "genericimagefilewriter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/icanvas.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <algorithm>
#include <cstring>
#include <memory>
#include <stdexcept>
#include <vector>

namespace foundation
{

struct GenericImageFileWriter::Impl
{
    std::vector<const ICanvas*>     m_canvas;
    std::vector<OIIO::ImageSpec>    m_spec;
};

GenericImageFileWriter::GenericImageFileWriter(const char* filename) :
    IImageFileWriter(),
    impl(new Impl())
{
    assert(filename);

    m_filename = filename;

    m_writer = OIIO::ImageOutput::create(m_filename);
    if (m_writer == nullptr)
    {
        const std::string msg = OIIO::geterror();
        throw ExceptionIOError(msg.c_str());
    }
}

GenericImageFileWriter::~GenericImageFileWriter()
{
    // Destroy the ImageOutput stucture.
    if (m_writer != nullptr)
        OIIO::ImageOutput::destroy(m_writer);

    delete impl;
}

size_t GenericImageFileWriter::get_image_count() const
{
    return impl->m_canvas.size();
}

void GenericImageFileWriter::append_image(const ICanvas* image)
{
    assert(image);

    impl->m_canvas.push_back(image);
    impl->m_spec.push_back(OIIO::ImageSpec());

    set_image_spec();
}

namespace
{
    OIIO::TypeDesc convert_pixel_format(const PixelFormat format)
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
}

void GenericImageFileWriter::set_image_output_format(const PixelFormat output_pixel_format)
{
    assert(!impl->m_spec.empty());

    impl->m_spec.back().set_format(convert_pixel_format(output_pixel_format));
}

void GenericImageFileWriter::set_image_channels(
    const size_t    channel_count,
    const char**    channel_names)
{
    assert(!impl->m_spec.empty());
    assert(channel_count > 0);
    assert(channel_names);

    OIIO::ImageSpec& spec = impl->m_spec.back();

    spec.nchannels = static_cast<int>(channel_count);

    for (size_t i = 0; i < channel_count; i++)
    {
        const char* name = channel_names[i];

        spec.channelnames.push_back(name);

        if (strcmp(name, "A") == 0)
            spec.alpha_channel = static_cast<int>(i);
    }
}

void GenericImageFileWriter::set_image_spec()
{
    assert(!impl->m_spec.empty());

    const CanvasProperties& props = impl->m_canvas.back()->properties();
    OIIO::ImageSpec& spec = impl->m_spec.back();

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
    if (m_writer->supports("tiles"))
    {
        spec.tile_width = static_cast<int>(props.m_tile_width);
        spec.tile_height = static_cast<int>(props.m_tile_height);
    }
    else
    {
        spec.tile_width = 0;
        spec.tile_height = 0;
    }

    // Channel names.
    const char* channel_names[] = { "R", "G", "B", "A" };
    set_image_channels(props.m_channel_count, channel_names);

    // Format of the pixel data.
    const boost::filesystem::path filepath(m_filename);
    const std::string extension = lower_case(filepath.extension().string());

    set_image_output_format(props.m_pixel_format);
}

void GenericImageFileWriter::set_exr_image_attributes(const ImageAttributes& image_attributes)
{
    OIIO::ImageSpec& spec = impl->m_spec.back();

    if (image_attributes.exist("dwa_compression_lvl"))
        spec.attribute("openexr:dwaCompressionLevel", image_attributes.get<float>("dwa_compression_lvl"));

    if (image_attributes.exist("white_xy_chromaticity") &&
        image_attributes.exist("red_xy_chromaticity") &&
        image_attributes.exist("green_xy_chromaticity") &&
        image_attributes.exist("blue_xy_chromaticity"))
    {
        float chromaticities[8];
        memset(chromaticities, 0, 8 * sizeof(float));

        const Vector2f red = image_attributes.get<Vector2f>("red_xy_chromaticity");
        const Vector2f green = image_attributes.get<Vector2f>("green_xy_chromaticity");
        const Vector2f blue = image_attributes.get<Vector2f>("blue_xy_chromaticity");
        const Vector2f white = image_attributes.get<Vector2f>("white_xy_chromaticity");

        chromaticities[0] = red[0]; chromaticities[1] = red[1];
        chromaticities[2] = green[0]; chromaticities[3] = green[1];
        chromaticities[4] = blue[0]; chromaticities[5] = blue[1];
        chromaticities[6] = white[0]; chromaticities[7] = white[1];

        OIIO::TypeDesc type;
        type.basetype = OIIO::TypeDesc::BASETYPE::FLOAT;
        type.aggregate = OIIO::TypeDesc::AGGREGATE::SCALAR;
        type.arraylen = 8;

        spec.attribute("chromaticities", type, chromaticities);
    }
}

void GenericImageFileWriter::set_generic_image_attributes(const ImageAttributes& image_attributes)
{
    OIIO::ImageSpec& spec = impl->m_spec.back();

    for (const_each<ImageAttributes> i = image_attributes; i; ++i)
    {
        // Fetch the name and the value of the attribute.
        const std::string attr_name = i->key();
        const std::string attr_value = i->value<std::string>();

        if (attr_name == "author")
            spec.attribute("Artist", attr_value.c_str());

        else if (attr_name == "copyright")
            spec.attribute("Copyright", attr_value.c_str());

        else if (attr_name == "title")
            spec.attribute("DocumentName", attr_value.c_str());

        else if (attr_name == "description")
            spec.attribute("ImageDescription", attr_value.c_str());

        else if (attr_name == "date")
            spec.attribute("DateTime", attr_value.c_str());

        else if (attr_name == "software")
            spec.attribute("Software", attr_value.c_str());

        else if (attr_name == "computer")
            spec.attribute("HostComputer", attr_value.c_str());

        else if (attr_name == "image_name")
            spec.attribute("oiio:subimagename", attr_value.c_str());

        else if (attr_name == "color_space")
        {
            if (attr_value == "linear")
                spec.attribute("oiio:ColorSpace", "Linear");
            else
                spec.attribute("oiio:ColorSpace", attr_value.c_str());
        }

        else if (attr_name == "compression")
            spec.attribute("compression", attr_value.c_str());

        else if (attr_name == "compression_quality")
            spec.attribute("CompressionQuality", from_string<int>(attr_value));

        else if (attr_name == "dpi")
        {
            const size_t dpi = from_string<size_t>(attr_value);
            const float dpm = dpi * (100.0f / 2.54f);
            spec.attribute("XResolution", dpm);
            spec.attribute("YResolution", dpm);
            spec.attribute("ResolutionUnit", "cm");
        }

        else
            spec.attribute(attr_name, attr_value.c_str());
    }
}

void GenericImageFileWriter::set_image_attributes(const ImageAttributes& image_attributes)
{
    assert(!impl->m_spec.empty());

    // Retrieve filename extension.
    const boost::filesystem::path filepath(m_filename);
    const std::string extension = lower_case(filepath.extension().string());

    // General image attributes.
    set_generic_image_attributes(image_attributes);

    // Set image attributes depending of its extension.
    if (extension == ".exr")
        set_exr_image_attributes(image_attributes);
}

void GenericImageFileWriter::write_tiles(const size_t image_index)
{
    // Retrieve canvas.
    assert(image_index < impl->m_canvas.size());
    const ICanvas* canvas = impl->m_canvas[image_index];
    assert(canvas);

    // Retrieve canvas properties.
    const CanvasProperties& props = canvas->properties();

    // Retrieve image spec.
    assert(image_index < impl->m_spec.size());
    const OIIO::ImageSpec& spec = impl->m_spec[image_index];

    // Compute the tiles' xstride offset in bytes.
    size_t xstride = props.m_pixel_size;

    // Loop over the columns of tiles.
    for (size_t tile_y = 0; tile_y < props.m_tile_count_y; tile_y++)
    {
        // Loop over the rows of tiles.
        for (size_t tile_x = 0; tile_x < props.m_tile_count_x; tile_x++)
        {
            // Compute the offset of the tile in pixels from the origin (origin: x=0;y=0).
            const size_t tile_offset_x = tile_x * props.m_tile_width;
            assert(tile_offset_x <= props.m_canvas_width);
            const size_t tile_offset_y = tile_y * props.m_tile_height;
            assert(tile_offset_y <= props.m_canvas_height);

            // Compute the tile's ystride offset in bytes.
            const size_t ystride = xstride * std::min(static_cast<size_t>(spec.width + spec.x - tile_offset_x), 
                                                      static_cast<size_t>(spec.tile_width));

            // Retrieve the (tile_x, tile_y) tile.
            const Tile& tile = canvas->tile(tile_x, tile_y);

            // Write the tile into the file.
            if (!m_writer->write_tile(
                    static_cast<int>(tile_offset_x),
                    static_cast<int>(tile_offset_y),
                    0,
                    convert_pixel_format(props.m_pixel_format),
                    tile.get_storage(),
                    xstride,
                    ystride))
            {
                const std::string msg = m_writer->geterror();
                close_file();
                throw ExceptionIOError(msg.c_str());
            }
        }
    }
}

void GenericImageFileWriter::write_scanlines(const size_t image_index)
{
    // Retrieve canvas.
    assert(image_index < impl->m_canvas.size());
    const ICanvas* canvas = impl->m_canvas[image_index];
    assert(canvas);

    // Retrieve canvas properties.
    const CanvasProperties& props = canvas->properties();

    // Construct the temporary buffer holding one row of tiles in target format.
    std::unique_ptr<uint8> buffer(new uint8[props.m_canvas_width * props.m_tile_height * props.m_pixel_size]);
    uint8* APPLESEED_RESTRICT buffer_ptr = buffer.get();

    // Loop over the rows of tiles.
    for (size_t tile_y = 0; tile_y < props.m_tile_count_y; tile_y++)
    {
        // Loop over the columns of tiles.
        for (size_t tile_x = 0; tile_x < props.m_tile_count_x; tile_x++)
        {
            // Retrieve the (tile_x, tile_y) tile.
            const Tile& tile = canvas->tile(tile_x, tile_y);

            // Loop over the row pixels of the current tile.
            for (size_t y = 0; y < tile.get_height(); y++)
            {
                // Loop over the column pixels of the current tile.
                for (size_t x = 0; x < tile.get_width(); x++)
                {
                    // Horizontal coordinate of the pixel in the temporary buffer.
                    const size_t buffer_x = tile_x * props.m_tile_width + x;

                    // Index of the pixel in the temporary buffer.
                    const size_t buffer_index = (y * props.m_canvas_width + buffer_x) * props.m_pixel_size;

                    // Retrieve the (x, y) pixel.
                    const uint8* APPLESEED_RESTRICT pixel = tile.pixel(x, y);

                    // Write the pixel into the buffer.
                    memcpy(&(buffer_ptr[buffer_index]), pixel, props.m_pixel_size);
                }
            }
        }

        // Compute y dimensional scanline border.
        const size_t y_begin = tile_y;
        const size_t y_end = y_begin + props.m_tile_height;

        // Write scanline into the file.
        if (!m_writer->write_scanlines(
                static_cast<int>(y_begin), 
                static_cast<int>(y_end), 
                0, 
                convert_pixel_format(props.m_pixel_format),
                buffer_ptr))
        {
            const std::string msg = m_writer->geterror();
            close_file();
            throw ExceptionIOError(msg.c_str());
        }
    }
}

void GenericImageFileWriter::write(const size_t image_index)
{
    assert(image_index < impl->m_canvas.size());

    if (m_writer->supports("tiles"))
        write_tiles(image_index);
    else
        write_scanlines(image_index);
}

void GenericImageFileWriter::write_single_image()
{
    if (!m_writer->open(m_filename, impl->m_spec.back()))
    {
        const std::string msg = m_writer->geterror();
        throw ExceptionIOError(msg.c_str());
    }

    assert(impl->m_canvas.size() > 0);

    write(impl->m_canvas.size() - 1);

    close_file();
}

void GenericImageFileWriter::write_multi_images()
{
    if (!m_writer->supports("multiimage"))
        throw ExceptionIOError("File format is unable to write multiple images");

    if (!m_writer->open(m_filename, static_cast<int>(get_image_count()), impl->m_spec.data()))
    {
        const std::string msg = m_writer->geterror();
        throw ExceptionIOError(msg.c_str());
    }
    
    for (size_t i = 0, e = get_image_count(); i < e; ++i)
    {
        if (i > 0)
        {
            if (!m_writer->open(m_filename, impl->m_spec[i], OIIO::ImageOutput::AppendSubimage))
            {
                const std::string msg = m_writer->geterror();
                close_file();
                throw ExceptionIOError(msg.c_str());
            }
        }

        write(i);
    }
    
    close_file();
}

void GenericImageFileWriter::write()
{
    const size_t image_count = get_image_count();

    switch (image_count)
    {
      case 0:
          return;
      case 1:
          write_single_image();
          break;
      default:
          write_multi_images();
          break;
    }
}

void GenericImageFileWriter::close_file()
{
    // Close the image file.
    if (!m_writer->close())
    {
        const std::string msg = m_writer->geterror();
        throw ExceptionIOError(msg.c_str());
    }
}

}   // namespace foundation
