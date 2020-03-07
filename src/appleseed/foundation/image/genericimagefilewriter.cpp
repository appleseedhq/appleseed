
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
#include "foundation/image/imageattributes.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/string/string.h"
#include "foundation/utility/iostreamop.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/version.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <algorithm>
#include <cstdint>
#include <cstring>
#include <memory>
#include <stdexcept>
#include <vector>

namespace foundation
{

namespace
{
    OIIO::TypeDesc convert_pixel_format(const PixelFormat format)
    {
        switch (format)
        {
          case PixelFormatUInt8: return OIIO::TypeDesc::UINT8;
          case PixelFormatUInt16: return OIIO::TypeDesc::UINT16;
          case PixelFormatUInt32: return OIIO::TypeDesc::UINT32;
          case PixelFormatHalf: return OIIO::TypeDesc::HALF;
          case PixelFormatFloat: return OIIO::TypeDesc::FLOAT;
          case PixelFormatDouble: return OIIO::TypeDesc::DOUBLE;
          default: return OIIO::TypeDesc::UNKNOWN;
        }
    }
}

struct GenericImageFileWriter::Impl
{
    const std::string                   m_filename;
#if OIIO_VERSION >= 20000
    std::unique_ptr<OIIO::ImageOutput>  m_writer;
#else
    OIIO::ImageOutput*                  m_writer;
#endif
    std::vector<const ICanvas*>         m_canvas;
    std::vector<OIIO::ImageSpec>        m_spec;

    explicit Impl(const char* filename)
      : m_filename(filename)
    {
        m_writer = OIIO::ImageOutput::create(m_filename);

        if (m_writer == nullptr)
            throw ExceptionIOError(OIIO::geterror().c_str());
    }

    ~Impl()
    {
#if OIIO_VERSION < 20000
        if (m_writer != nullptr)
            OIIO::ImageOutput::destroy(m_writer);
#endif
    }

    void close_file()
    {
        assert(m_writer);

        if (!m_writer->close())
            throw ExceptionIOError(m_writer->geterror().c_str());
    }

    void set_image_spec()
    {
        assert(!m_spec.empty());

        const CanvasProperties& props = m_canvas.back()->properties();
        OIIO::ImageSpec& spec = m_spec.back();

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
        // Needs to be manually set if the number of channels is higher than 4.
        if (props.m_channel_count <= 4)
        {
            const char* channel_names[] = { "R", "G", "B", "A" };
            set_image_channels(props.m_channel_count, channel_names);
        }

        // Format of the pixel data.
        const boost::filesystem::path filepath(m_filename);
        const std::string extension = lower_case(filepath.extension().string());

        set_image_output_format(props.m_pixel_format);
    }

    void set_generic_image_attributes(const ImageAttributes& image_attributes)
    {
        OIIO::ImageSpec& spec = m_spec.back();

        for (auto& i : image_attributes)
        {
            // Fetch the name and the value of the attribute.
            const std::string attr_name = i.key();
            const std::string attr_value = i.value<std::string>();

            if (attr_name == "author")
                spec.attribute("Artist", attr_value);
            else if (attr_name == "copyright")
                spec.attribute("Copyright", attr_value);
            else if (attr_name == "title")
                spec.attribute("DocumentName", attr_value);
            else if (attr_name == "description")
                spec.attribute("ImageDescription", attr_value);
            else if (attr_name == "date")
                spec.attribute("DateTime", attr_value);
            else if (attr_name == "software")
                spec.attribute("Software", attr_value);
            else if (attr_name == "computer")
                spec.attribute("HostComputer", attr_value);
            else if (attr_name == "image_name")
                spec.attribute("oiio:subimagename", attr_value);
            else if (attr_name == "color_space")
                spec.attribute("oiio:ColorSpace", attr_value == "linear" ? "Linear" : attr_value);
            else if (attr_name == "compression")
                spec.attribute("compression", attr_value);
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
            else if (attr_name == "dither")
                spec.attribute("oiio:dither", from_string<int>(attr_value));
            else
            {
                // Write all other attributes as string attributes.
                // A limitation of foundation::ImageAttributes (which is essentially
                // a foundation::StringDictionary) is that type information is lost.
                // This forces us to write attributes as string attributes, which may
                // not be what is expected by OIIO.
                spec.attribute(attr_name, attr_value);
            }
        }
    }

    void set_exr_image_attributes(const ImageAttributes& image_attributes)
    {
        OIIO::ImageSpec& spec = m_spec.back();

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

            chromaticities[0] = red[0];
            chromaticities[1] = red[1];

            chromaticities[2] = green[0];
            chromaticities[3] = green[1];

            chromaticities[4] = blue[0];
            chromaticities[5] = blue[1];

            chromaticities[6] = white[0];
            chromaticities[7] = white[1];

            OIIO::TypeDesc type;
            type.basetype = OIIO::TypeDesc::BASETYPE::FLOAT;
            type.aggregate = OIIO::TypeDesc::AGGREGATE::SCALAR;
            type.arraylen = 8;

            spec.attribute("chromaticities", type, chromaticities);
        }
    }

    void set_image_output_format(const PixelFormat output_pixel_format)
    {
        assert(!m_spec.empty());

        m_spec.back().set_format(convert_pixel_format(output_pixel_format));
    }

    void set_image_channels(
        const size_t    channel_count,
        const char**    channel_names)
    {
        assert(!m_spec.empty());
        assert(channel_count > 0);
        assert(channel_names);

        OIIO::ImageSpec& spec = m_spec.back();

        spec.nchannels = static_cast<int>(channel_count);
        spec.channelnames.clear();

        for (size_t i = 0; i < channel_count; ++i)
        {
            const char* name = channel_names[i];

            spec.channelnames.push_back(name);

            if (strcmp(name, "A") == 0)
                spec.alpha_channel = static_cast<int>(i);
        }
    }

    void write_single_image()
    {
        if (!m_writer->open(m_filename, m_spec.back()))
            throw ExceptionIOError(m_writer->geterror().c_str());

        assert(!m_canvas.empty());
        write(m_canvas.size() - 1);

        close_file();
    }

    void write_multiple_images()
    {
        if (!m_writer->supports("multiimage"))
            throw ExceptionIOError("file format is unable to write multiple images");

        if (!m_writer->open(m_filename, static_cast<int>(m_canvas.size()), m_spec.data()))
            throw ExceptionIOError(m_writer->geterror().c_str());

        for (size_t i = 0, e = m_canvas.size(); i < e; ++i)
        {
            if (i > 0)
            {
                if (!m_writer->open(m_filename, m_spec[i], OIIO::ImageOutput::AppendSubimage))
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

    void write(const size_t image_index)
    {
        m_writer->supports("tiles")
            ? write_tiles(image_index)
            : write_scanlines(image_index);
    }

    void write_scanlines(const size_t image_index)
    {
        // Retrieve canvas.
        assert(image_index < m_canvas.size());
        const ICanvas* canvas = m_canvas[image_index];
        assert(canvas);

        // Retrieve canvas properties.
        const CanvasProperties& props = canvas->properties();

        // Construct the temporary buffer holding one row of tiles in target format.
        std::unique_ptr<std::uint8_t[]> buffer(new std::uint8_t[props.m_canvas_width * props.m_tile_height * props.m_pixel_size]);
        std::uint8_t* APPLESEED_RESTRICT buffer_ptr = buffer.get();

        // Loop over the rows of tiles.
        for (size_t tile_y = 0; tile_y < props.m_tile_count_y; tile_y++)
        {
            // Loop over the columns of tiles.
            size_t tile_height = 0;
            for (size_t tile_x = 0; tile_x < props.m_tile_count_x; tile_x++)
            {
                // Retrieve the (tile_x, tile_y) tile.
                const Tile& tile = canvas->tile(tile_x, tile_y);
                tile_height = tile.get_height();

                // Loop over the row pixels of the current tile.
                for (size_t y = 0; y < tile_height; y++)
                {
                    // Loop over the column pixels of the current tile.
                    for (size_t x = 0; x < tile.get_width(); x++)
                    {
                        // Horizontal coordinate of the pixel in the temporary buffer.
                        const size_t buffer_x = tile_x * props.m_tile_width + x;

                        // Index of the pixel in the temporary buffer.
                        const size_t buffer_index = (y * props.m_canvas_width + buffer_x) * props.m_pixel_size;

                        // Retrieve the (x, y) pixel.
                        const std::uint8_t* APPLESEED_RESTRICT pixel = tile.pixel(x, y);

                        // Write the pixel into the buffer.
                        memcpy(&(buffer_ptr[buffer_index]), pixel, props.m_pixel_size);
                    }
                }
            }

            // Compute y dimensional scanline border.
            const size_t y_begin = tile_y * props.m_tile_height;
            const size_t y_end = y_begin + tile_height;

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

    void write_tiles(const size_t image_index)
    {
        // Retrieve canvas.
        assert(image_index < m_canvas.size());
        const ICanvas* canvas = m_canvas[image_index];
        assert(canvas);

        // Retrieve canvas properties.
        const CanvasProperties& props = canvas->properties();

        // Retrieve image spec.
        assert(image_index < m_spec.size());
        const OIIO::ImageSpec& spec = m_spec[image_index];
        assert(spec.nchannels == spec.channelnames.size());
        assert(spec.nchannels == props.m_channel_count);

        // Compute the tiles' xstride offset in bytes.
        const size_t xstride = props.m_pixel_size;

        // Loop over the columns of tiles.
        for (size_t tile_y = 0; tile_y < props.m_tile_count_y; tile_y++)
        {
            // Loop over the rows of tiles.
            for (size_t tile_x = 0; tile_x < props.m_tile_count_x; tile_x++)
            {
                // Compute the offset of the tile in pixels from the origin (0, 0).
                const size_t tile_offset_x = tile_x * props.m_tile_width;
                const size_t tile_offset_y = tile_y * props.m_tile_height;
                assert(tile_offset_x <= props.m_canvas_width);
                assert(tile_offset_y <= props.m_canvas_height);

                // Compute the tile's ystride offset in bytes.
                const size_t ystride =
                    xstride *
                    std::min(
                        static_cast<size_t>(spec.width + spec.x - tile_offset_x),
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
};

GenericImageFileWriter::GenericImageFileWriter(const char* filename)
  : impl(new Impl(filename))
{
}

GenericImageFileWriter::~GenericImageFileWriter()
{
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
    impl->set_image_spec();
}

void GenericImageFileWriter::set_image_output_format(const PixelFormat output_pixel_format)
{
    impl->set_image_output_format(output_pixel_format);
}

void GenericImageFileWriter::set_image_channels(
    const size_t    channel_count,
    const char**    channel_names)
{
    impl->set_image_channels(channel_count, channel_names);
}

void GenericImageFileWriter::set_image_attributes(const ImageAttributes& image_attributes)
{
    assert(!impl->m_spec.empty());

    // General image attributes.
    impl->set_generic_image_attributes(image_attributes);

    // Retrieve filename extension.
    const boost::filesystem::path filepath(impl->m_filename);
    const std::string extension = lower_case(filepath.extension().string());

    // Set file format-specific image attributes.
    if (extension == ".exr")
        impl->set_exr_image_attributes(image_attributes);
}

void GenericImageFileWriter::write()
{
    switch (get_image_count())
    {
      case 0:
        return;

      case 1:
        impl->write_single_image();
        break;

      default:
        impl->write_multiple_images();
        break;
    }
}

}   // namespace foundation
