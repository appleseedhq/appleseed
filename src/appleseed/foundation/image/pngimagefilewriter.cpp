
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
#include "pngimagefilewriter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/icanvas.h"
#include "foundation/utility/foreach.h"

// openimageio headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/imageio.h"
#include "foundation/platform/_endoiioheaders.h"

// std headers.
#include <cstring>
#include <cstdint>
#include <memory>
#include <string>

namespace foundation
{

static void png_set_image_desc(OIIO::ImageSpec& spec, const CanvasProperties& props);
static void png_set_image_attributes(OIIO::ImageSpec& spec, const ImageAttributes& image_attributes);
static void write_scanline(const ICanvas& image, OIIO::ImageOutput* out);

void PNGImageFileWriter::write(
    const char*             filename,
    const ICanvas&          image,
    const ImageAttributes&  image_attributes)
{
    // Creates an ImageOutput structure, which is the main class to write an image file in OpenImageIO.
    OIIO::ImageOutput* out = OIIO::ImageOutput::create(filename);
    if (out == nullptr)
    {
        const std::string msg = OpenImageIO::geterror();
        throw ExceptionIOError(msg.c_str());
    }
 
    // Create an ImageSpec, which describe the internal structure and attributes of the image file.
    OIIO::ImageSpec spec;
    png_set_image_desc(spec, image.properties());
    png_set_image_attributes(spec, image_attributes);

    // Opens the image file at the specified file path and fills the image header with spec data.
    if (!out->open(filename, spec))
    {
        const std::string msg = out->geterror();
        OIIO::ImageOutput::destroy(out);
        throw ExceptionIOError(msg.c_str());
    }

    // Writes in-memory pixels to image file.
    write_scanline(image, out);

    // Closes the image file.
    if (!out->close())
    {
        const std::string msg = out->geterror();
        OIIO::ImageOutput::destroy(out);
        throw ExceptionIOError(msg.c_str());
    }

    // Destroy the ImageOutput stucture.
    OIIO::ImageOutput::destroy(out);
}

void png_set_image_desc(OIIO::ImageSpec& spec, const CanvasProperties& props)
{
    // Size of the data of the image.
    spec.width = static_cast<int>(props.m_canvas_width);
    spec.height = static_cast<int>(props.m_canvas_height);
    spec.depth = 1;

    // Origin of the pixel data of the image.
    spec.x = 0;
    spec.y = 0;
    spec.z = 0;

    // Full size of the data of the image.
    spec.full_width = spec.width;
    spec.full_height = spec.height;
    spec.full_depth = spec.depth;

    // Origin of the pixel data of the full image.
    spec.full_x = spec.x;
    spec.full_y = spec.y;
    spec.full_z = spec.z;

    // Size of a tile. Unsupported by PNG files.
    spec.tile_width = 0;
    spec.tile_height = 0;
    spec.tile_depth = 0;

    // Number of channels.
    spec.nchannels = static_cast<int>(props.m_channel_count);
    if (spec.nchannels > 3)
    {
        // Names of the first three channels.
        spec.channelnames.push_back("R");
        spec.channelnames.push_back("G");
        spec.channelnames.push_back("B");
    }

    // Sets alpha channel if present.
    if (spec.nchannels == 4)
    {
        spec.channelnames.push_back("A");
        spec.alpha_channel = 3;
    }
    else
        spec.alpha_channel = -1;

    // No depth channel.
    spec.z_channel = -1;

    // No deep data.
    spec.deep = false;

    // Format of the pixel data.
    spec.set_format(OIIO::TypeDesc::UINT8);
}

// See OpenImageIO reference documentation for an exhaustive attribute names list.
// https://github.com/OpenImageIO/oiio/blob/master/src/doc/openimageio.pdf
//
void png_set_image_attributes(OIIO::ImageSpec& spec, const ImageAttributes& image_attributes)
{
    for (const_each<ImageAttributes> i = image_attributes; i; ++i)
    {
        // Fetch the name and the value of the attribute.
        const std::string attr_name = i->key();
        const std::string attr_value = i->value<std::string>();

        if (attr_name == "title")
            spec.attribute("DocumentName", attr_value);

        else if (attr_name == "author")
            spec.attribute("Artist", attr_value);

        else if (attr_name == "description")
            spec.attribute("ImageDescription", attr_value);

        else if (attr_name == "copyright")
            spec.attribute("Copyright", attr_value);

        else if (attr_name == "creation_time")
            spec.attribute("DateTime", attr_value);

        else if (attr_name == "software")
            spec.attribute("Software", attr_value);

        else if (attr_name == "disclaimer")
            spec.attribute("Disclaimer", attr_value);

        else if (attr_name == "warning")
            spec.attribute("Warning", attr_value);

        else if (attr_name == "source")
            spec.attribute("Source", attr_value);

        else if (attr_name == "comment")
            spec.attribute("Comment", attr_value);

        else if (attr_name == "dpi")
        {
            const size_t dpi = from_string<size_t>(attr_value);
            const double dpm = dpi * (100.0 / 2.54);
            spec.attribute("XResolution", static_cast<float>(dpm));
            spec.attribute("YResolution", static_cast<float>(dpm));
            spec.attribute("ResolutionUnit", "cm");
        }

        else
            spec.attribute(attr_name, attr_value);
    }
}

void write_scanline(const ICanvas& image, OIIO::ImageOutput* out)
{
    // Retrieves canvas properties
    const CanvasProperties& props = image.properties();

    // Constructs the temporary buffer holding one row of tiles in target format.
    std::unique_ptr<uint8> buffer(new uint8[props.m_canvas_width * props.m_tile_height * props.m_channel_count]);
    uint8* __restrict buffer_ptr = buffer.get();

    // Constructs the temporary buffer holding pixel channels.
    std::unique_ptr<uint8> pixel_components(new uint8[props.m_channel_count]);
    uint8* __restrict pixel_components_ptr = pixel_components.get();

    // Loops over the rows of tiles
    for (size_t tile_y = 0; tile_y < props.m_tile_count_y; tile_y++)
    {
        // Loops over the columns of tiles
        for (size_t tile_x = 0; tile_x < props.m_tile_count_x; tile_x++)
        {
            // Retrieves the (tile_x, tile_y) tile.
            const Tile& tile = image.tile(tile_x, tile_y);

            // Checks if tile height is lesser or equal than props tile height.
            assert(tile.get_height() <= props.m_tile_height);

            // Loops over the row pixels of the current tile
            for (size_t y = 0; y < tile.get_height(); y++)
            {
                // Checks if tile width is lesser or equal than props tile width.
                assert(tile.get_width() <= props.m_tile_width);

                // Loops over the column pixels of the current tile
                for (size_t x = 0; x < tile.get_width(); x++)
                {
                    // Horizontal coordinate of the pixel in the temporary buffer.
                    const size_t buffer_x = tile_x * props.m_tile_width + x;

                    // Index of the pixel in the temporary buffer.
                    const size_t buffer_index = (y * props.m_canvas_width + buffer_x) * props.m_channel_count;

                    // Checks if tile channel count is lesser or equal than props channel count.
                    assert(tile.get_channel_count() <= props.m_channel_count);

                    // Loops over tile channel count.
                    for (size_t channel_count = 0; channel_count < tile.get_channel_count(); channel_count++)
                    {
                        tile.get_pixel(x, y, pixel_components_ptr);
                        buffer_ptr[buffer_index + channel_count] = pixel_components_ptr[channel_count];
                    }
                }
            }
        }

        // Computes 
        const size_t y_begin = tile_y * props.m_tile_height;
        const size_t y_end = y_begin + props.m_tile_height;

        if (!out->write_scanlines(static_cast<int>(y_begin), static_cast<int>(y_end), 0, OIIO::TypeDesc::UINT8, buffer_ptr))
        {
            const std::string msg = out->geterror();
            out->close();
            OIIO::ImageOutput::destroy(out);
            throw ExceptionIOError(msg.c_str());
        }
    }
}

}	// namespace foundation
