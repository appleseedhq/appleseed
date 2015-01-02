
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
#include "pngimagefilewriter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/icanvas.h"
#include "foundation/image/tile.h"
#include "foundation/platform/types.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// libpng headers.
#include "png.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdio>
#include <string>
#include <vector>

using namespace std;

namespace foundation
{

//
// PNGImageFileWriter class implementation.
//

namespace
{
    // Fatal error callback.
    void error_callback(png_structp png_ptr, png_const_charp error_msg)
    {
        // Close the file.
        FILE* fp = static_cast<FILE*>(png_get_error_ptr(png_ptr));
        fclose(fp);

        // Deallocate the png_struct structure.
        png_destroy_write_struct(&png_ptr, 0);

        // Throw an exception.
        throw Exception(error_msg);
    }

    // Warning callback.
    void warning_callback(png_structp png_ptr, png_const_charp warning_msg)
    {
        // todo: implement.
    }

    // Add a text chunk for a given (key, text) pair.
    void add_text_chunk(
        vector<png_text_struct>&    text_chunks,
        const string&               key,
        const string&               text)
    {
        png_text text_chunk;
        text_chunk.key = duplicate_string(key.c_str());
        text_chunk.text = duplicate_string(text.c_str());
        text_chunk.compression = PNG_TEXT_COMPRESSION_NONE;
        text_chunks.push_back(text_chunk);
    }

    // Add image attributes to a PNG file.
    void add_attributes(
        png_structp                 png_ptr,
        png_infop                   info_ptr,
        vector<png_text_struct>&    text_chunks,
        const ImageAttributes&      image_attributes)
    {
        for (const_each<ImageAttributes> i = image_attributes; i; ++i)
        {
            // Fetch the name and the value of the attribute.
            const string attr_name = i->name();
            const string attr_value = i->value<string>();

            if (attr_name == "title")
                add_text_chunk(text_chunks, "Title", attr_value);

            else if (attr_name == "author")
                add_text_chunk(text_chunks, "Author", attr_value);

            else if (attr_name == "description")
                add_text_chunk(text_chunks, "Description", attr_value);

            else if (attr_name == "copyright")
                add_text_chunk(text_chunks, "Copyright", attr_value);

            else if (attr_name == "creation_time")
                add_text_chunk(text_chunks, "Creation Time", attr_value);

            else if (attr_name == "software")
                add_text_chunk(text_chunks, "Software", attr_value);

            else if (attr_name == "disclaimer")
                add_text_chunk(text_chunks, "Disclaimer", attr_value);

            else if (attr_name == "warning")
                add_text_chunk(text_chunks, "Warning", attr_value);

            else if (attr_name == "source")
                add_text_chunk(text_chunks, "Source", attr_value);

            else if (attr_name == "comment")
                add_text_chunk(text_chunks, "Comment", attr_value);

            else if (attr_name == "dpi")
            {
                const size_t dpi = from_string<size_t>(attr_value);
                const double dpm = dpi * (100.0 / 2.54);
                png_set_pHYs(
                    png_ptr,
                    info_ptr,
                    static_cast<png_uint_32>(dpm),
                    static_cast<png_uint_32>(dpm),
                    PNG_RESOLUTION_METER);
            }

            else
                add_text_chunk(text_chunks, attr_name, attr_value);
        }
    }

    // Deallocate text chunks.
    void destroy_text_chunks(vector<png_text_struct>& text_chunks)
    {
        for (size_t i = 0; i < text_chunks.size(); ++i)
        {
            free_string(text_chunks[i].key);
            free_string(text_chunks[i].text);
        }

        text_chunks.clear();
    }
}

void PNGImageFileWriter::write(
    const char*             filename,
    const ICanvas&          image,
    const ImageAttributes&  image_attributes)
{
    // Retrieve canvas properties.
    const CanvasProperties& props = image.properties();

    // todo: lift these limitations.
    assert(props.m_channel_count == 3 || props.m_channel_count == 4);

    // Open the file in write mode.
    FILE* fp = fopen(filename, "wb");
    if (fp == 0)
        throw ExceptionIOError();

    // Allocate and initialize the png_struct structure.
    png_structp png_ptr =
        png_create_write_struct(
            PNG_LIBPNG_VER_STRING,
            fp,
            error_callback,
            warning_callback);
    if (png_ptr == 0)
    {
        fclose(fp);
        throw ExceptionMemoryError();
    }

    // Allocate the png_info structure.
    png_infop info_ptr = png_create_info_struct(png_ptr);
    if (info_ptr == 0)
    {
        png_destroy_write_struct(&png_ptr, 0);
        fclose(fp);
        throw ExceptionMemoryError();
    }

    // Set up the output control.
    png_init_io(png_ptr, fp);

    // Set image information.
    png_set_IHDR(
        png_ptr,
        info_ptr,
        static_cast<png_uint_32>(props.m_canvas_width),
        static_cast<png_uint_32>(props.m_canvas_height),
        8,                                  // bit depth -- todo: allow higher bit depths
        props.m_channel_count == 4
            ? PNG_COLOR_TYPE_RGB_ALPHA
            : PNG_COLOR_TYPE_RGB,
        PNG_INTERLACE_NONE,
        PNG_COMPRESSION_TYPE_DEFAULT,
        PNG_FILTER_TYPE_DEFAULT);

    // Mark the image as being sRGB (implying specific gamma and color matching functions).
    // See http://www.vias.org/pngguide/chapter10_07.html for details about intents.
    png_set_sRGB_gAMA_and_cHRM(
        png_ptr,
        info_ptr,
        PNG_sRGB_INTENT_PERCEPTUAL);        // todo: allow the user to select different intents

    // Set the number of significant bits for each of the R, G, B and A channels.
    // todo: are we required to provide these information?
    png_color_8 sig_bit;
    sig_bit.red = 8;
    sig_bit.green = 8;
    sig_bit.blue = 8;
    sig_bit.alpha = 8;
    sig_bit.gray = 8;                       // for completeness
    png_set_sBIT(png_ptr, info_ptr, &sig_bit);

    // Add image attributes.
    vector<png_text_struct> text_chunks;
    add_attributes(
        png_ptr,
        info_ptr,
        text_chunks,
        image_attributes);
    if (!text_chunks.empty())
    {
        png_set_text(
            png_ptr,
            info_ptr,
            &text_chunks[0],
            static_cast<int>(text_chunks.size()));
    }

    // Write the file header information.
    png_write_info(png_ptr, info_ptr);

    // Create the temporary buffer holding one row of tiles in target format.
    vector<uint8> buffer(
          props.m_canvas_width
        * props.m_tile_height
        * props.m_channel_count);

    // Construct pointers to each row of the temporary buffer.
    vector<uint8*> buffer_rows(props.m_tile_height);
    for (size_t y = 0; y < props.m_tile_height; ++y)
        buffer_rows[y] = &buffer[y * props.m_canvas_width * props.m_channel_count];

    // Loop over the rows of tiles.
    for (size_t tile_y = 0; tile_y < props.m_tile_count_y; ++tile_y)
    {
        // Convert this row of tiles to target format.
        for (size_t tile_x = 0; tile_x < props.m_tile_count_x; ++tile_x)
        {
            const Tile& tile = image.tile(tile_x, tile_y);
            assert(tile.get_height() <= props.m_tile_height);
            for (size_t y = 0; y < tile.get_height(); ++y)
            {
                for (size_t x = 0; x < tile.get_width(); ++x)
                {
                    // Horizontal coordinate of the pixel in the temporary buffer.
                    const size_t buffer_x = tile_x * props.m_tile_width + x;

                    // Index of the pixel in the temporary buffer.
                    const size_t buffer_index =
                        (y * props.m_canvas_width + buffer_x) * props.m_channel_count;

                    // Fetch the pixel at coordinates (x, y) in the tile,
                    // perform format conversion if necessary, and store
                    // the converted pixel into the temporary buffer.
                    if (tile.get_channel_count() == 3)
                    {
                        Color3b pixel;
                        tile.get_pixel(x, y, pixel);
                        buffer[buffer_index + 0] = pixel[0];
                        buffer[buffer_index + 1] = pixel[1];
                        buffer[buffer_index + 2] = pixel[2];
                    }
                    else
                    {
                        assert(tile.get_channel_count() == 4);
                        Color4b pixel;
                        tile.get_pixel(x, y, pixel);
                        buffer[buffer_index + 0] = pixel[0];
                        buffer[buffer_index + 1] = pixel[1];
                        buffer[buffer_index + 2] = pixel[2];
                        buffer[buffer_index + 3] = pixel[3];
                    }
                }
            }
        }

        // Write this row of tiles to the file.
        const size_t row_count = image.tile(0, tile_y).get_height();
        png_write_rows(
            png_ptr,
            &buffer_rows[0],
            static_cast<png_uint_32>(row_count));
    }

    // Finish writing the file.
    png_write_end(png_ptr, 0);

    // Deallocate the png_struct and png_info structures.
    png_destroy_write_struct(&png_ptr, &info_ptr);

    // Deallocate text chunks.
    destroy_text_chunks(text_chunks);

    // Close the file.
    fclose(fp);
}

}   // namespace foundation
