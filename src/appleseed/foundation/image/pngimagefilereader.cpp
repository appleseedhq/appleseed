
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
#include "pngimagefilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/exceptionunsupportedimageformat.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/platform/types.h"

// libpng headers.
#include "png.h"

// Standard headers.
#include <csetjmp>
#include <cstddef>
#include <cstdio>
#include <memory>

using namespace std;

namespace foundation
{

//
// PNGImageFileReader class implementation.
//

Image* PNGImageFileReader::read(
    const char*         filename,
    ImageAttributes*    image_attributes)
{
    // Open the file.
    FILE* file = fopen(filename, "rb");
    if (file == 0)
        throw ExceptionIOError("failed to open file", filename);

    // Read and check the file header.
    png_byte header[8];
    {
        const size_t read = fread(header, 1, 8, file);
        if (read != 8 || !png_check_sig(header, 8))
        {
            fclose(file);
            throw ExceptionIOError("not a valid PNG file", filename);
        }
    }

    // Create read structure.
    png_structp png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, 0, 0, 0);
    if (png_ptr == 0)
    {
        fclose(file);
        throw ExceptionIOError("not a valid PNG file", filename);
    }

    // Create info structure.
    png_infop info_ptr = png_create_info_struct(png_ptr);
    if (info_ptr == 0)
    {
        png_destroy_read_struct(&png_ptr, 0, 0);
        fclose(file);
        throw ExceptionIOError("not a valid PNG file", filename);
    }

    // Set error handler.
    if (setjmp(png_jmpbuf(png_ptr)))
    {
        png_destroy_read_struct(&png_ptr, &info_ptr, 0);
        fclose(file);
        throw ExceptionIOError("failed to read file", filename);
    }

    // Initialize libpng I/O.
    png_init_io(png_ptr, file);

    // Tell libpng we have already handled the first 8 magic bytes.
    png_set_sig_bytes(png_ptr, 8);

    // Read the information before the actual image data.
    png_read_info(png_ptr, info_ptr);

    // Extract image dimensions.
    const png_uint_32 width = png_get_image_width(png_ptr, info_ptr);
    const png_uint_32 height = png_get_image_height(png_ptr, info_ptr);

    // Extract color type.
    const png_byte color_type = png_get_color_type(png_ptr, info_ptr);
    if (color_type != PNG_COLOR_TYPE_RGB && color_type != PNG_COLOR_TYPE_RGBA)
    {
        png_destroy_read_struct(&png_ptr, &info_ptr, 0);
        fclose(file);
        throw ExceptionUnsupportedImageFormat();
    }

    // Extract bit depth.
    const png_byte bit_depth = png_get_bit_depth(png_ptr, info_ptr);
    if (bit_depth != 8)
    {
        png_destroy_read_struct(&png_ptr, &info_ptr, 0);
        fclose(file);
        throw ExceptionUnsupportedImageFormat();
    }

    // Create the image.
    auto_ptr<Image> image(
        new Image(
            width, height,
            width, height,
            color_type == PNG_COLOR_TYPE_RGB ? 3 : 4,
            PixelFormatUInt8));

    // Consistency check.
    const png_size_t rowbytes = png_get_rowbytes(png_ptr, info_ptr);
    if (rowbytes != width * image->properties().m_pixel_size)
    {
        png_destroy_read_struct(&png_ptr, &info_ptr, 0);
        fclose(file);
        throw ExceptionIOError("failed to read file", filename);
    }

    // Allocate row pointers.
    uint8* pixel_data = image->pixel(0, 0);
    png_bytep* rows = new png_bytep[height];
    for (png_uint_32 i = 0; i < height; ++i)
        rows[i] = pixel_data + i * rowbytes;

    // Read the image.
    png_read_image(png_ptr, rows);

    // Cleanup.
    delete [] rows;
    png_destroy_read_struct(&png_ptr, &info_ptr, 0);
    fclose(file);

    // Return the image.
    return image.release();
}

}   // namespace foundation
