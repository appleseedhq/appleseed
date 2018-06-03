
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

#ifndef APPLESEED_FOUNDATION_IMAGE_GENERICIMAGEFILEWRITER_H
#define APPLESEED_FOUNDATION_IMAGE_GENERICIMAGEFILEWRITER_H

// appleseed.foundation headers.
#include "foundation/image/imageattributes.h"
#include "foundation/image/pixel.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/imageio.h"
#include "foundation/platform/_endoiioheaders.h"

// Forward declarations.
namespace foundation { class ICanvas; }

namespace foundation
{

class APPLESEED_DLLSYMBOL GenericImageFileWriter
{
  public:
    explicit GenericImageFileWriter(const char* filename);
    ~GenericImageFileWriter();

    void append_image(const ICanvas* image);

    void set_image_output_format(const PixelFormat output_pixel_format);

    void set_image_channels(
        const size_t    channel_count,
        const char**    channel_names);

    void set_image_attributes(const ImageAttributes& image_attributes);

    size_t get_image_count() const;

    void write();

  private:
    void close_file();

    void set_image_spec();
    
    void set_generic_image_attributes(const ImageAttributes& image_attributes);
    void set_exr_image_attributes(const ImageAttributes& image_attributes);
    
    void write(const size_t image_index);
    void write_single_image();
    void write_multi_images();

    void write_scanlines(const size_t image_index);
    void write_tiles(const size_t image_index);

  private:
    struct Impl;
    Impl* impl;

    OIIO::ImageOutput*  m_writer;
    const char*         m_filename;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_GENERICIMAGEFILEWRITER_H
