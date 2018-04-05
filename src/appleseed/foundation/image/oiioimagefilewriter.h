
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

#ifndef APPLESEED_FOUNDATION_IMAGE_OIIOIMAGEFILEWRITER_H
#define APPLESEED_FOUNDATION_IMAGE_OIIOIMAGEFILEWRITER_H

// appleseed.foundation headers.
#include "foundation/image/icanvas.h"

// openimageio headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/imageio.h"
#include "foundation/platform/_endoiioheaders.h"

namespace foundation
{

class APPLESEED_DLLSYMBOL OIIOImageFileWriter
{
  public:

    void create(const char* filename);

    void append_image(const ICanvas* image);

    void set_image_spec(
        const CanvasProperties& props,
        const size_t            channel_count,
        const char**            channel_names,
        const PixelFormat       output_pixel_format);

    void set_image_spec(
        const CanvasProperties& props,
        const PixelFormat       output_pixel_format);

    void set_image_attribute(
        const char*     key,
        const char*     value);

    size_t number_of_images() const;

    void write();

    void destroy();

  private:

    void close_file();
    void write_single_image();
    void write_multi_images();
    void write(const ICanvas* image);
    void write_tiles(const ICanvas* image);
    void write_scanlines(const ICanvas* image);

  private:

    struct OIIOImages
    {
        std::vector<const ICanvas*>     m_canvas;
        std::vector<OIIO::ImageSpec>    m_spec;
    };

    OIIO::ImageOutput*  m_writer;
    OIIOImages          m_images;
    const char*         m_filename;

};

}   // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_OIIOIMAGEFILEWRITER_H
