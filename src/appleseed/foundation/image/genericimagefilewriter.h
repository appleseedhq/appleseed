
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2019 Francois Beaune, The appleseedhq Organization
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

#pragma once

// appleseed.foundation headers.
#include "foundation/image/iimagefilewriter.h"
#include "foundation/image/pixel.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/imageio.h"
#include "OpenImageIO/version.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation { class ICanvas; }
namespace foundation { class ImageAttributes; }

namespace foundation
{

class APPLESEED_DLLSYMBOL GenericImageFileWriter
  : public IImageFileWriter
{
  public:
    explicit GenericImageFileWriter(const char* filename);
    ~GenericImageFileWriter() override;

    // Add an image to the image stack.
    void append_image(const ICanvas* image);

    // Set the pixel format of the topmost image on the stack.
    void set_image_output_format(const PixelFormat output_pixel_format);

    // Set the image channel names of the topmost image on the stack.
    void set_image_channels(
        const size_t    channel_count,
        const char**    channel_names);

    // Set attributes of the topmost image on the stack.
    void set_image_attributes(const ImageAttributes& image_attributes);

    // Return the number of images in the stack.
    size_t get_image_count() const;

    // Write all images from the stack (if possible) to disk.
    void write();

  private:
    struct Impl;
    Impl* impl;
};

}   // namespace foundation
