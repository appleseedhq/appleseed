
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

#ifndef APPLESEED_FOUNDATION_IMAGE_PNGIMAGEFILEWRITER_H
#define APPLESEED_FOUNDATION_IMAGE_PNGIMAGEFILEWRITER_H

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/image/iimagefilewriter.h"
#include "foundation/image/imageattributes.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class ICanvas; }

namespace foundation
{

//
// PNG image file writer.
//
// The following image attributes are recognized by the PNG format:
//
//   title              string      short (one line) title or caption for the image
//   author             string      name of the author of the image
//   description        string      description of the image (possibly long)
//   copyright          string      copyright notice
//   creation_time      string      time of original image creation
//   software           string      software used to create the image
//   disclaimer         string      legal disclaimer
//   warning            string      warning of nature of content
//   source             string      device used to create the image
//   comment            string      miscellaneous comment
//   dpi                integer     physical image resolution, in dots per inch
//
// Other image attributes will be stored as generic text chunks.
//
// Reference: http://www.libpng.org/pub/png/spec/1.2/PNG-Chunks.html
//

class APPLESEED_DLLSYMBOL PNGImageFileWriter
  : public IImageFileWriter
{
  public:
    // Memory allocation error.
    struct ExceptionMemoryError : public Exception {};

    // Write a PNG image file.
    virtual void write(
        const char*             filename,
        const ICanvas&          image,
        const ImageAttributes&  image_attributes = ImageAttributes());
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_PNGIMAGEFILEWRITER_H
