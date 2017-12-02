
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

#ifndef APPLESEED_FOUNDATION_IMAGE_EXRIMAGEFILEWRITER_H
#define APPLESEED_FOUNDATION_IMAGE_EXRIMAGEFILEWRITER_H

// appleseed.foundation headers.
#include "foundation/image/iimagefilewriter.h"
#include "foundation/image/imageattributes.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class ICanvas; }

namespace foundation
{

//
// OpenEXR image file writer.
//
// The following image attributes are recognized by the OpenEXR format:
//
//   author             string      name of the author of the image
//   creation_time      string      time of original image creation
//   comment            string      miscellaneous comment
//   dpi                float       physical image resolution, in dots per inch
//
// Other image attributes will be stored as generic string attributes.
//
// Reference: openexr/ImfStandardAttributes.h
//

class APPLESEED_DLLSYMBOL EXRImageFileWriter
  : public IImageFileWriter
{
  public:
    // Constructor.
    EXRImageFileWriter();

    // Destructor.
    ~EXRImageFileWriter() override;

    // Write an OpenEXR image file.
    void write(
        const char*             filename,
        const ICanvas&          image,
        const ImageAttributes&  image_attributes = ImageAttributes()) override;

    // Write an OpenEXR image file.
    void write(
        const char*             filename,
        const ICanvas&          image,
        const ImageAttributes&  image_attributes,
        const size_t            channel_count,
        const char**            channel_names);

    // Write an OpenEXR image file with multiple parts.
    void begin_multipart_exr();

    void append_part(
        const char*             part_name,
        const ICanvas&          image,
        const ImageAttributes&  image_attributes,
        const size_t            channel_count,
        const char**            channel_names);

    void write_multipart_exr(const char* filename);

  private:
    struct Impl;
    Impl *impl;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_EXRIMAGEFILEWRITER_H
