
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

#ifndef APPLESEED_FOUNDATION_IMAGE_PROGRESSIVEPNGIMAGEFILEREADER_H
#define APPLESEED_FOUNDATION_IMAGE_PROGRESSIVEPNGIMAGEFILEREADER_H

// appleseed.foundation headers.
#include "foundation/image/iprogressiveimagefilereader.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class CanvasProperties; }
namespace foundation    { class ImageAttributes; }
namespace foundation    { class Logger; }
namespace foundation    { class Tile; }

namespace foundation
{

//
// Progressive PNG image file reader interface.
//

class APPLESEED_DLLSYMBOL ProgressivePNGImageFileReader
  : public IProgressiveImageFileReader
{
  public:
    // Constructors.
    explicit ProgressivePNGImageFileReader(Logger* logger = 0);
    ProgressivePNGImageFileReader(
        const size_t        tile_width,
        const size_t        tile_height);
    ProgressivePNGImageFileReader(
        Logger*             logger,
        const size_t        tile_width,
        const size_t        tile_height);

    // Destructor.
    ~ProgressivePNGImageFileReader();

    // Open an image file.
    virtual void open(
        const char*         filename);

    // Close the image file.
    virtual void close();

    // Return true if an image file is currently open.
    virtual bool is_open() const;

    // Read canvas properties.
    virtual void read_canvas_properties(
        CanvasProperties&   props);

    // Read image attributes.
    virtual void read_image_attributes(
        ImageAttributes&    attrs);

    // Read an image tile. Returns a newly allocated tile.
    virtual Tile* read_tile(
        const size_t        tile_x,
        const size_t        tile_y);

  private:
    struct Impl;
    Impl* impl;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_PROGRESSIVEPNGIMAGEFILEREADER_H
