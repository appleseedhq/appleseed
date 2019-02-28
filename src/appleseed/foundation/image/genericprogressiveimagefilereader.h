
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
// A progressive image file reader based on OpenImageIO.
//

class APPLESEED_DLLSYMBOL GenericProgressiveImageFileReader
  : public IProgressiveImageFileReader
{
  public:
    // Constructor.
    explicit GenericProgressiveImageFileReader(Logger* logger = nullptr);

    // Destructor.
    ~GenericProgressiveImageFileReader() override;

    // Open an image file.
    void open(
        const char*         filename) override;

    // Close the image file.
    void close() override;

    // Return true if an image file is currently open.
    bool is_open() const override;

    // Read canvas properties.
    void read_canvas_properties(
        CanvasProperties&   props) override;

    // Read image attributes.
    void read_image_attributes(
        ImageAttributes&    attrs) override;

    // Choose the layer in the image file if available.
    bool choose_subimage(const size_t subimage) const;

    // Read an image tile. Returns a newly allocated tile.
    Tile* read_tile(
        const size_t        tile_x,
        const size_t        tile_y) override;

    // Read an image tile. Outputs the content in the given tile.
    void read_tile(
        const size_t        tile_x,
        const size_t        tile_y,
        Tile*               output_tile);

  private:
    struct Impl;
    Impl* impl;
};

}   // namespace foundation
