
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
#include "progressivepngimagefilereader.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/imageattributes.h"
#include "foundation/image/pngimagefilereader.h"
#include "foundation/image/tile.h"

// Standard headers.
#include <cassert>
#include <memory>

using namespace std;

namespace foundation
{

//
// ProgressivePNGImageFileReader class implementation.
//

struct ProgressivePNGImageFileReader::Impl
{
    Logger*             m_logger;
    size_t              m_tile_width;
    size_t              m_tile_height;
    auto_ptr<Image>     m_image;
    ImageAttributes     m_attributes;
};

ProgressivePNGImageFileReader::ProgressivePNGImageFileReader(Logger* logger)
  : impl(new Impl())
{
    impl->m_logger = logger;
    impl->m_tile_width = 32;
    impl->m_tile_height = 32;
}

ProgressivePNGImageFileReader::ProgressivePNGImageFileReader(
    const size_t        tile_width,
    const size_t        tile_height)
  : impl(new Impl())
{
    impl->m_logger = 0;
    impl->m_tile_width = tile_width;
    impl->m_tile_height = tile_height;
}

ProgressivePNGImageFileReader::ProgressivePNGImageFileReader(
    Logger*             logger,
    const size_t        tile_width,
    const size_t        tile_height)
  : impl(new Impl())
{
    impl->m_logger = logger;
    impl->m_tile_width = tile_width;
    impl->m_tile_height = tile_height;
}

ProgressivePNGImageFileReader::~ProgressivePNGImageFileReader()
{
    if (is_open())
        close();

    delete impl;
}

void ProgressivePNGImageFileReader::open(const char* filename)
{
    assert(filename);
    assert(!is_open());

    PNGImageFileReader reader;
    auto_ptr<Image> image(reader.read(filename, &impl->m_attributes));

    impl->m_image.reset(
        new Image(
            *image.get(),
            impl->m_tile_width,
            impl->m_tile_height,
            image->properties().m_pixel_format));
}

void ProgressivePNGImageFileReader::close()
{
    assert(is_open());
    impl->m_image.reset();
}

bool ProgressivePNGImageFileReader::is_open() const
{
    return impl->m_image.get() != 0;
}

void ProgressivePNGImageFileReader::read_canvas_properties(
    CanvasProperties&   props)
{
    assert(is_open());
    props = impl->m_image->properties();
}

void ProgressivePNGImageFileReader::read_image_attributes(
    ImageAttributes&    attrs)
{
    assert(is_open());
    attrs = impl->m_attributes;
}

Tile* ProgressivePNGImageFileReader::read_tile(
    const size_t        tile_x,
    const size_t        tile_y)
{
    assert(is_open());
    return new Tile(impl->m_image->tile(tile_x, tile_y));
}

}   // namespace foundation
