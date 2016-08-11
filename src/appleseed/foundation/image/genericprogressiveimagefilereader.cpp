
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
#include "genericprogressiveimagefilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionunsupportedfileformat.h"
#include "foundation/image/progressiveexrimagefilereader.h"
#include "foundation/image/progressivepngimagefilereader.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <memory>
#include <string>

using namespace std;
namespace bf = boost::filesystem;

namespace foundation
{

//
// GenericProgressiveImageFileReader class implementation.
//

struct GenericProgressiveImageFileReader::Impl
{
    Logger*                                 m_logger;
    bool                                    m_has_default_tile_size;
    size_t                                  m_default_tile_width;
    size_t                                  m_default_tile_height;
    auto_ptr<IProgressiveImageFileReader>   m_reader;
};

GenericProgressiveImageFileReader::GenericProgressiveImageFileReader(Logger* logger)
  : impl(new Impl())
{
    impl->m_logger = logger;
    impl->m_has_default_tile_size = false;
    impl->m_default_tile_width = 0;
    impl->m_default_tile_height = 0;
}

GenericProgressiveImageFileReader::GenericProgressiveImageFileReader(
    const size_t        default_tile_width,
    const size_t        default_tile_height)
  : impl(new Impl())
{
    impl->m_logger = 0;
    impl->m_has_default_tile_size = true;
    impl->m_default_tile_width = default_tile_width;
    impl->m_default_tile_height = default_tile_height;
}

GenericProgressiveImageFileReader::GenericProgressiveImageFileReader(
    Logger*             logger,
    const size_t        default_tile_width,
    const size_t        default_tile_height)
  : impl(new Impl())
{
    impl->m_logger = logger;
    impl->m_has_default_tile_size = true;
    impl->m_default_tile_width = default_tile_width;
    impl->m_default_tile_height = default_tile_height;
}

GenericProgressiveImageFileReader::~GenericProgressiveImageFileReader()
{
    if (is_open())
        close();

    delete impl;
}

void GenericProgressiveImageFileReader::open(const char* filename)
{
    assert(filename);
    assert(!is_open());

    const bf::path filepath(filename);
    const string extension = lower_case(filepath.extension().string());

    if (extension == ".exr")
    {
        impl->m_reader.reset(
            impl->m_has_default_tile_size
                ? new ProgressiveEXRImageFileReader(
                      impl->m_logger,
                      impl->m_default_tile_width,
                      impl->m_default_tile_height)
                : new ProgressiveEXRImageFileReader(
                      impl->m_logger));
    }
    else if (extension == ".png")
    {
        impl->m_reader.reset(
            impl->m_has_default_tile_size
                ? new ProgressivePNGImageFileReader(
                      impl->m_logger,
                      impl->m_default_tile_width,
                      impl->m_default_tile_height)
                : new ProgressivePNGImageFileReader(
                      impl->m_logger));
    }
    else
    {
        throw ExceptionUnsupportedFileFormat(filename);
    }

    impl->m_reader->open(filename);
}

void GenericProgressiveImageFileReader::close()
{
    assert(is_open());
    impl->m_reader->close();
    impl->m_reader.reset();
}

bool GenericProgressiveImageFileReader::is_open() const
{
    return impl->m_reader.get() && impl->m_reader->is_open();
}

void GenericProgressiveImageFileReader::read_canvas_properties(
    CanvasProperties&   props)
{
    assert(is_open());
    impl->m_reader->read_canvas_properties(props);
}

void GenericProgressiveImageFileReader::read_image_attributes(
    ImageAttributes&    attrs)
{
    assert(is_open());
    impl->m_reader->read_image_attributes(attrs);
}

Tile* GenericProgressiveImageFileReader::read_tile(
    const size_t        tile_x,
    const size_t        tile_y)
{
    assert(is_open());
    return impl->m_reader->read_tile(tile_x, tile_y);
}

}   // namespace foundation
