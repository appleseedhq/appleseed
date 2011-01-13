
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "foundation/image/progressiveexrimagefilereader.h"
#include "foundation/utility/string.h"

// boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <memory>

using namespace boost;
using namespace std;

namespace foundation
{

//
// GenericProgressiveImageFileReader class implementation.
//

struct GenericProgressiveImageFileReader::Impl
{
    Logger*                                 m_logger;
    auto_ptr<IProgressiveImageFileReader>   m_reader;
};

// Constructor.
GenericProgressiveImageFileReader::GenericProgressiveImageFileReader(Logger* logger)
  : impl(new Impl())
{
    impl->m_logger = logger;
}

// Destructor.
GenericProgressiveImageFileReader::~GenericProgressiveImageFileReader()
{
    if (is_open())
        close();
}

// Open an image file.
void GenericProgressiveImageFileReader::open(const char* filename)
{
    assert(filename);
    assert(!is_open());

    // Extract the extension of the image filename.
    const filesystem::path filepath(filename);
    const string extension = lower_case(filepath.extension());

    // Create the appropriate image file reader, depending on the filename extension.
    if (extension == ".exr")
        impl->m_reader.reset(new ProgressiveEXRImageFileReader(impl->m_logger));
    else throw ExceptionUnknownFileTypeError();

    // Open the image file.
    impl->m_reader->open(filename);
}

// Close the image file.
void GenericProgressiveImageFileReader::close()
{
    assert(is_open());
    impl->m_reader->close();
    impl->m_reader.reset();
}

// Return true if an image file is currently open.
bool GenericProgressiveImageFileReader::is_open() const
{
    return impl->m_reader.get() && impl->m_reader->is_open();
}

// Read canvas properties.
void GenericProgressiveImageFileReader::read_canvas_properties(
    CanvasProperties&   props)
{
    assert(is_open());
    impl->m_reader->read_canvas_properties(props);
}

// Read image attributes.
void GenericProgressiveImageFileReader::read_image_attributes(
    ImageAttributes&    attrs)
{
    assert(is_open());
    impl->m_reader->read_image_attributes(attrs);
}

// Read an image tile. Returns a newly allocated tile.
Tile* GenericProgressiveImageFileReader::read_tile(
    const size_t        tile_x,
    const size_t        tile_y)
{
    assert(is_open());
    return impl->m_reader->read_tile(tile_x, tile_y);
}

}   // namespace foundation
