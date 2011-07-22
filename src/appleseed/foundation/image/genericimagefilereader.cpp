
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
#include "genericimagefilereader.h"

// appleseed.foundation headers.
#include "foundation/image/exrimagefilereader.h"
#include "foundation/image/pngimagefilereader.h"
#include "foundation/utility/string.h"

// boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <memory>

using namespace boost;
using namespace std;

namespace foundation
{

//
// GenericImageFileReader class implementation.
//

Image* GenericImageFileReader::read(
    const char*         filename,
    ImageAttributes*    image_attributes)
{
    // Extract the extension of the image filename.
    const filesystem::path filepath(filename);
    const string extension = lower_case(filepath.extension());

    // Create the appropriate image file reader, depending on the filename extension.
    auto_ptr<IImageFileReader> image_file_reader;
    if (extension == ".exr")
        image_file_reader.reset(new EXRImageFileReader());
    else if (extension == ".png")
        image_file_reader.reset(new PNGImageFileReader());
    else throw ExceptionUnknownFileTypeError();

    // Read the image file.
    return image_file_reader->read(filename, image_attributes);
}

}   // namespace foundation
