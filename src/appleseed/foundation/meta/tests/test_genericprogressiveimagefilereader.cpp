
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/image/genericprogressiveimagefilereader.h"
#include "foundation/image/tile.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <memory>

using namespace foundation;

TEST_SUITE(Foundation_Image_GenericProgressiveImageFileReader)
{
    TEST_CASE(ReadTile_CalledASecondTimeOnTheSamePFMFile_ThrowsIOException)
    {
        GenericProgressiveImageFileReader reader;
        reader.open("unit tests/inputs/test_genericprogressiveimagefilereader_image.pnm");

        // Read the image a first time, then delete it.
        delete reader.read_tile(0, 0);

        // Read the image a second time, then delete it.
        // With OpenImageIO 1.5.20, reading a second time fails and this line throws a foundation::IOException.
        // See https://github.com/OpenImageIO/oiio/issues/1600 for details.
        delete reader.read_tile(0, 0);
    }
}
