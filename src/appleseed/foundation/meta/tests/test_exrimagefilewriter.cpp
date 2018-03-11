
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

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/exrimagefilewriter.h"
#include "foundation/image/genericprogressiveimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <memory>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Image_EXRImageFileWriter)
{
    static const char* Filename = "unit tests/outputs/test_exrimagefilewriter.exr";
    static const Color4b Reference(50, 100, 150, 42);

    void write_test_openexr_file_to_disk()
    {
        Image image(2, 2, 32, 32, 4, PixelFormatFloat);
        image.clear(Reference);

        EXRImageFileWriter writer;
        writer.write(Filename, image);
    }

    TEST_CASE(CorrectlyWriteTestImage)
    {
        write_test_openexr_file_to_disk();

        GenericProgressiveImageFileReader reader;
        reader.open(Filename);
        unique_ptr<Tile> tile(reader.read_tile(0, 0));

        for (size_t i = 0; i < 4; ++i)
        {
            Color4b c;
            tile->get_pixel(i, c);
            EXPECT_EQ(Reference, c);
        }
    }
}
