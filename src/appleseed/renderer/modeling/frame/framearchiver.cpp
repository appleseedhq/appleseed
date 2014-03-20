
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "framearchiver.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/core/exceptions/exceptionunsupportedfileformat.h"
#include "foundation/image/color.h"
#include "foundation/image/exceptionunsupportedimageformat.h"
#include "foundation/image/exrimagefilewriter.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/imageattributes.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/math/fastmath.h"
#include "foundation/math/scalar.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/platform/timer.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cmath>
#include <memory>
#include <string>

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

//
// FrameArchiver class implementation.
//

bool FrameArchiver::archive(
    const char*         directory,
    char**              output_path) const
{
    assert(directory);

    // Construct the name of the image file.
    const string filename =
        "autosave." + get_time_stamp_string() + ".exr";

    // Construct the path to the image file.
    const string file_path = (filesystem::path(directory) / filename).string();

    // Return the path to the image file.
    if (output_path)
        *output_path = duplicate_string(file_path.c_str());

    Image transformed_image(*impl->m_image);
    transform_to_output_color_space(transformed_image);

    return
        write_image(
            file_path.c_str(),
            transformed_image,
            ImageAttributes::create_default_attributes());
}

}
