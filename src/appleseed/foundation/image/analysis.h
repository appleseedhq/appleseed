
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_IMAGE_ANALYSIS_H
#define APPLESEED_FOUNDATION_IMAGE_ANALYSIS_H

// appleseed.main headers.
#include "main/dllsymbol.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"

// Forward declarations.
namespace foundation    { class Image; }

namespace foundation
{

//
// Compute the average Rec. 709 relative luminance of a linear RGB image.
// Pixels containing NaN values are skipped.
//

DLLSYMBOL double compute_average_luminance(
    const Image&    image);


//
// Exception thrown by foundation::compute_rms_deviation() when the two
// images are incompatible.
//

struct ExceptionIncompatibleImages
  : public Exception
{
};


//
// Compute the Root-Mean-Square deviation between two images.
//

DLLSYMBOL double compute_rms_deviation(
    const Image&    image1,
    const Image&    image2);

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_ANALYSIS_H
