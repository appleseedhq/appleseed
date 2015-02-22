
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_UTILITY_SAMPLINGMODE_H
#define APPLESEED_RENDERER_UTILITY_SAMPLINGMODE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/makevector.h"

// Standard headers.
#include <string>

namespace renderer
{

inline SamplingContext::Mode get_sampling_context_mode(const ParamArray& params)
{
    const std::string sampling_mode =
        params.get_required<std::string>(
            "sampling_mode",
            "rng",
            foundation::make_vector("rng", "qmc"));

    return
        sampling_mode == "rng"
            ? SamplingContext::RNGMode
            : SamplingContext::QMCMode;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_UTILITY_SAMPLINGMODE_H
