
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Tiago Chaves, The appleseedhq Organization
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
#include "foundation/image/color.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Image; }

namespace renderer
{

// Bilinear interpolation of RGB colors.
foundation::Color3f blerp(
    const foundation::Image&    image,
    const std::size_t           x0,
    const std::size_t           y0,
    const std::size_t           x1,
    const std::size_t           y1,
    const float                 fx,
    const float                 fy);

// Returns the weighted average of the four pixels closest to the image coordinate (fx, fy).
// Note: assumes fx and fy are valid coordinates (i.e. they are inside the image boundaries).
foundation::Color3f box_sample(
    const foundation::Image& image,
    const float fx,
    const float fy);

// Returns the weighted average of the four pixels closest to the image coordinate (fx, fy).
// Samples the edge-most pixels when coordinates are outside the image (i.e. texture clamping).
foundation::Color3f clamped_box_sample(
    const foundation::Image& image,
    const float fx,
    const float fy);

//
// Sampling filter from Masaki Kawase's GDC2003 Presentation:
// "Frame Buffer Postprocessing Effects in DOUBLE-S.T.E.A.L (Wreckless)".
//
// Reference:
//
//   http://www.daionet.gr.jp/~masa/archives/GDC2003_DSTEAL.ppt
//

foundation::Color3f kawase_sample(
    const foundation::Image&    image,
    const std::size_t           x,
    const std::size_t           y,
    const std::size_t           offset);

//
// Sampling filters from Marius Bjørge's SIGGRAPH2015 Presentation: "Bandwidth-Efficient Rendering".
//
// Reference:
//
//   https://community.arm.com/cfs-file/__key/communityserver-blogs-components-weblogfiles/00-00-00-20-66/siggraph2015_2D00_mmg_2D00_marius_2D00_notes.pdf
//
//

// Note: corner_x and corner_y should be half-integer values.
foundation::Color3f dual_filter_downsample(
    const foundation::Image&    image,
    const float                 corner_x,
    const float                 corner_y,
    const std::size_t           offset = 1);

foundation::Color3f dual_filter_upsample(
    const foundation::Image&    image,
    const std::size_t           center_x,
    const std::size_t           center_y,
    const std::size_t           offset = 1);

}   // namespace renderer
