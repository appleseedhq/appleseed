
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/containers/dictionary.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace foundation
{

//
// Image attributes.
//
//
// Here is a non-exhaustive list of attributes you can fill for images:
//
// - author: The author of the image. [string]
// - copyright: Any copyright notice or owner of the image. [string]
// - title: The name of the overall document that this image is a part of. [string]
// - description: The image description. [string]
// - date: The creation date of the image. [string]
// - software: The software that was used to create the image. [string]
// - computer: The name or identity of the computer that created the image. [string]
// - image_name: The name of the image. [string]
// - white_xy_chromaticity: The white chromaticity of the image. [vector2]
// - red_xy_chromaticity: The red chromaticity of the image. [vector2]
// - green_xy_chromaticity: The green chromaticity of the image. [vector2]
// - blue_xy_chromaticity: The blue chromaticity of the image. [vector2]
// - color_space: The color space of the image. [string]
//      - linear
//      - sRGB
// - compression: The compression mode of the image. [string]
// - compression_quality: The compression quality of the image between 0 and 100. [int]
// - dpi: The resolution of the image in dots per inch. [int]
//

class APPLESEED_DLLSYMBOL ImageAttributes
  : public StringDictionary
{
  public:
    // Create a default set of image attributes.
    static ImageAttributes create_default_attributes();
};

}   // namespace foundation
