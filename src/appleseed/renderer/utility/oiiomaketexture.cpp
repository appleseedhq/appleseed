
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Jonathan Dent, The appleseedhq Organization
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
#include "oiiomaketexture.h"

// OIIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/imagebufalgo.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <sstream>
#include <string>
#include <unordered_map>

using namespace foundation;
using namespace OIIO;

namespace renderer
{

bool oiio_make_texture(
    const char* in_filename,
    const char* out_filename,
    const char* in_colorspace,
    const char* out_depth,
    APIString&  error_msg)
{
    std::unordered_map<std::string, TypeDesc> out_depth_map;
    out_depth_map["sint8"] = TypeDesc::INT8;
    out_depth_map["uint8"] = TypeDesc::UINT8;
    out_depth_map["uint16"] = TypeDesc::UINT16;
    out_depth_map["sint16"] = TypeDesc::INT16;
    out_depth_map["half"] = TypeDesc::HALF;
    out_depth_map["float"] = TypeDesc::FLOAT;

    ImageSpec spec;

    if (strcmp(out_depth, "default") != 0)
        spec.format = out_depth_map[out_depth];

    spec.attribute("maketx:updatemode", 1);
    spec.attribute("maketx:constant_color_detect", 1);
    spec.attribute("maketx:monochrome detect", 1);
    spec.attribute("maketx:opaque detect", 1);
    spec.attribute("maketx:unpremult", 1);
    spec.attribute("maketx:incolorspace", in_colorspace);
    spec.attribute("maketx:outcolorspace", "linear");
    spec.attribute("maketx:fixnan", "box3");

    const ImageBufAlgo::MakeTextureMode mode = ImageBufAlgo::MakeTxTexture;

    std::stringstream s;
    const bool success = ImageBufAlgo::make_texture(mode, in_filename, out_filename, spec, &s);

    if (!success)
        error_msg = s.str().c_str();

    return success;
}

}
