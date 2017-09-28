
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "aov.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/imagestack.h"

using namespace foundation;

namespace renderer
{

//
// AOV class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID AOV::get_class_uid()
{
    return g_class_uid;
}

AOV::AOV(
    const char*         name,
    const ParamArray&   params)
  : Entity(g_class_uid, params)
  , m_image(nullptr)
{
    set_name(name);
}

void AOV::release()
{
    delete this;
}

void AOV::create_image(
    const size_t    canvas_width,
    const size_t    canvas_height,
    const size_t    tile_width,
    const size_t    tile_height,
    ImageStack&     aov_images)
{
    const size_t index = aov_images.append(
        get_name(),
        4, // TODO: check if we can pass aov->get_channel_count() here.
        PixelFormatFloat);
    m_image = &aov_images.get_image(index);
}

Image& AOV::get_image() const
{
    return *m_image;
}


//
// ColorAOV class implementation.
//

ColorAOV::ColorAOV(const char* name, const ParamArray& params)
  : AOV(name, params)
{
}

size_t ColorAOV::get_channel_count() const
{
    return 3;
}

const char** ColorAOV::get_channel_names() const
{
    static const char* ChannelNames[] = {"R", "G", "B"};
    return ChannelNames;
}

bool ColorAOV::has_color_data() const
{
    return true;
}

}   // namespace renderer
