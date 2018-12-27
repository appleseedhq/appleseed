
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/global/globallogger.h"
#include "renderer/kernel/aov/imagestack.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/imageattributes.h"

// Standard headers.
#include <exception>

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
  , m_image_index(~size_t(0))
{
    set_name(name);
}

void AOV::release()
{
    delete this;
}

Image& AOV::get_image() const
{
    return *m_image;
}

void AOV::post_process_image(const Frame& frame)
{
}

void AOV::create_image(
    const size_t        canvas_width,
    const size_t        canvas_height,
    const size_t        tile_width,
    const size_t        tile_height,
    ImageStack&         aov_images)
{
    m_image_index = aov_images.append(
        get_name(),
        get_channel_count(),
        PixelFormatFloat);

    m_image = &aov_images.get_image(m_image_index);
}

bool AOV::write_images(const char* file_path) const
{
    try
    {
        GenericImageFileWriter writer(file_path);

        writer.append_image(&get_image());

        if (has_color_data())
            writer.set_image_output_format(PixelFormatHalf);

        writer.set_image_channels(get_channel_count(), get_channel_names());

        ImageAttributes image_attributes = ImageAttributes::create_default_attributes();
        image_attributes.insert("color_space", "linear");
        writer.set_image_attributes(image_attributes);

        writer.write();
    }
    catch (const std::exception& e)
    {
        RENDERER_LOG_ERROR(
            "failed to write image file %s: %s.",
            file_path,
            e.what());

        return false;
    }

    return true;
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
    return 4;
}

const char** ColorAOV::get_channel_names() const
{
    static const char* ChannelNames[] = {"R", "G", "B", "A"};
    return ChannelNames;
}

bool ColorAOV::has_color_data() const
{
    return true;
}

void ColorAOV::clear_image()
{
    m_image->clear(Color4f(0.0f, 0.0f, 0.0f, 0.0f));
}


//
// UnfilteredAOV class implementation.
//

UnfilteredAOV::UnfilteredAOV(const char* name, const ParamArray& params)
  : AOV(name, params)
{
}

UnfilteredAOV::~UnfilteredAOV()
{
    delete m_image;
}

size_t UnfilteredAOV::get_channel_count() const
{
    return 3;
}

const char** UnfilteredAOV::get_channel_names() const
{
    static const char* ChannelNames[] = {"R", "G", "B"};
    return ChannelNames;
}

bool UnfilteredAOV::has_color_data() const
{
    return false;
}

void UnfilteredAOV::clear_image()
{
    m_image->clear(Color3f(0.0f, 0.0f, 0.0f));
}

void UnfilteredAOV::create_image(
    const size_t        canvas_width,
    const size_t        canvas_height,
    const size_t        tile_width,
    const size_t        tile_height,
    ImageStack&         aov_images)
{
    m_image =
        new Image(
            canvas_width,
            canvas_height,
            tile_width,
            tile_height,
            get_channel_count(),
            PixelFormatFloat);

    // We need to clear the image because the default channel value might not be zero.
    clear_image();
}

}   // namespace renderer
