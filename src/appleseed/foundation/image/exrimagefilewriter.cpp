
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

// Interface header.
#include "exrimagefilewriter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/exceptionunsupportedimageformat.h"
#include "foundation/image/icanvas.h"
#include "foundation/image/oiioimagefilewriter.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

using namespace std;

namespace foundation
{

//
// EXRImageFileWriter class implementation.
//

void EXRImageFileWriter::write(
    const char*             filename,
    const ICanvas&          image,
    const ImageAttributes&  image_attributes)
{
    static const char* ChannelNames[] = {"R", "G", "B", "A"};
    const CanvasProperties& props = image.properties();
    write(filename, image, image_attributes, props.m_channel_count, ChannelNames);
}

void EXRImageFileWriter::write(
    const char*             filename,
    const ICanvas&          image,
    const ImageAttributes&  image_attributes,
    const size_t            channel_count,
    const char**            channel_names)
{
    assert(filename);
    assert(channel_names);

    // Create the writer.
    m_writer.create(filename);

    // Sets the image to write.
    m_writer.append_image(&image);

    // Sets file headers attributes and specifications.
    m_writer.set_image_spec(image.properties(), channel_count, channel_names, image.properties().m_pixel_format);
    set_image_attributes(image_attributes);

    // Writes the image.
    m_writer.write();

    // Destroy the writer.
    m_writer.destroy();
}

void EXRImageFileWriter::begin_multipart_exr(const char* filename)
{
    // Create the writer.
    m_writer.create(filename);
}

void EXRImageFileWriter::append_part(
    const char*             part_name,
    const ICanvas&          image,
    const ImageAttributes&  image_attributes,
    const size_t            channel_count,
    const char**            channel_names)
{
    assert(part_name);
    assert(channel_names);

    // Adds an image to the writer.
    m_writer.append_image(&image);

    // Sets file headers attributes and specifications.
    m_writer.set_image_spec(image.properties(), channel_count, channel_names, image.properties().m_pixel_format);
    set_image_attributes(image_attributes);

    // Stores the part name as attribute of the image.
    m_writer.set_image_attribute("Part name", part_name);
}

void EXRImageFileWriter::write_multipart_exr()
{
    // Writes the image.
    m_writer.write();

    // Destroy the writer.
    m_writer.destroy();
}

// See OpenImageIO reference documentation for an exhaustive attribute names list.
// https://github.com/OpenImageIO/oiio/blob/master/src/doc/openimageio.pdf
//
void EXRImageFileWriter::set_image_attributes(const ImageAttributes& image_attributes)
{
    for (const_each<ImageAttributes> i = image_attributes; i; ++i)
    {
        // Fetch the name and the value of the attribute.
        const std::string attr_name = i->key();
        const std::string attr_value = i->value<std::string>();

        if (attr_name == "author")
            m_writer.set_image_attribute("Copyright", attr_value.c_str());

        else if (attr_name == "comment")
            m_writer.set_image_attribute("ImageDescription", attr_value.c_str());

        else if (attr_name == "creation_time")
            m_writer.set_image_attribute("DateTime", attr_value.c_str());

        else
            m_writer.set_image_attribute(attr_name.c_str(), attr_value.c_str());
    }
}

}   // namespace foundation
