
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
#include "pngimagefilewriter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/icanvas.h"
#include "foundation/image/oiioimagefilewriter.h"
#include "foundation/utility/foreach.h"

// std headers.
#include <cstring>
#include <cstdint>
#include <memory>
#include <string>

namespace foundation
{

void PNGImageFileWriter::write(
    const char*             filename,
    const ICanvas&          image,
    const ImageAttributes&  image_attributes)
{
    assert(filename);

    // Creates the writer.
    m_writer.create(filename);
    
    // Sets the image to write.
    m_writer.append_image(&image);

    // Sets file headers attributes and specifications.
    m_writer.set_image_spec(image.properties(), PixelFormat::PixelFormatUInt8);
    set_image_attributes(image_attributes);

    // Writes the image.
    m_writer.write();

    // Destroy the writer.
    m_writer.destroy();
}

// See OpenImageIO reference documentation for an exhaustive attribute names list.
// https://github.com/OpenImageIO/oiio/blob/master/src/doc/openimageio.pdf
//
void PNGImageFileWriter::set_image_attributes(const ImageAttributes& image_attributes)
{
    for (const_each<ImageAttributes> i = image_attributes; i; ++i)
    {
        // Fetch the name and the value of the attribute.
        const std::string attr_name = i->key();
        const std::string attr_value = i->value<std::string>();

        if (attr_name == "title")
            m_writer.set_image_attribute("DocumentName", attr_value.c_str());

        else if (attr_name == "author")
            m_writer.set_image_attribute("Artist", attr_value.c_str());

        else if (attr_name == "description")
            m_writer.set_image_attribute("ImageDescription", attr_value.c_str());

        else if (attr_name == "copyright")
            m_writer.set_image_attribute("Copyright", attr_value.c_str());

        else if (attr_name == "creation_time")
            m_writer.set_image_attribute("DateTime", attr_value.c_str());

        else if (attr_name == "software")
            m_writer.set_image_attribute("Software", attr_value.c_str());

        else if (attr_name == "disclaimer")
            m_writer.set_image_attribute("Disclaimer", attr_value.c_str());

        else if (attr_name == "warning")
            m_writer.set_image_attribute("Warning", attr_value.c_str());

        else if (attr_name == "source")
            m_writer.set_image_attribute("Source", attr_value.c_str());

        else if (attr_name == "comment")
            m_writer.set_image_attribute("Comment", attr_value.c_str());

        else if (attr_name == "dpi")
        {
            const size_t dpi = from_string<size_t>(attr_value);
            const double dpm = dpi * (100.0 / 2.54);
            const char* dpm_str = to_string<double>(dpm).c_str();
            m_writer.set_image_attribute("XResolution", dpm_str);
            m_writer.set_image_attribute("YResolution", dpm_str);
            m_writer.set_image_attribute("ResolutionUnit", "cm");
        }

        else
            m_writer.set_image_attribute(attr_name.c_str(), attr_value.c_str());
    }
}

}	// namespace foundation
