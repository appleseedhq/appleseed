
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

// Interface header.
#include "exrutils.h"

// appleseed.foundation headers.
#include "foundation/image/imageattributes.h"
#include "foundation/math/vector.h"
#include "foundation/platform/system.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/string.h"

// OpenEXR headers.
#include "foundation/platform/_beginexrheaders.h"
#include "OpenEXR/ImfChromaticities.h"
#include "OpenEXR/ImfChromaticitiesAttribute.h"
#include "OpenEXR/ImfStandardAttributes.h"
#include "OpenEXR/ImfStringAttribute.h"
#include "OpenEXR/ImfThreading.h"
#include "foundation/platform/_endexrheaders.h"

// Standard headers.
#include <string>

using namespace Imf;
using namespace std;

namespace foundation
{

namespace
{
    struct OpenEXRInitializer
    {
        OpenEXRInitializer()
        {
            setGlobalThreadCount(
                static_cast<int>(System::get_logical_cpu_core_count()));
        }
    };

    void add_attribute(Header& header, const string& attr_name, const string& attr_value)
    {
        // Try to guess the type of the value represented by the string.

        try // Vector3
        {
            const Vector3d v = from_string<Vector3d>(attr_value);
            header.insert(attr_name.c_str(), V3dAttribute(Imath::V3d(v[0], v[1], v[2])));
            return;
        }
        catch (ExceptionStringConversionError&) {}

        try // Vector2
        {
            const Vector2d v = from_string<Vector2d>(attr_value);
            header.insert(attr_name.c_str(), V2dAttribute(Imath::V2d(v[0], v[1])));
            return;
        }
        catch (ExceptionStringConversionError&) {}

        try // int / double / float
        {
            const float f = from_string<float>(attr_value);
            header.insert(attr_name.c_str(), FloatAttribute(f));
            return;
        }
        catch (ExceptionStringConversionError&) {}

        // todo: check more types here if needed...

        // As a fallback, use a string.
        header.insert(attr_name.c_str(), StringAttribute(attr_value));
    }
}

void initialize_openexr()
{
    static OpenEXRInitializer initializer;
}

void add_attributes(
    const ImageAttributes&  image_attributes,
    Header&                 header)
{
    size_t num_chromaticity_coordinates = 0;
    Imf::Chromaticities chromaticities;

    for (const_each<ImageAttributes> i = image_attributes; i; ++i)
    {
        // Fetch the name and the value of the attribute.
        const string attr_name = i->key();
        const string attr_value = i->value<string>();

        if (attr_name == "dpi")
            addXDensity(header, from_string<float>(attr_value));

        else if (attr_name == "author")
            addOwner(header, attr_value);

        else if (attr_name == "comment")
            addComments(header, attr_value);

        else if (attr_name == "creation_time")
            addCapDate(header, attr_value);

        else if (attr_name == "chromaticity_wxy")
        {
            const Vector2f c = from_string<Vector2f>(attr_value);
            chromaticities.white = Imath::V2f(c[0], c[1]);
            ++num_chromaticity_coordinates;
        }
        else if (attr_name == "chromaticity_rxy")
        {
            const Vector2f c = from_string<Vector2f>(attr_value);
            chromaticities.red = Imath::V2f(c[0], c[1]);
            ++num_chromaticity_coordinates;
        }
        else if (attr_name == "chromaticity_gxy")
        {
            const Vector2f c = from_string<Vector2f>(attr_value);
            chromaticities.green = Imath::V2f(c[0], c[1]);
            ++num_chromaticity_coordinates;
        }
        else if (attr_name == "chromaticity_bxy")
        {
            const Vector2f c = from_string<Vector2f>(attr_value);
            chromaticities.blue = Imath::V2f(c[0], c[1]);
            ++num_chromaticity_coordinates;
        }
        else
            add_attribute(header, attr_name, attr_value);
    }

    // Only save chromaticities if they are complete.
    if (num_chromaticity_coordinates == 4)
        header.insert("chromaticities", Imf::ChromaticitiesAttribute(chromaticities));
}

}   // namespace foundation
