
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "foundation/platform/system.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// OpenEXR headers.
#include "foundation/platform/exrheaderguards.h"
BEGIN_EXR_INCLUDES
#include "OpenEXR/ImfStandardAttributes.h"
#include "OpenEXR/ImfStringAttribute.h"
#include "OpenEXR/ImfThreading.h"
END_EXR_INCLUDES

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
}

void initialize_openexr()
{
    static OpenEXRInitializer initializer;
}

void add_attributes(
    const ImageAttributes&  image_attributes,
    Header&                 header)
{
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

        else
            header.insert(attr_name.c_str(), StringAttribute(attr_value));
    }
}

}   // namespace foundation
