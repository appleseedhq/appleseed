
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "imageattributes.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"

// Standard headers.
#include <sstream>

using namespace std;

namespace foundation
{

//
// ImageAttributes class implementation.
//

// Create a default set of image attributes.
ImageAttributes ImageAttributes::create_default_attributes()
{
    ImageAttributes attributes;

    // "software" attribute.
    stringstream software;
    software << Appleseed::get_lib_name();
    software << " version ";
    software << Appleseed::get_lib_version();
    software << " (build ";
    software << Appleseed::get_lib_build_number();
    software << ")";
    attributes.insert("software", software.str());

    // "dpi" attribute.
    attributes.insert("dpi", 72);

    return attributes;
}

}   // namespace foundation
