
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
#include "settingsfilewriter.h"

// appleseed.foundation headers.
#include "foundation/utility/indenter.h"
#include "foundation/utility/xmlelement.h"

// Standard headers.
#include <cstdio>

namespace foundation
{

bool SettingsFileWriter::write(
    const char*         filename,
    const Dictionary&   settings)
{
    FILE* file = fopen(filename, "wt");

    if (!file)
        return false;

    fprintf(file, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");

    Indenter indenter(4);

    XMLElement settings_element("settings", file, indenter);
    settings_element.write(XMLElement::HasChildElements);

    write_dictionary(settings, file, indenter);

    settings_element.close();

    fclose(file);

    return true;
}

}   // namespace foundation
