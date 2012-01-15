
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "testutils.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/drawing.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"

// Standard headers.
#include <fstream>
#include <sstream>

using namespace std;

namespace foundation
{

bool load_text_file(const string& filename, string& contents)
{
    ifstream file(filename.c_str());

    if (!file.is_open())
        return false;

    stringstream sstr;
    sstr << file.rdbuf();
    contents = sstr.str();

    return true;
}

bool compare_text_files(const string& filename1, const string& filename2)
{
    string contents1;

    if (!load_text_file(filename1, contents1))
        return false;

    string contents2;
    
    if (!load_text_file(filename2, contents2))
        return false;

    return contents1 == contents2;
}

void write_point_cloud_image(
    const string&               image_path,
    const size_t                image_width,
    const size_t                image_height,
    const vector<Vector2d>&     points)
{
    Image image(
        image_width,
        image_height,
        32,
        32,
        3,
        PixelFormatFloat);

    image.clear(Color3f(0.0f));

    for (size_t i = 0; i < points.size(); ++i)
        Drawing::draw_dot(image, points[i], Color3f(1.0f));

    GenericImageFileWriter().write(image_path.c_str(), image);
}

void write_point_cloud_image(
    const string&               image_path,
    const vector<Vector2d>&     points)
{
    write_point_cloud_image(
        image_path,
        512,
        512,
        points);
}

}   // namespace foundation
