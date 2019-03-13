
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

#pragma once

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

// Forward declarations.
namespace foundation    { class Image; }

namespace foundation
{

// Load a text file into memory.
bool load_text_file(
    const std::string&              filename,
    std::string&                    contents);

// Compare two text files on disk.
// Return true if they have the exact same content, false otherwise.
// Return false in case one of the file (or both) could not be open.
bool compare_text_files(
    const std::string&              filename1,
    const std::string&              filename2);

// Compare the contents of two images. The layout of the two images
// (the tile size and exact pixel format) are excluded from the
// comparison. Return true if they are equal (within a per-component
// epsilon), false otherwise.
bool are_images_feq(
    const Image&                    image1,
    const Image&                    image2,
    const float                     eps);

// Rescale a 2D point cloud to fit into an image.
void fit_point_cloud_to_image(
    std::vector<Vector2d>&          points);

// Write a 2D point cloud to an image file.
void write_point_cloud_image(
    const std::string&              image_path,
    const size_t                    image_width,
    const size_t                    image_height,
    const std::vector<Vector2d>&    points);

// Write a 2D point cloud to a 512x512 image file.
void write_point_cloud_image(
    const std::string&              image_path,
    const std::vector<Vector2d>&    points);

}   // namespace foundation
