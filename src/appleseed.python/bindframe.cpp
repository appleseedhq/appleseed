
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.python headers.
#include "pyseed.h" // has to be first, to avoid redefinition warnings
#include "dict2dict.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/kernel/aov/imagestack.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/image/tile.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace
{
    auto_release_ptr<Frame> create_frame(
        const string&       name,
        const bpy::dict&    params)
    {
        return FrameFactory::create(name.c_str(), bpy_dict_to_param_array(params));
    }

    void transform_tile_to_output_color_space(const Frame* frame, Tile* tile)
    {
        frame->transform_to_output_color_space(*tile);
    }

    void transform_image_to_output_color_space(const Frame* frame, Image* image)
    {
        frame->transform_to_output_color_space(*image);
    }

    bpy::object archive_frame(const Frame* frame, const char* directory)
    {
        char* output = 0;

        if (frame->archive(directory, &output))
        {
            const bpy::str path(output);
            foundation::free_string(output);
            return path;
        }

        // Return None.
        return bpy::object();
    }
}

void bind_frame()
{
    bpy::class_<Frame, auto_release_ptr<Frame>, bpy::bases<Entity>, boost::noncopyable>("Frame", bpy::no_init)
        .def("__init__", bpy::make_constructor(create_frame))

        .def("image", &Frame::image, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("aov_images", &Frame::aov_images, bpy::return_value_policy<bpy::reference_existing_object>())

        .def("transform_tile_to_output_color_space", transform_tile_to_output_color_space)
        .def("transform_image_to_output_color_space", transform_image_to_output_color_space)

        .def("clear_main_image", &Frame::clear_main_image)
        .def("write_main_image", &Frame::write_main_image)
        .def("write_aov_images", &Frame::write_aov_images)
        .def("archive", archive_frame)
        ;
}
