
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "dict2dict.h"
#include "metadata.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/kernel/aov/imagestack.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/platform/python.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    auto_release_ptr<Frame> create_frame(
        const std::string&    name,
        const bpy::dict&      params)
    {
        return FrameFactory::create(name.c_str(), bpy_dict_to_param_array(params));
    }

    auto_release_ptr<Frame> create_frame_with_aovs(
        const std::string&    name,
        const bpy::dict&      params,
        const AOVContainer&   aovs,
        const AOVContainer&   lpe_aovs)
    {
        return FrameFactory::create(name.c_str(), bpy_dict_to_param_array(params), aovs, lpe_aovs);
    }

    bpy::object archive_frame(const Frame* frame, const char* directory)
    {
        char* output = nullptr;

        if (frame->archive(directory, &output))
        {
            const bpy::str path(output);
            foundation::free_string(output);
            return path;
        }

        // Return None.
        return bpy::object();
    }

    void copy_item_from_list(
        const bpy::list&    l,
        const size_t        index,
        size_t&             dst)
    {
        bpy::extract<size_t> ex(l[index]);
        if (!ex.check())
        {
            PyErr_SetString(PyExc_TypeError, "Incompatible type.");
            bpy::throw_error_already_set();
        }

        dst = ex();
    }

    void set_crop_window(Frame* frame, const bpy::list& window)
    {
        if (bpy::len(window) != 4)
        {
            PyErr_SetString(PyExc_RuntimeError, "Invalid list length given to appleseed.Frame.set_crop_window");
            bpy::throw_error_already_set();
        }

        AABB2u crop;
        copy_item_from_list(window, 0, crop.min[0]);
        copy_item_from_list(window, 1, crop.min[1]);
        copy_item_from_list(window, 2, crop.max[0]);
        copy_item_from_list(window, 3, crop.max[1]);
        frame->set_crop_window(crop);
    }

    bpy::list get_crop_window(const Frame* frame)
    {
        AABB2u window = frame->get_crop_window();
        bpy::list result;
        result.append(window.min[0]);
        result.append(window.min[1]);
        result.append(window.max[0]);
        result.append(window.max[1]);
        return result;
    }

    bpy::list get_input_metadata()
    {
        return dictionary_array_to_bpy_list(FrameFactory::get_input_metadata());
    }
}

void bind_frame()
{
    bpy::class_<Frame, auto_release_ptr<Frame>, bpy::bases<Entity>, boost::noncopyable>("Frame", bpy::no_init)
        .def("get_input_metadata", get_input_metadata).staticmethod("get_input_metadata")
        .def("__init__", bpy::make_constructor(create_frame))
        .def("__init__", bpy::make_constructor(create_frame_with_aovs))

        .def("reset_crop_window", &Frame::reset_crop_window)
        .def("has_crop_window", &Frame::has_crop_window)
        .def("set_crop_window", set_crop_window)
        .def("get_crop_window", get_crop_window)

        .def("image", &Frame::image, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("aovs", &Frame::aovs, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("post_processing_stages", &Frame::post_processing_stages, bpy::return_value_policy<bpy::reference_existing_object>())

        .def("write_main_image", &Frame::write_main_image)
        .def("write_aov_images", &Frame::write_aov_images)
        .def("write_main_and_aov_images_to_multipart_exr", &Frame::write_main_and_aov_images_to_multipart_exr)
        .def("archive", archive_frame);
}
