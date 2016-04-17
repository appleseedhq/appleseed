
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/tilestack.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    Tile* copy_tile(const Tile* source)
    {
        return new Tile(*source);
    }

    Tile* deepcopy_tile(const Tile* source, bpy::dict& memo)
    {
        // todo: not sure what to store in memo...
        return new Tile(*source);
    }

    void copy_tile_data_to_py_buffer(const Tile& tile, bpy::object& buffer)
    {
        void* array = 0;
        Py_ssize_t len;

        const int success = PyObject_AsWriteBuffer(buffer.ptr(), &array, &len);

        if (success != 0)
            bpy::throw_error_already_set();

        if (static_cast<size_t>(len) < tile.get_size())
        {
            PyErr_SetString(PyExc_IndexError, "Buffer size is smaller than data size");
            bpy::throw_error_already_set();
        }

        std::copy(
            tile.get_storage(),
            tile.get_storage() + tile.get_size(),
            reinterpret_cast<uint8*>(array));
    }

    bpy::list blender_tile_data(const Tile& tile)
    {
        bpy::list pixels;

        for (size_t y = 0, height = tile.get_height(); y < height; ++y)
        {
            for (size_t x = 0, width = tile.get_width(); x < width; ++x)
            {
                // Blender's image coordinate system is y up.
                Color4f linear_rgba;
                tile.get_pixel(x, height - y - 1, linear_rgba);

                bpy::list p;
                p.append(linear_rgba[0]);
                p.append(linear_rgba[1]);
                p.append(linear_rgba[2]);
                p.append(linear_rgba[3]);
                pixels.append(p);
            }
        }

        return pixels;
    }

    Image* copy_image(const Image* source)
    {
        return new Image(*source);
    }

    Image* deepcopy_image(const Image* source, bpy::dict& memo)
    {
        // todo: not sure what to store in memo...
        return new Image(*source);
    }

    std::string image_stack_get_name(const ImageStack* image_stack, const size_t index)
    {
        return image_stack->get_name(index);
    }
}

void bind_image()
{
    bpy::enum_<PixelFormat>("PixelFormat")
        .value("UInt8",  PixelFormatUInt8)
        .value("UInt16", PixelFormatUInt16)
        .value("UInt32", PixelFormatUInt32)
        .value("Half",   PixelFormatHalf)
        .value("Float",  PixelFormatFloat)
        .value("Double", PixelFormatDouble)
        ;

    bpy::class_<CanvasProperties, boost::noncopyable>("CanvasProperties", bpy::no_init)
        .def_readonly("canvas_width", &CanvasProperties::m_canvas_width)
        .def_readonly("canvas_height", &CanvasProperties::m_canvas_height)
        .def_readonly("tile_width", &CanvasProperties::m_tile_width)
        .def_readonly("tile_height", &CanvasProperties::m_tile_height)
        .def_readonly("channel_count", &CanvasProperties::m_channel_count)
        .def_readonly("pixel_format", &CanvasProperties::m_pixel_format)
        .def_readonly("rcp_canvas_width", &CanvasProperties::m_rcp_canvas_width)
        .def_readonly("rcp_canvas_height", &CanvasProperties::m_rcp_canvas_height)
        .def_readonly("rcp_tile_width", &CanvasProperties::m_rcp_tile_width)
        .def_readonly("rcp_tile_height", &CanvasProperties::m_rcp_tile_height)
        .def_readonly("tile_count_x", &CanvasProperties::m_tile_count_x)
        .def_readonly("tile_count_y", &CanvasProperties::m_tile_count_y)
        .def_readonly("tile_count", &CanvasProperties::m_tile_count)
        .def_readonly("pixel_count", &CanvasProperties::m_pixel_count)
        .def_readonly("pixel_size", &CanvasProperties::m_pixel_size)
        .def("get_tile_width", &CanvasProperties::get_tile_width)
        .def("get_tile_height", &CanvasProperties::get_tile_height)
        ;

    bpy::class_<Tile, boost::noncopyable>("Tile", bpy::init<size_t, size_t, size_t, PixelFormat>())
        .def("__copy__", copy_tile, bpy::return_value_policy<bpy::manage_new_object>())
        .def("__deepcopy__", deepcopy_tile, bpy::return_value_policy<bpy::manage_new_object>())
        .def("get_pixel_format", &Tile::get_pixel_format)
        .def("get_width", &Tile::get_width)
        .def("get_height", &Tile::get_height)
        .def("get_channel_count", &Tile::get_channel_count)
        .def("get_pixel_count", &Tile::get_pixel_count)
        .def("get_size", &Tile::get_size)
        .def("copy_data_to", copy_tile_data_to_py_buffer)   // todo: maybe this needs a better name

        .def("blender_tile_data", blender_tile_data)
        ;

    const Tile& (Image::*image_get_tile)(const size_t, const size_t) const = &Image::tile;

    bpy::class_<Image, boost::noncopyable>("Image", bpy::no_init)
        .def("__copy__", copy_image, bpy::return_value_policy<bpy::manage_new_object>())
        .def("__deepcopy__", copy_image, bpy::return_value_policy<bpy::manage_new_object>())
        .def("properties", &Image::properties, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("tile", image_get_tile, bpy::return_value_policy<bpy::reference_existing_object>())
        ;

    const Image& (ImageStack::*image_stack_get_image)(const size_t) const = &ImageStack::get_image;

    bpy::class_<ImageStack, boost::noncopyable>("ImageStack", bpy::no_init)
        .def("empty", &ImageStack::empty)
        .def("size", &ImageStack::size)
        .def("get_name", image_stack_get_name)
        .def("get_image", image_stack_get_image, bpy::return_value_policy<bpy::reference_existing_object>())
        ;
}
