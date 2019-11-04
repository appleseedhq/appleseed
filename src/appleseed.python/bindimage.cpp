
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

// appleseed.renderer headers.
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/tilestack.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/platform/python.h"

// Standard headers.
#include <cstddef>
#include <cstring>
#include <memory>
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

    Image* copy_image(const Image* source)
    {
        return new Image(*source);
    }

    Image* deepcopy_image(const Image* source, bpy::dict& memo)
    {
        // todo: not sure what to store in memo...
        return new Image(*source);
    }

    const char* python_array_code(PixelFormat format)
    {
        switch (format)
        {
          case PixelFormatUInt8:
            return "B";

          case PixelFormatUInt16:
            return "H";

          case PixelFormatUInt32:
            return "I";

          case PixelFormatHalf:
            return "B";

          case PixelFormatFloat:
            return "f";

          case PixelFormatDouble:
            return "d";

          default:
            assert(false);
            return nullptr;
        }
    }

    // Copied from OpenImageIO's python bindings.
    bpy::object c_array_to_py_array(const char* data, PixelFormat format, size_t size)
    {
        // Figure out what kind of array to return and create it
        bpy::object arr_module(bpy::handle<>(PyImport_ImportModule("array")));
        bpy::object array = arr_module.attr("array")(python_array_code(format));

        // Create a Python byte array (or string for Python2) to hold the data.
#if PY_MAJOR_VERSION >= 3
        bpy::object string_py(bpy::handle<>(PyBytes_FromStringAndSize(data, size)));
#else
        bpy::object string_py(bpy::handle<>(PyString_FromStringAndSize(data, size)));
#endif

        // Copy the data from the string to the array, then return the array.
#if (PY_MAJOR_VERSION < 3) || (PY_MAJOR_VERSION == 3 && PY_MINOR_VERSION < 2)
        array.attr("fromstring")(string_py);
#else
        array.attr("frombytes")(string_py);
#endif
        return array;
    }

    bpy::object tile_get_storage(const Tile* tile)
    {
        // Allocate our own temp buffer and copy the pixels.
        std::unique_ptr<char[]> data (new char[tile->get_size()]);
        std::memcpy(data.get(), tile->get_storage(), tile->get_size());

        return c_array_to_py_array(data.get(), tile->get_pixel_format(), tile->get_size());
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
        .value("Double", PixelFormatDouble);

    bpy::class_<CanvasProperties, boost::noncopyable>("CanvasProperties", bpy::no_init)
        .add_property("m_canvas_width", &CanvasProperties::m_canvas_width)
        .add_property("m_canvas_height", &CanvasProperties::m_canvas_height)
        .add_property("m_tile_width", &CanvasProperties::m_tile_width)
        .add_property("m_tile_height", &CanvasProperties::m_tile_height)
        .add_property("m_channel_count", &CanvasProperties::m_channel_count)
        .add_property("m_pixel_format", &CanvasProperties::m_pixel_format)
        .add_property("m_rcp_canvas_width", &CanvasProperties::m_rcp_canvas_width)
        .add_property("m_rcp_canvas_height", &CanvasProperties::m_rcp_canvas_height)
        .add_property("m_rcp_tile_width", &CanvasProperties::m_rcp_tile_width)
        .add_property("m_rcp_tile_height", &CanvasProperties::m_rcp_tile_height)
        .add_property("m_tile_count_x", &CanvasProperties::m_tile_count_x)
        .add_property("m_tile_count_y", &CanvasProperties::m_tile_count_y)
        .add_property("m_tile_count", &CanvasProperties::m_tile_count)
        .add_property("m_pixel_count", &CanvasProperties::m_pixel_count)
        .add_property("m_pixel_size", &CanvasProperties::m_pixel_size)
        .def("get_tile_width", &CanvasProperties::get_tile_width)
        .def("get_tile_height", &CanvasProperties::get_tile_height);

    bpy::class_<Tile, boost::noncopyable>("Tile", bpy::init<size_t, size_t, size_t, PixelFormat>())
        .def("__copy__", copy_tile, bpy::return_value_policy<bpy::manage_new_object>())
        .def("__deepcopy__", deepcopy_tile, bpy::return_value_policy<bpy::manage_new_object>())
        .def("get_pixel_format", &Tile::get_pixel_format)
        .def("get_width", &Tile::get_width)
        .def("get_height", &Tile::get_height)
        .def("get_channel_count", &Tile::get_channel_count)
        .def("get_pixel_count", &Tile::get_pixel_count)
        .def("get_size", &Tile::get_size)
        .def("get_storage", tile_get_storage);

    const Tile& (Image::*image_get_tile)(const size_t, const size_t) const = &Image::tile;

    bpy::class_<Image, boost::noncopyable>("Image", bpy::no_init)
        .def("__copy__", copy_image, bpy::return_value_policy<bpy::manage_new_object>())
        .def("__deepcopy__", copy_image, bpy::return_value_policy<bpy::manage_new_object>())
        .def("properties", &Image::properties, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("tile", image_get_tile, bpy::return_value_policy<bpy::reference_existing_object>());

    const Image& (ImageStack::*image_stack_get_image)(const size_t) const = &ImageStack::get_image;

    bpy::class_<ImageStack, boost::noncopyable>("ImageStack", bpy::no_init)
        .def("empty", &ImageStack::empty)
        .def("size", &ImageStack::size)
        .def("get_name", image_stack_get_name)
        .def("get_image", image_stack_get_image, bpy::return_value_policy<bpy::reference_existing_object>());
}
