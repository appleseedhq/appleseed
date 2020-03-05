
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/hash/murmurhash.h"
#include "foundation/platform/python.h"

// Standard headers.
#include <string>

namespace bpy = boost::python;
using namespace foundation;

void bind_murmurhash()
{
    bpy::class_<MurmurHash>("MurmurHash")
        .def(bpy::init<>())
        .def("append", (MurmurHash& (MurmurHash::*)(const float&)) &MurmurHash::append, bpy::return_self<>())
        .def("append", (MurmurHash& (MurmurHash::*)(const double&)) &MurmurHash::append, bpy::return_self<>())
        .def("append", (MurmurHash& (MurmurHash::*)(const int&)) &MurmurHash::append, bpy::return_self<>())
        .def("append", (MurmurHash& (MurmurHash::*)(const char*)) &MurmurHash::append, bpy::return_self<>())
        .def("append", (MurmurHash& (MurmurHash::*)(const std::string&)) &MurmurHash::append, bpy::return_self<>())
        .add_property("h1", &MurmurHash::h1)
        .add_property("h2", &MurmurHash::h2)
        .def(bpy::self == bpy::self)
        .def(bpy::self != bpy::self)
        .def(bpy::self <  bpy::self);
}
