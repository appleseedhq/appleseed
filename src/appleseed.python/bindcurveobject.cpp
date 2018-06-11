
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Girish Ramesh, The appleseedhq Organization
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
#include "bindentitycontainers.h"
#include "dict2dict.h"

// appleseed.renderer headers.
#include "renderer/api/object.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"
#include "foundation/utility/murmurhash.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <cstddef>
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;
using namespace std;

// Work around a regression in Visual Studio 2015 Update 3.
#if defined(_MSC_VER) && _MSC_VER == 1900
namespace boost
{
    template <> CurveObject const volatile* get_pointer<CurveObject const volatile>(CurveObject const volatile* p) { return p; }
}
#endif

namespace
{
    auto_release_ptr<CurveObject> create_curve_obj(
            const string&       name,
            const bpy::dict&    params)
    {
        return auto_release_ptr<CurveObject>(
                        CurveObjectFactory().create(name.c_str(), bpy_dict_to_param_array(params)));
    }


    auto_release_ptr<CurveObject> read_curve_object(
            const bpy::list&    search_paths,
            const string&       object_name,
            const bpy::dict&    params)
    {
        SearchPaths paths;

        for (bpy::ssize_t i = 0, e = bpy::len(search_paths); i < e; ++i)
        {
            bpy::extract<const char*> ex(search_paths[i]);
            if (!ex.check())
            {
                PyErr_SetString(PyExc_TypeError, "Incompatible type. Only strings.");
                bpy::throw_error_already_set();
            }

            paths.push_back_explicit_path(ex());
        }

        auto_release_ptr<CurveObject> object(CurveObjectReader::read(paths, object_name.c_str(), bpy_dict_to_param_array(params)));

        if (object.get() == nullptr)
        {
            PyErr_SetString(PyExc_RuntimeError, "appleseed.CurveObjectReader failed");
            bpy::throw_error_already_set();
        }

        return object;
    }

    bool write_curve_object(
            const CurveObject*   object,
            const string&        filename)
    {
        return CurveObjectWriter::write(*object, filename.c_str());
    }
}

void bind_curve_object()
{
    bpy::enum_<CurveBasis>("CurveBasis")
            .value("Linear", CurveBasis::Linear)
            .value("Bezier", CurveBasis::Bezier)
            .value("Bspline", CurveBasis::Bspline)
            .value("Catmullrom", CurveBasis::Catmullrom);

    bpy::class_<Curve1Type>("Curve1Type")
            .def(bpy::init<GVector3*, GScalar, GScalar, GColor3>())
            .def(bpy::init<GVector3*, GScalar*, GScalar*, GColor3*>());

    bpy::class_<Curve3Type>("Curve3Type")
            .def(bpy::init<GVector3*, GScalar, GScalar, GColor3>())
            .def(bpy::init<GVector3*, GScalar*, GScalar*, GColor3*>());


    bpy::class_<CurveObject, auto_release_ptr<CurveObject>, bpy::bases<Object>, boost::noncopyable>("CurveObject", bpy::no_init)
            .def("__init__", bpy::make_constructor(create_curve_obj))

            .def("get_basis", &CurveObject::get_basis)
            .def("push_basis", &CurveObject::push_basis)
            .def("get_curve_count", &CurveObject::get_curve_count)
            .def("push_curve_count", &CurveObject::push_curve_count)

            .def("reserve_curves1", &CurveObject::reserve_curves1)
            .def("reserve_curves3", &CurveObject::reserve_curves3)
            .def("push_curve1", &CurveObject::push_curve1)
            .def("push_curve3", &CurveObject::push_curve3)

            .def("get_curve1_count", &CurveObject::get_curve1_count)
            .def("get_curve3_count", &CurveObject::get_curve3_count)
            .def("get_curve1", &CurveObject::get_curve1, bpy::return_value_policy<bpy::reference_existing_object>())
            .def("get_curve3", &CurveObject::get_curve3, bpy::return_value_policy<bpy::reference_existing_object>());

    boost::python::implicitly_convertible<auto_release_ptr<CurveObject>, auto_release_ptr<Object>>();

    bpy::class_<CurveObjectReader>("CurveObjectReader", bpy::no_init)
            .def("read", read_curve_object).staticmethod("read");

    bpy::class_<CurveObjectWriter>("CurveObjectWriter", bpy::no_init)
            .def("write", write_curve_object).staticmethod("write");
}
