
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_PYTHON_DICT2DICT_H
#define APPLESEED_PYTHON_DICT2DICT_H

// appleseed.foundation headers.
#include "foundation/platform/python.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace renderer      { class ParamArray; }

foundation::Dictionary bpy_dict_to_dictionary(const boost::python::dict& d);
boost::python::dict dictionary_to_bpy_dict(const foundation::Dictionary& dict);

renderer::ParamArray bpy_dict_to_param_array(const boost::python::dict& d);
boost::python::dict param_array_to_bpy_dict(const renderer::ParamArray& array);

// Convert a DictionaryArray into a Python dictionary.
// Use key, which must be present in all dictionaries in the array
// as the key for the Python dictionary.
boost::python::dict dictionary_array_to_bpy_dict(
    const foundation::DictionaryArray&  array,
    const char*                         key);

#endif  // !APPLESEED_PYTHON_DICT2DICT_H
