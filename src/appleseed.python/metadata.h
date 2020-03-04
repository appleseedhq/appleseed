
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

#pragma once

// appleseed.python headers.
#include "dict2dict.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/platform/python.h"
#include "foundation/utility/api/specializedapiarrays.h"

namespace detail
{
    template <typename FactoryRegistrar>
    boost::python::dict get_entity_model_metadata()
    {
        FactoryRegistrar registrar;
        const typename FactoryRegistrar::FactoryArrayType factories = registrar.get_factories();

        boost::python::dict model_metadata;

        for (size_t i = 0, e = factories.size(); i < e; ++i)
        {
            model_metadata[factories[i]->get_model()] =
                dictionary_to_bpy_dict(factories[i]->get_model_metadata());
        }

        return model_metadata;
    }

    template <typename FactoryRegistrar>
    boost::python::dict get_entity_input_metadata()
    {
        FactoryRegistrar registrar;
        const typename FactoryRegistrar::FactoryArrayType factories = registrar.get_factories();

        boost::python::dict input_metadata;

        for (size_t i = 0, e = factories.size(); i < e; ++i)
        {
            input_metadata[factories[i]->get_model()] =
                dictionary_array_to_bpy_dict(factories[i]->get_input_metadata(), "name");
        }

        return input_metadata;
    }
}
