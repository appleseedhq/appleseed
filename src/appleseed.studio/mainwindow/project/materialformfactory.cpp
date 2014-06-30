
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Marius Avram, The appleseedhq Organization
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
#include "materialformfactory.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/specializedarrays.h"

// appleseed.renderer headers.
#include "renderer/api/material.h"

using namespace renderer;
using namespace foundation;
using namespace std;

namespace appleseed {
namespace studio {

MaterialFormFactory::MaterialFormFactory(
    const MaterialFactoryRegistrar& factory_registrar,
    const string&                   entity_name)
    : MultiModelEntityEditorFormFactory<MaterialFactoryRegistrar>(factory_registrar, entity_name)
{

}

string MaterialFormFactory::add_model_widget_definition(
    const Dictionary&               values,
    InputMetadataCollection&        metadata) const
{
    // Add only the generic material model
    Dictionary model_items;
    auto_ptr<IMaterialFactory> material_factory(new GenericMaterialFactory());

    model_items.insert(
        material_factory->get_human_readable_model(),
        material_factory->get_model());

    const std::string model =
        get_value(
            values,
            ModelParameter,
            material_factory->get_model());

    metadata.push_back(
        Dictionary()
            .insert("name", ModelParameter)
            .insert("label", "Model")
            .insert("type", "enumeration")
            .insert("items", model_items)
            .insert("use", "required")
            .insert("default", model)
            .insert("on_change", "rebuild_form"));

    return model;
}

}
}
