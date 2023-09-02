
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

// Interface header.
#include "ilightfactory.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/api/specializedapiarrays.h"

using namespace foundation;

namespace renderer
{

void ILightFactory::add_common_input_metadata(DictionaryArray& metadata)
{
    metadata.push_back(
        Dictionary()
            .insert("name", "cast_indirect_light")
            .insert("label", "Cast Indirect Light")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "true")
            .insert("help", "If enabled, this light contributes to indirect lighting"));

    metadata.push_back(
        Dictionary()
            .insert("name", "importance_multiplier")
            .insert("label", "Importance Multiplier")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "10.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Adjust the sampling effort for this light with respect to the other lights"));

    metadata.push_back(
        Dictionary()
            .insert("name", "cast_shadows")
            .insert("label", "Cast Shadows")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "true")
            .insert("help", "If enabled, this light casts shadows"));
}

void ILightFactory::add_common_sun_input_metadata(foundation::DictionaryArray& metadata)
{
    metadata.push_back(
        Dictionary()
        .insert("name", "environment_edf")
        .insert("label", "Bind To")
        .insert("type", "entity")
        .insert("entity_types",
            Dictionary().insert("environment_edf", "Environment EDFs"))
        .insert("use", "optional")
        .insert("help", "If an environment EDF is bound, use the sun angles and turbidity values from the environment"));

    metadata.push_back(
        Dictionary()
        .insert("name", "visible")
        .insert("label", "Visible")
        .insert("type", "boolean")
        .insert("use", "optional")
        .insert("default", "true")
        .insert("help", "Make the sun visible to the camera"));

    metadata.push_back(
        Dictionary()
        .insert("name", "turbidity")
        .insert("label", "Turbidity")
        .insert("type", "numeric")
        .insert("min",
            Dictionary()
            .insert("value", "1.0")
            .insert("type", "hard"))
        .insert("max",
            Dictionary()
            .insert("value", "10.0")
            .insert("type", "hard"))
        .insert("use", "required")
        .insert("default", "2.0")
        .insert("help", "Atmospheric haziness"));

    metadata.push_back(
        Dictionary()
        .insert("name", "radiance_multiplier")
        .insert("label", "Radiance Multiplier")
        .insert("type", "numeric")
        .insert("min",
            Dictionary()
            .insert("value", "0.0")
            .insert("type", "hard"))
        .insert("max",
            Dictionary()
            .insert("value", "10.0")
            .insert("type", "soft"))
        .insert("use", "optional")
        .insert("default", "1.0")
        .insert("help", "Light intensity multiplier"));

    metadata.push_back(
        Dictionary()
        .insert("name", "size_multiplier")
        .insert("label", "Size Multiplier")
        .insert("type", "numeric")
        .insert("min",
            Dictionary()
            .insert("value", "0.0")
            .insert("type", "hard"))
        .insert("max",
            Dictionary()
            .insert("value", "100.0")
            .insert("type", "soft"))
        .insert("use", "optional")
        .insert("default", "1.0")
        .insert("help", "The size multiplier allows to make the sun bigger or smaller, hence making it cast softer or harder shadows"));

    metadata.push_back(
        Dictionary()
        .insert("name", "distance")
        .insert("label", "Distance")
        .insert("type", "numeric")
        .insert("min",
            Dictionary()
            .insert("value", "0.0")
            .insert("type", "hard"))
        .insert("max",
            Dictionary()
            .insert("value", "500.0")
            .insert("type", "soft"))
        .insert("use", "optional")
        .insert("default", "149.6")
        .insert("help", "Distance between Sun and scene (millions of km)"));
}

}   // namespace renderer
