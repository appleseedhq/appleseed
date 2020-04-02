
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
#include "imaterialfactory.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/api/specializedapiarrays.h"

using namespace foundation;

namespace renderer
{

void IMaterialFactory::add_surface_shader_metadata(DictionaryArray& metadata)
{
    metadata.push_back(
        Dictionary()
            .insert("name", "surface_shader")
            .insert("label", "Surface Shader")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary().insert("surface_shader", "Surface Shaders"))
            .insert("use", "optional"));
}

void IMaterialFactory::add_alpha_map_metadata(DictionaryArray& metadata)
{
    metadata.push_back(
        Dictionary()
            .insert("name", "alpha_map")
            .insert("label", "Alpha Map")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "optional"));
}

void IMaterialFactory::add_displacement_metadata(DictionaryArray& metadata)
{
    metadata.push_back(
        Dictionary()
            .insert("name", "displacement_map")
            .insert("label", "Displacement Map")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "displacement_method")
            .insert("label", "Displacement Method")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Bump Mapping", "bump")
                    .insert("Normal Mapping", "normal"))
            .insert("use", "required")
            .insert("default", "bump")
            .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "bump_amplitude")
            .insert("label", "Bump Amplitude")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-1.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("visible_if",
                Dictionary()
                    .insert("displacement_method", "bump")));

    metadata.push_back(
        Dictionary()
            .insert("name", "bump_offset")
            .insert("label", "Bump Offset")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.01")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "10.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0.5")
            .insert("visible_if",
                Dictionary()
                    .insert("displacement_method", "bump")));

    metadata.push_back(
        Dictionary()
            .insert("name", "normal_map_up")
            .insert("label", "Normal Map Up Vector")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Green Channel (Y)", "y")
                    .insert("Blue Channel (Z)", "z"))
            .insert("use", "optional")
            .insert("default", "z")
            .insert("visible_if",
                Dictionary()
                    .insert("displacement_method", "normal")));
}

void IMaterialFactory::add_default_tangent_mode_metadata(DictionaryArray& metadata)
{
    metadata.push_back(
        Dictionary()
            .insert("name", "default_tangent_mode")
            .insert("label", "Default Tangent Mode")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("UV Coordinates", "uv")
                    .insert("Local X direction", "local_x")
                    .insert("Local Y direction", "local_y")
                    .insert("Local Z direction", "local_z")
                    .insert("Radial", "radial"))
            .insert("use", "optional")
            .insert("default", "uv"));
}

}   // namespace renderer
