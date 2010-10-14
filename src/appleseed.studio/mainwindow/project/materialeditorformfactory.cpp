
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "materialeditorformfactory.h"

// appleseed.studio headers.
#include "mainwindow/project/tools.h"

// appleseed.renderer headers.
#include "renderer/api/material.h"
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

MaterialEditorFormFactory::MaterialEditorFormFactory(const Assembly& assembly)
  : m_assembly(assembly)
{
}

void MaterialEditorFormFactory::update(
    const Dictionary&           values,
    WidgetDefinitionCollection& definitions) const
{
    definitions.clear();

    const string material_name_suggestion =
        get_name_suggestion("material", m_assembly.materials());

    definitions.push_back(
        Dictionary()
            .insert("name", "name")
            .insert("label", "Name")
            .insert("widget", "text_box")
            .insert("use", "required")
            .insert("default", material_name_suggestion)
            .insert("focus", "true"));

    definitions.push_back(
        Dictionary()
            .insert("name", "bsdf")
            .insert("label", "BSDF")
            .insert("widget", "entity_picker")
            .insert("entity_types", Dictionary().insert("bsdf", "BSDF"))
            .insert("use", "optional"));

    definitions.push_back(
        Dictionary()
            .insert("name", "edf")
            .insert("label", "EDF")
            .insert("widget", "entity_picker")
            .insert("entity_types", Dictionary().insert("edf", "EDF"))
            .insert("use", "optional"));

    definitions.push_back(
        Dictionary()
            .insert("name", "surface_shader")
            .insert("label", "Surface Shader")
            .insert("widget", "entity_picker")
            .insert(
                "entity_types",
                Dictionary().insert("surface_shader", "Surface Shaders"))
            .insert("use", "required"));
}

}   // namespace studio
}   // namespace appleseed
