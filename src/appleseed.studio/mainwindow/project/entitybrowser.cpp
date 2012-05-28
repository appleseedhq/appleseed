
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "entitybrowser.h"

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"
#include "renderer/api/color.h"
#include "renderer/api/edf.h"
#include "renderer/api/environmentedf.h"
#include "renderer/api/environmentshader.h"
#include "renderer/api/material.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

namespace
{
    template <typename EntityContainer>
    StringDictionary build_entity_dictionary(const EntityContainer& entities)
    {
        StringDictionary result;

        for (const_each<EntityContainer> i = entities; i; ++i)
            result.insert(i->get_name(), i->get_name());

        return result;
    }
}

EntityBrowser<Scene>::EntityBrowser(const Scene& scene)
  : m_scene(scene)
{
}

StringDictionary EntityBrowser<Scene>::get_entities(const string& type) const
{
    if (type == "color")
    {
        return build_entity_dictionary(m_scene.colors());
    }
    else if (type == "environment_edf")
    {
        return build_entity_dictionary(m_scene.environment_edfs());
    }
    else if (type == "environment_shader")
    {
        return build_entity_dictionary(m_scene.environment_shaders());
    }
    else if (type == "texture_instance")
    {
        return build_entity_dictionary(m_scene.texture_instances());
    }
    else
    {
        return StringDictionary();
    }
}

EntityBrowser<Assembly>::EntityBrowser(const Assembly& assembly)
  : m_assembly(assembly)
{
}

StringDictionary EntityBrowser<Assembly>::get_entities(const string& type) const
{
    if (type == "bsdf")
    {
        return build_entity_dictionary(m_assembly.bsdfs());
    }
    else if (type == "color")
    {
        return build_entity_dictionary(m_assembly.colors());
    }
    else if (type == "edf")
    {
        return build_entity_dictionary(m_assembly.edfs());
    }
    else if (type == "material")
    {
        return build_entity_dictionary(m_assembly.materials());
    }
    else if (type == "surface_shader")
    {
        return build_entity_dictionary(m_assembly.surface_shaders());
    }
    else if (type == "texture_instance")
    {
        return build_entity_dictionary(m_assembly.texture_instances());
    }
    else
    {
        return StringDictionary();
    }
}

}   // namespace studio
}   // namespace appleseed
