
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
#include "entitybrowser.h"

// appleseed.renderer headers.
#include "renderer/api/aov.h"
#include "renderer/api/bsdf.h"
#include "renderer/api/bssrdf.h"
#include "renderer/api/camera.h"
#include "renderer/api/color.h"
#include "renderer/api/edf.h"
#include "renderer/api/environmentedf.h"
#include "renderer/api/environmentshader.h"
#include "renderer/api/frame.h"
#include "renderer/api/material.h"
#include "renderer/api/postprocessing.h"
#include "renderer/api/scene.h"
#include "renderer/api/shadergroup.h"
#include "renderer/api/surfaceshader.h"
#include "renderer/api/volume.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cassert>

using namespace foundation;
using namespace renderer;

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

    void merge_dictionary(StringDictionary& dest, const StringDictionary& other)
    {
        for (const_each<StringDictionary> i = other; i; ++i)
            dest.insert(i->key(), i->value());
    }

    const Scene* get_parent_scene(const Entity* entity)
    {
        assert(entity);

        while (dynamic_cast<const Scene*>(entity) == nullptr)
            entity = entity->get_parent();

        return static_cast<const Scene*>(entity);
    }
}


//
// EntityBrowser<Project> class implementation.
//

EntityBrowser<Project>::EntityBrowser(const Project& project)
{
}

StringDictionary EntityBrowser<Project>::get_entities(const std::string& type) const
{
    return StringDictionary();
}


//
// EntityBrowser<BaseGroup> class implementation.
//

EntityBrowser<BaseGroup>::EntityBrowser(const BaseGroup& base_group)
  : m_base_group(base_group)
{
}

StringDictionary EntityBrowser<BaseGroup>::get_entities(const std::string& type) const
{
    return
        type == "color" ? build_entity_dictionary(m_base_group.colors()) :
        type == "texture_instance" ? build_entity_dictionary(m_base_group.texture_instances()) :
        StringDictionary();
}


//
// EntityBrowser<Scene> class implementation.
//

EntityBrowser<Scene>::EntityBrowser(const Scene& scene)
  : EntityBrowser<BaseGroup>(scene)
  , m_scene(scene)
{
}

StringDictionary EntityBrowser<Scene>::get_entities(const std::string& type) const
{
    return
        type == "camera" ? build_entity_dictionary(m_scene.cameras()) :
        type == "environment_edf" ? build_entity_dictionary(m_scene.environment_edfs()) :
        type == "environment_shader" ? build_entity_dictionary(m_scene.environment_shaders()) :
        EntityBrowser<BaseGroup>::get_entities(type);
}


//
// EntityBrowser<Assembly> class implementation.
//

EntityBrowser<Assembly>::EntityBrowser(const Assembly& assembly)
  : EntityBrowser<BaseGroup>(assembly)
  , m_assembly(assembly)
{
}

StringDictionary EntityBrowser<Assembly>::get_entities(const std::string& type) const
{
    StringDictionary entities =
        type == "bsdf" ? build_entity_dictionary(m_assembly.bsdfs()) :
        type == "bssrdf" ? build_entity_dictionary(m_assembly.bssrdfs()) :
        type == "edf" ? build_entity_dictionary(m_assembly.edfs()) :
        type == "material" ? build_entity_dictionary(m_assembly.materials()) :
        type == "surface_shader" ? build_entity_dictionary(m_assembly.surface_shaders()) :
        type == "environment_edf" ? build_entity_dictionary(get_parent_scene(&m_assembly)->environment_edfs()) :
        type == "environment_shader" ? build_entity_dictionary(get_parent_scene(&m_assembly)->environment_shaders()) :
        type == "volume" ? build_entity_dictionary(m_assembly.volumes()) :
        type == "shader_group" ? build_entity_dictionary(m_assembly.shader_groups()) :
        EntityBrowser<BaseGroup>::get_entities(type);

    const Assembly* parent_assembly = dynamic_cast<const Assembly*>(m_assembly.get_parent());

    if (parent_assembly)
    {
        merge_dictionary(entities, EntityBrowser<Assembly>(*parent_assembly).get_entities(type));
    }
    else
    {
        const Scene* parent_scene = dynamic_cast<const Scene*>(m_assembly.get_parent());
        assert(parent_scene);

        merge_dictionary(entities, EntityBrowser<Scene>(*parent_scene).get_entities(type));
    }

    return entities;
}


//
// EntityBrowser<Frame> class implementation.
//

EntityBrowser<Frame>::EntityBrowser(const Frame& frame)
  : m_frame(frame)
{
}

StringDictionary EntityBrowser<Frame>::get_entities(const std::string& type) const
{
    return
        type == "aov" ? build_entity_dictionary(m_frame.aovs()) :
        type == "post_processing_stage" ? build_entity_dictionary(m_frame.post_processing_stages()) :
        StringDictionary();
}

}   // namespace studio
}   // namespace appleseed
