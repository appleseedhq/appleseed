
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "renderlayerrule.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// RenderLayerRule class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID RenderLayerRule::get_class_uid()
{
    return g_class_uid;
}

struct RenderLayerRule::Impl
{
    string      m_render_layer;
    int         m_order;
    UniqueID    m_entity_type_uid;
};

RenderLayerRule::RenderLayerRule(
    const char*         name,
    const ParamArray&   params)
  : Entity(g_class_uid, params)
  , impl(new Impl())
{
    set_name(name);

    const EntityDefMessageContext context("render layer rule", this);

    impl->m_render_layer = params.get_required<string>("render_layer", "", context);
    impl->m_order = params.get_required<int>("order", 0, context);

    const string entity_type = params.get_optional<string>("entity_type", "");

    if (entity_type == "")
        impl->m_entity_type_uid = UniqueID(~0);
    else if (entity_type == "assembly")
        impl->m_entity_type_uid = Assembly::get_class_uid();
    else if (entity_type == "assembly_instance")
        impl->m_entity_type_uid = AssemblyInstance::get_class_uid();
    else if (entity_type == "edf")
        impl->m_entity_type_uid = EDF::get_class_uid();
    else if (entity_type == "environment_edf")
        impl->m_entity_type_uid = EnvironmentEDF::get_class_uid();
    else if (entity_type == "environment_shader")
        impl->m_entity_type_uid = EnvironmentShader::get_class_uid();
    else if (entity_type == "light")
        impl->m_entity_type_uid = Light::get_class_uid();
    else if (entity_type == "material")
        impl->m_entity_type_uid = Material::get_class_uid();
    else if (entity_type == "object")
        impl->m_entity_type_uid = Object::get_class_uid();
    else if (entity_type == "object_instance")
        impl->m_entity_type_uid = ObjectInstance::get_class_uid();
    else if (entity_type == "surface_shader")
        impl->m_entity_type_uid = SurfaceShader::get_class_uid();
    else
    {
        RENDERER_LOG_ERROR(
            "%s: invalid value \"%s\" for parameter \"%s\", using default value \"\".",
            context.get(),
            entity_type.c_str(),
            "entity_type");
        impl->m_entity_type_uid = UniqueID(~0);
    }
}

RenderLayerRule::~RenderLayerRule()
{
    delete impl;
}

const char* RenderLayerRule::get_render_layer() const
{
    return impl->m_render_layer.c_str();
}

const UniqueID RenderLayerRule::get_entity_type_uid() const
{
    return impl->m_entity_type_uid;
}

int RenderLayerRule::get_order() const
{
    return impl->m_order;
}


//
// RenderLayerRuleFactory class implementation.
//

DictionaryArray RenderLayerRuleFactory::get_input_metadata()
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "render_layer")
            .insert("label", "Render Layer")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "entity_type")
            .insert("label", "Entity Type")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("All", "")
                    .insert("Assemblies", "assembly")
                    .insert("Assembly Instances", "assembly_instance")
                    .insert("EDFs", "edf")
                    .insert("Environment EDFs", "environment_edf")
                    .insert("Environment Shaders", "environment_shader")
                    .insert("Lights", "light")
                    .insert("Materials", "material")
                    .insert("Objects", "object")
                    .insert("Object Instances", "object_instance")
                    .insert("Surface Shaders", "surface_shader"))
            .insert("default", "")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "order")
            .insert("label", "Order")
            .insert("type", "text")
            .insert("default", "1")
            .insert("use", "required"));

    return metadata;
}

}   // namespace renderer
