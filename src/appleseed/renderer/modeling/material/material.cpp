
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "material.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/material/bumpmappingmodifier.h"
#include "renderer/modeling/material/normalmappingmodifier.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/uid.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Material class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

Material::Material(
    const char*         name,
    const ParamArray&   params)
  : ConnectableEntity(g_class_uid, params)
  , m_surface_shader(0)
  , m_bsdf(0)
  , m_edf(0)
  , m_alpha_map(0)
  , m_normal_modifier(0)
{
    set_name(name);

    m_inputs.declare("bsdf", InputFormatEntity, "");
    m_inputs.declare("edf", InputFormatEntity, "");
    m_inputs.declare("surface_shader", InputFormatEntity);
    m_inputs.declare("alpha_map", InputFormatScalar, "");
    m_inputs.declare("displacement_map", InputFormatSpectralReflectance, "");
}

void Material::release()
{
    delete this;
}

const char* Material::get_model() const
{
    return MaterialFactory::get_model();
}

bool Material::has_alpha_map() const
{
    if (!m_params.strings().exist("alpha_map"))
        return false;

    return !m_params.get<string>("alpha_map").empty();
}

bool Material::on_frame_begin(
    const Project&      project,
    const Assembly&     assembly)
{
    const EntityDefMessageContext context("material", this);

    m_surface_shader = get_uncached_surface_shader();
    m_bsdf = get_uncached_bsdf();
    m_edf = get_uncached_edf();
    m_alpha_map = get_uncached_alpha_map();

    const Source* displacement_source = m_inputs.source("displacement_map");

    if (displacement_source)
    {
        if (dynamic_cast<const TextureSource*>(displacement_source) == 0)
        {
            RENDERER_LOG_ERROR(
                "%s: a texture instance must be bound to the \"displacement_map\" input.",
                context.get());

            return false;
        }
        else
        {
            const TextureSource* displacement_map = static_cast<const TextureSource*>(displacement_source);
            const Texture& texture = displacement_map->get_texture_instance().get_texture();

            if (texture.get_color_space() != ColorSpaceLinearRGB)
            {
                RENDERER_LOG_WARNING(
                    "%s: color space for displacement map \"%s\" "
                    "should be \"%s\" but is \"%s\" instead; expect artifacts and/or slowdowns.",
                    context.get(),
                    texture.get_name(),
                    color_space_name(ColorSpaceLinearRGB),
                    color_space_name(texture.get_color_space()));
            }

            // Retrieve the displacement method and create the normal modifier.
            const string displacement_method =
                m_params.get_required<string>("displacement_method", "bump", make_vector("bump", "normal"), context);
            if (displacement_method == "bump")
            {
                const double amplitude = m_params.get_optional<double>("bump_amplitude", 1.0);
                m_normal_modifier = new BumpMappingModifier(displacement_map, 2.0, amplitude);
            }
            else
            {
                const string up_string =
                    m_params.get_optional<string>("normal_map_up", "z", make_vector("y", "z"), context);
                m_normal_modifier =
                    new NormalMappingModifier(
                        displacement_map,
                        up_string == "y" ? NormalMappingModifier::UpVectorY : NormalMappingModifier::UpVectorZ);
            }
        }
    }

    return true;
}

void Material::on_frame_end(
    const Project&      project,
    const Assembly&     assembly)
{
    m_surface_shader = 0;
    m_bsdf = 0;
    m_edf = 0;
    m_alpha_map = 0;

    delete m_normal_modifier;
    m_normal_modifier = 0;
}

const SurfaceShader* Material::get_uncached_surface_shader() const
{
    return static_cast<const SurfaceShader*>(m_inputs.get_entity("surface_shader"));
}

const BSDF* Material::get_uncached_bsdf() const
{
    return static_cast<const BSDF*>(m_inputs.get_entity("bsdf"));
}

const EDF* Material::get_uncached_edf() const
{
    return static_cast<const EDF*>(m_inputs.get_entity("edf"));
}

const Source* Material::get_uncached_alpha_map() const
{
    return m_inputs.source("alpha_map");
}


//
// MaterialFactory class implementation.
//

const char* MaterialFactory::get_model()
{
    return "generic_material";
}

DictionaryArray MaterialFactory::get_input_metadata()
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "bsdf")
            .insert("label", "BSDF")
            .insert("type", "entity")
            .insert("entity_types", Dictionary().insert("bsdf", "BSDF"))
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "edf")
            .insert("label", "EDF")
            .insert("type", "entity")
            .insert("entity_types", Dictionary().insert("edf", "EDF"))
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "surface_shader")
            .insert("label", "Surface Shader")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary().insert("surface_shader", "Surface Shaders"))
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha_map")
            .insert("label", "Alpha Map")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "displacement_map")
            .insert("label", "Displacement Map")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
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
            .insert("default", "bump"));

    metadata.push_back(
        Dictionary()
            .insert("name", "bump_amplitude")
            .insert("label", "Bump Amplitude")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "1.0"));

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
            .insert("default", "z"));

    return metadata;
}

auto_release_ptr<Material> MaterialFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<Material>(new Material(name, params));
}

}   // namespace renderer
