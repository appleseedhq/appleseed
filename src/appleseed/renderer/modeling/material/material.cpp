
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
#include "material.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
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
  , m_normal_map(0)
{
    set_name(name);

    m_inputs.declare("bsdf", InputEntity, "");
    m_inputs.declare("edf", InputEntity, "");
    m_inputs.declare("surface_shader", InputEntity);
    m_inputs.declare("alpha_map", InputFormatScalar, "");
    m_inputs.declare("normal_map", InputFormatSpectrum, "");
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

namespace
{
    void check_texture_source_color_space_is_linear_rgb(
        const char*     map_type,
        const Source*   source)
    {
        if (dynamic_cast<const TextureSource*>(source))
        {
            const Texture* texture =
                static_cast<const TextureSource*>(source)->get_texture_instance().get_texture();

            if (texture->get_color_space() != ColorSpaceLinearRGB)
            {
                RENDERER_LOG_WARNING(
                    "color space for %s \"%s\" should be \"%s\" but is \"%s\" instead; expect artifacts and/or slowdowns.",
                    map_type,
                    texture->get_name(),
                    color_space_name(ColorSpaceLinearRGB),
                    color_space_name(texture->get_color_space()));
            }
        }
    }
}

bool Material::on_frame_begin(
    const Project&      project,
    const Assembly&     assembly)
{
    m_surface_shader = get_uncached_surface_shader();
    m_bsdf = get_uncached_bsdf();
    m_edf = get_uncached_edf();
    m_alpha_map = get_uncached_alpha_map();
    m_normal_map = get_uncached_normal_map();

    check_texture_source_color_space_is_linear_rgb("normal map", m_normal_map);

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
    m_normal_map = 0;
}

const SurfaceShader* Material::get_uncached_surface_shader() const
{
    return static_cast<SurfaceShader*>(m_inputs.get_entity("surface_shader"));
}

const BSDF* Material::get_uncached_bsdf() const
{
    return static_cast<BSDF*>(m_inputs.get_entity("bsdf"));
}

const EDF* Material::get_uncached_edf() const
{
    return static_cast<EDF*>(m_inputs.get_entity("edf"));
}

const Source* Material::get_uncached_alpha_map() const
{
    return m_inputs.source("alpha_map");
}

const Source* Material::get_uncached_normal_map() const
{
    return m_inputs.source("normal_map");
}


//
// MaterialFactory class implementation.
//

const char* MaterialFactory::get_model()
{
    return "generic_material";
}

DictionaryArray MaterialFactory::get_widget_definitions()
{
    DictionaryArray definitions;

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
            .insert("entity_types",
                Dictionary().insert("surface_shader", "Surface Shaders"))
            .insert("use", "required"));

    definitions.push_back(
        Dictionary()
            .insert("name", "alpha_map")
            .insert("label", "Alpha Map")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional"));

    definitions.push_back(
        Dictionary()
            .insert("name", "normal_map")
            .insert("label", "Normal Map")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional"));

    return definitions;
}

auto_release_ptr<Material> MaterialFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<Material>(new Material(name, params));
}

}   // namespace renderer
