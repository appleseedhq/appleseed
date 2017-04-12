
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/material/bumpmappingmodifier.h"
#include "renderer/modeling/material/normalmappingmodifier.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <string>

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

UniqueID Material::get_class_uid()
{
    return g_class_uid;
}

Material::Material(
    const char*             name,
    const ParamArray&       params)
  : ConnectableEntity(g_class_uid, params)
  , m_shade_alpha_cutouts(params.get_optional<bool>("shade_alpha_cutouts", false))
  , m_has_render_data(false)
{
    set_name(name);

    m_inputs.declare("surface_shader", InputFormatEntity, "");
}

bool Material::has_alpha_map() const
{
    return get_non_empty(m_params, "alpha_map") != 0;
}

bool Material::has_uniform_alpha_map_value_of_one() const
{
    const Source* source = get_uncached_alpha_map();

    if (!source || !source->is_uniform())
        return false;

    float alpha;

    source->evaluate_uniform(alpha);

    return alpha == 1.0f;
}

const char* Material::get_surface_shader_name() const
{
    return get_non_empty(m_params, "surface_shader");
}

const char* Material::get_bsdf_name() const
{
    return get_non_empty(m_params, "bsdf");
}

const char* Material::get_bssrdf_name() const
{
    return get_non_empty(m_params, "bssrdf");
}

const char* Material::get_edf_name() const
{
    return get_non_empty(m_params, "edf");
}

const SurfaceShader* Material::get_uncached_surface_shader() const
{
    return static_cast<const SurfaceShader*>(m_inputs.get_entity("surface_shader"));
}

const BSDF* Material::get_uncached_bsdf() const
{
    return static_cast<const BSDF*>(m_inputs.get_entity("bsdf"));
}

const BSSRDF* Material::get_uncached_bssrdf() const
{
    return static_cast<const BSSRDF*>(m_inputs.get_entity("bssrdf"));
}

const EDF* Material::get_uncached_edf() const
{
    return static_cast<const EDF*>(m_inputs.get_entity("edf"));
}

const Source* Material::get_uncached_alpha_map() const
{
    return m_inputs.source("alpha_map");
}

const ShaderGroup* Material::get_uncached_osl_surface() const
{
    return 0;
}

bool Material::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    assert(!m_has_render_data);

    if (!ConnectableEntity::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    m_render_data.m_surface_shader = get_uncached_surface_shader();
    if (m_render_data.m_surface_shader == 0)
        m_render_data.m_surface_shader = project.get_scene()->get_default_surface_shader();
    m_render_data.m_bsdf = 0;
    m_render_data.m_bssrdf = 0;
    m_render_data.m_edf = 0;
    m_render_data.m_alpha_map = get_uncached_alpha_map();
    m_render_data.m_shader_group = 0;
    m_render_data.m_basis_modifier = 0;
    m_has_render_data = true;

    return true;
}

void Material::on_frame_end(
    const Project&          project,
    const BaseGroup*        parent)
{
    if (m_has_render_data)
    {
        delete m_render_data.m_basis_modifier;
        m_has_render_data = false;
    }

    ConnectableEntity::on_frame_end(project, parent);
}

bool Material::has_emission() const
{
    return get_uncached_edf() != 0;
}

const char* Material::get_non_empty(const ParamArray& params, const char* name) const
{
    if (!params.strings().exist(name))
        return 0;

    const char* value = params.strings().get(name);

    return is_empty_string(value) ? 0 : value;
}

IBasisModifier* Material::create_basis_modifier(const MessageContext& context) const
{
    // Retrieve the source bound to the displacement map input.
    const Source* displacement_source = m_inputs.source("displacement_map");

    // Nothing to do if there is no displacement source.
    if (displacement_source == 0)
        return 0;

    // Only texture instances can be bound to the displacement map input.
    if (dynamic_cast<const TextureSource*>(displacement_source) == 0)
    {
        RENDERER_LOG_ERROR(
            "%s: a texture instance must be bound to the \"displacement_map\" input.",
            context.get());
        return 0;
    }

    // Retrieve the displacement texture.
    const TextureSource* displacement_map = static_cast<const TextureSource*>(displacement_source);
    const Texture& texture = displacement_map->get_texture_instance().get_texture();

    // Print a warning if the displacement texture is not expressed in the linear RGB color space.
    if (texture.get_color_space() != ColorSpaceLinearRGB)
    {
        RENDERER_LOG_WARNING(
            "%s: color space for displacement map \"%s\" "
            "should be \"%s\" but is \"%s\" instead; expect artifacts and/or slowdowns.",
            context.get(),
            texture.get_path().c_str(),
            color_space_name(ColorSpaceLinearRGB),
            color_space_name(texture.get_color_space()));
    }

    // Retrieve the displacement method.
    const string displacement_method =
        m_params.get_required<string>(
            "displacement_method",
            "bump",
            make_vector("bump", "normal"),
            context);

    // Create the basis modifier.
    if (displacement_method == "bump")
    {
        const float offset = m_params.get_optional<float>("bump_offset", 2.0f);
        const float amplitude = m_params.get_optional<float>("bump_amplitude", 1.0f);
        return new BumpMappingModifier(displacement_map, offset, amplitude);
    }
    else
    {
        const NormalMappingModifier::UpVector up_vector =
            m_params.get_optional<string>("normal_map_up", "z", make_vector("y", "z"), context) == "y"
                ? NormalMappingModifier::UpVectorY
                : NormalMappingModifier::UpVectorZ;
        return new NormalMappingModifier(displacement_map, up_vector);
    }
}

}   // namespace renderer
