
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#ifdef WITH_OSL
#include "renderer/modeling/bsdf/oslbsdf.h"
#endif
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/material/bumpmappingmodifier.h"
#include "renderer/modeling/material/normalmappingmodifier.h"
#include "renderer/modeling/scene/textureinstance.h"
#ifdef WITH_OSL
#include "renderer/modeling/shadergroup/shadergroup.h"
#endif
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/makevector.h"

// Standard headers.
#include <cassert>
#include <cstring>
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

struct Material::Impl
{
    string m_model;
};

Material::Material(
    const char*         name,
    const char*         model,
    const ParamArray&   params)
  : ConnectableEntity(g_class_uid, params)
  , impl(new Impl())
  , m_shade_alpha_cutouts(false)
  , m_surface_shader(0)
  , m_bsdf(0)
  , m_edf(0)
  , m_alpha_map(0)
  , m_normal_modifier(0)
#ifdef WITH_OSL
  , m_shader_group(0)
#endif
{
    set_name(name);

    impl->m_model = model;

    m_inputs.declare("surface_shader", InputFormatEntity);
    
    if (strcmp(get_model(), "generic_material") == 0)
    {
        m_inputs.declare("bsdf", InputFormatEntity, "");
        m_inputs.declare("edf", InputFormatEntity, "");
        m_inputs.declare("alpha_map", InputFormatScalar, "");
        m_inputs.declare("displacement_map", InputFormatSpectralReflectance, "");
    }
#ifdef WITH_OSL
    else if (strcmp(get_model(), "osl_material") == 0)
    {
        m_inputs.declare("osl_surface", InputFormatEntity, "");
        m_inputs.declare("alpha_map", InputFormatScalar, "");
    }
#endif
    else
    {
        assert(!"Invalid material model.");
    }
}

Material::~Material()
{
    delete impl;
}

void Material::release()
{
    delete this;
}

const char* Material::get_model() const
{
    return impl->m_model.c_str();
}

namespace
{
    const char* get_non_empty(const ParamArray& params, const char* name)
    {
        if (!params.strings().exist(name))
            return 0;

        const char* value = params.strings().get(name);

        return strlen(value) > 0 ? value : 0;
    }
}

bool Material::has_alpha_map() const
{
    return get_non_empty(m_params, "alpha_map") != 0;
}

const char* Material::get_surface_shader_name() const
{
    return get_non_empty(m_params, "surface_shader");
}

const char* Material::get_bsdf_name() const
{
    return get_non_empty(m_params, "bsdf");
}

const char* Material::get_edf_name() const
{
    return get_non_empty(m_params, "edf");
}

bool Material::on_frame_begin(
    const Project&      project,
    const Assembly&     assembly,
    AbortSwitch*        abort_switch)
{
    const EntityDefMessageContext context("material", this);

    m_shade_alpha_cutouts = m_params.get_optional<bool>("shade_alpha_cutouts", false);

    m_surface_shader = get_uncached_surface_shader();
    m_alpha_map = get_uncached_alpha_map();
    
    if (strcmp(get_model(), "generic_material") == 0)
    {
        m_bsdf = get_uncached_bsdf();
        m_edf = get_uncached_edf();

        if (!create_normal_modifier(context))
            return false;

        if (m_edf && m_alpha_map)
        {
            RENDERER_LOG_WARNING(
                "%s: material is emitting light but may be partially or entirely transparent; "
                "this may lead to unexpected or unphysical results.",
                context.get());
        }
    }
#ifdef WITH_OSL
    else if (strcmp(get_model(), "osl_material") == 0)
    {
        m_shader_group = get_uncached_osl_surface();

        if (m_shader_group)
        {
            m_osl_bsdf = OSLBSDFFactory().create();
            m_bsdf = m_osl_bsdf.get();
            m_osl_bsdf->on_frame_begin(project, assembly, abort_switch);
        }
    }
#endif
    else
        assert(!"Invalid material model.");

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
    
#ifdef WITH_OSL
    if (m_osl_bsdf.get())
    {
        m_osl_bsdf->on_frame_end(project, assembly);
        m_osl_bsdf.reset();
    }

    m_shader_group = 0;
#endif
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

#ifdef WITH_OSL

bool Material::has_osl_surface() const
{
    if (strcmp(get_model(), "osl_material") != 0)
        return false;

    return get_non_empty(m_params, "osl_surface") != 0;
}

const ShaderGroup* Material::get_uncached_osl_surface() const
{
    return static_cast<const ShaderGroup*>(m_inputs.get_entity("osl_surface"));
}

#endif

bool Material::create_normal_modifier(const MessageContext& context)
{
    assert(m_normal_modifier == 0);

    // Retrieve the source bound to the displacement map input.
    const Source* displacement_source = m_inputs.source("displacement_map");

    // Nothing to do if there is no displacement source.
    if (displacement_source == 0)
        return true;

    // Only texture instances can be bound to the displacement map input.
    if (dynamic_cast<const TextureSource*>(displacement_source) == 0)
    {
        RENDERER_LOG_ERROR(
            "%s: a texture instance must be bound to the \"displacement_map\" input.",
            context.get());
        return false;
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
            texture.get_name(),
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

    // Create the normal modifier.
    if (displacement_method == "bump")
    {
        const double amplitude = m_params.get_optional<double>("bump_amplitude", 1.0);
        m_normal_modifier = new BumpMappingModifier(displacement_map, 2.0, amplitude);
    }
    else
    {
        const NormalMappingModifier::UpVector up_vector =
            m_params.get_optional<string>("normal_map_up", "z", make_vector("y", "z"), context) == "y"
                ? NormalMappingModifier::UpVectorY
                : NormalMappingModifier::UpVectorZ;
        m_normal_modifier = new NormalMappingModifier(displacement_map, up_vector);
    }

    return true;
}

}   // namespace renderer
