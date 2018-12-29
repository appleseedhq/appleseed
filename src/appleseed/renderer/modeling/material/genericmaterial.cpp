
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
#include "genericmaterial.h"

// appleseed.renderer headers.
#include "renderer/modeling/material/material.h"

// appleseed.foundation headers.
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Generic material.
    //

    const char* Model = "generic_material";

    class GenericMaterial
      : public Material
    {
      public:
        GenericMaterial(
            const char*             name,
            const ParamArray&       params)
          : Material(name, params)
        {
            m_inputs.declare("bsdf", InputFormatEntity, "");
            m_inputs.declare("bssrdf", InputFormatEntity, "");
            m_inputs.declare("edf", InputFormatEntity, "");
            m_inputs.declare("alpha_map", InputFormatFloat, "");
            m_inputs.declare("displacement_map", InputFormatSpectralReflectance, "");
            m_inputs.declare("volume", InputFormatEntity, "");
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            if (!Material::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            const OnFrameBeginMessageContext context("material", this);

            m_render_data.m_bsdf = get_uncached_bsdf();
            m_render_data.m_bssrdf = get_uncached_bssrdf();
            m_render_data.m_edf = get_uncached_edf();
            m_render_data.m_volume = get_uncached_volume();
            m_render_data.m_basis_modifier = create_basis_modifier(context);

            if (m_render_data.m_edf && m_render_data.m_alpha_map)
            {
                RENDERER_LOG_WARNING(
                    "%smaterial is emitting light but may be partially or entirely transparent; "
                    "this may lead to unexpected or unphysical results.",
                    context.get());
            }

            return true;
        }
    };
}


//
// GenericMaterialFactory class implementation.
//

void GenericMaterialFactory::release()
{
    delete this;
}

const char* GenericMaterialFactory::get_model() const
{
    return Model;
}

Dictionary GenericMaterialFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Generic Material");
}

DictionaryArray GenericMaterialFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_surface_shader_metadata(metadata);

    metadata.push_back(
        Dictionary()
            .insert("name", "bsdf")
            .insert("label", "BSDF")
            .insert("type", "entity")
            .insert("entity_types", Dictionary().insert("bsdf", "BSDF"))
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "bssrdf")
            .insert("label", "BSSRDF")
            .insert("type", "entity")
            .insert("entity_types", Dictionary().insert("bssrdf", "BSSRDF"))
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
        .insert("name", "volume")
        .insert("label", "Volume")
        .insert("type", "entity")
        .insert("entity_types",
            Dictionary().insert("volume", "Volume"))
        .insert("use", "optional"));

    add_alpha_map_metadata(metadata);
    add_displacement_metadata(metadata);

    return metadata;
}

auto_release_ptr<Material> GenericMaterialFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Material>(new GenericMaterial(name, params));
}

}   // namespace renderer
