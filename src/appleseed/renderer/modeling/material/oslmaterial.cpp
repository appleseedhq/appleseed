
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "oslmaterial.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/oslbsdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/oslbssrdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/edf/osledf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/shadergroup/shadergroup.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/api/specializedapiarrays.h"

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // OSL material.
    //

    const char* Model = "osl_material";

    class OSLMaterial
      : public Material
    {
      public:
        OSLMaterial(
            const char*             name,
            const ParamArray&       params)
          : Material(name, params)
        {
            m_inputs.declare("osl_surface", InputFormatEntity, "");
            m_inputs.declare("alpha_map", InputFormatFloat, "");

            m_osl_bsdf = OSLBSDFFactory().create();
            m_osl_bssrdf = OSLBSSRDFFactory().create();
            m_osl_edf = OSLEDFFactory().create();
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        bool has_emission() const override
        {
            if (const ShaderGroup* sg = get_uncached_osl_surface())
                return sg->has_emission();

            return false;
        }

        bool on_render_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnRenderBeginRecorder&  recorder,
            IAbortSwitch*           abort_switch) override
        {
            if (!Material::on_render_begin(project, parent, recorder, abort_switch))
                return false;

            bool success = true;
            success = success && m_osl_bsdf->on_render_begin(project, parent, recorder, abort_switch);
            success = success && m_osl_bssrdf->on_render_begin(project, parent, recorder, abort_switch);
            success = success && m_osl_edf->on_render_begin(project, parent, recorder, abort_switch);

            return success;
        }

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            if (!Material::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            bool success = true;
            success = success && m_osl_bsdf->on_frame_begin(project, parent, recorder, abort_switch);
            success = success && m_osl_bssrdf->on_frame_begin(project, parent, recorder, abort_switch);
            success = success && m_osl_edf->on_frame_begin(project, parent, recorder, abort_switch);
            if (!success)
                return false;

            m_render_data.m_shader_group = get_uncached_osl_surface();
            m_render_data.m_bsdf = nullptr;
            m_render_data.m_bssrdf = nullptr;
            m_render_data.m_edf = nullptr;

            if (m_render_data.m_shader_group)
            {
                if (m_render_data.m_shader_group->has_bsdfs())
                    m_render_data.m_bsdf = m_osl_bsdf.get();

                if (m_render_data.m_shader_group->has_subsurface())
                    m_render_data.m_bssrdf = m_osl_bssrdf.get();

                if (m_render_data.m_shader_group->has_emission())
                    m_render_data.m_edf = m_osl_edf.get();
            }

            return true;
        }

      private:
        auto_release_ptr<BSDF>      m_osl_bsdf;
        auto_release_ptr<BSSRDF>    m_osl_bssrdf;
        auto_release_ptr<EDF>       m_osl_edf;

        const ShaderGroup* get_uncached_osl_surface() const override
        {
            const ShaderGroup* sg =
                static_cast<const ShaderGroup*>(m_inputs.get_entity("osl_surface"));

            return sg && sg->is_valid() ? sg : nullptr;
        }
    };
}


//
// OSLMaterialFactory class implementation.
//

void OSLMaterialFactory::release()
{
    delete this;
}

const char* OSLMaterialFactory::get_model() const
{
    return "osl_material";
}

Dictionary OSLMaterialFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "OSL Material");
}

DictionaryArray OSLMaterialFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_surface_shader_metadata(metadata);

    metadata.push_back(
        Dictionary()
            .insert("name", "osl_surface")
            .insert("label", "OSL Surface")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary()
                    .insert("shader_group", "Shader Groups"))
            .insert("use", "optional"));

    add_alpha_map_metadata(metadata);
    add_default_tangent_mode_metadata(metadata);

    return metadata;
}

auto_release_ptr<Material> OSLMaterialFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Material>(new OSLMaterial(name, params));
}

}   // namespace renderer
