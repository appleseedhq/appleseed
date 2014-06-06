
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/shadergroup/shadergroup.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // OSL Material.
    //

    const char* Model = "osl_material";

    class OSLMaterial 
      : public Material
    {
      public:
        OSLMaterial(
            const char*                 name,
            const ParamArray&           params)
          : Material(name, params)
        {
            m_inputs.declare("osl_surface", InputFormatEntity, "");
            m_inputs.declare("alpha_map", InputFormatScalar, "");
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&              project,
            const Assembly&             assembly,
            foundation::AbortSwitch*    abort_switch = 0) OVERRIDE
        {
            if (!Material::on_frame_begin(project, assembly, abort_switch))
                return false;

            m_shader_group = get_uncached_osl_surface();

            if (m_shader_group)
            {
                m_osl_bsdf = OSLBSDFFactory().create();
                m_bsdf = m_osl_bsdf.get();
                m_osl_bsdf->on_frame_begin(project, assembly, abort_switch);
            }

            return true;
        }

        virtual void on_frame_end(
            const Project&              project,
            const Assembly&             assembly) OVERRIDE
        {
            Material::on_frame_end(project, assembly);

            if (m_osl_bsdf.get())
            {
                m_osl_bsdf->on_frame_end(project, assembly);
                m_osl_bsdf.reset();
            }

            m_shader_group = 0;
        }

        virtual bool has_osl_surface() const OVERRIDE
        {
            return get_non_empty(m_params, "osl_surface") != 0;
        }

      private:
        auto_release_ptr<BSDF> m_osl_bsdf;

        virtual const ShaderGroup* get_uncached_osl_surface() const OVERRIDE
        {
            return static_cast<const ShaderGroup*>(m_inputs.get_entity("osl_surface"));
        }
    };
}


//
// OSLMaterialFactory class implementation.
//

const char* OSLMaterialFactory::get_model() const
{
    return "osl_material";
}

const char* OSLMaterialFactory::get_human_readable_model() const
{
    return "OSL Material";
}

DictionaryArray OSLMaterialFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_input_metadata(metadata);

    metadata.push_back(
        Dictionary()
            .insert("name", "osl_surface")
            .insert("label", "OSL Surface")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary()
                    .insert("shader_group", "Shader Groups"))
            .insert("use", "optional"));

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

    return metadata;
}

auto_release_ptr<Material> OSLMaterialFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<Material>(
            new OSLMaterial(name, params));
}

}   // namespace renderer
