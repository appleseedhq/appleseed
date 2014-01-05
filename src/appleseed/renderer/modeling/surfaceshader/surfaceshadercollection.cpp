
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
#include "surfaceshadercollection.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/inputformat.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>

// Forward declarations.
namespace foundation    { class AbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class PixelContext; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingPoint; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // SurfaceShaderCollection.
    //

    const char* Model = "surface_shader_collection";

    const size_t MaxShaderCount = 8;

    string get_shader_name(const size_t i)
    {
        return "surface_shader" + to_string(i + 1);
    }

    string get_shader_label(const size_t i)
    {
        return "Surface Shader " + to_string(i + 1);
    }

    class SurfaceShaderCollection
      : public SurfaceShader
    {
      public:
        SurfaceShaderCollection(
            const char*             name,
            const ParamArray&       params)
          : SurfaceShader(name, params)
        {
            for (size_t i = 0; i < MaxShaderCount; ++i)
            {
                m_inputs.declare(
                    get_shader_name(i).c_str(),
                    InputFormatEntity,
                    i == 0 ? 0 : "");   // the first input is mandatory, the others are optional
            }
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
            const Project&          project,
            const Assembly&         assembly,
            AbortSwitch*            abort_switch) OVERRIDE
        {
            if (!SurfaceShader::on_frame_begin(project, assembly, abort_switch))
                return false;

            for (size_t i = 0; i < MaxShaderCount; ++i)
            {
                m_surface_shaders[i] =
                    static_cast<SurfaceShader*>(
                        m_inputs.get_entity(get_shader_name(i).c_str()));
            }

            assert(m_surface_shaders[0]);

            return true;
        }

        virtual void evaluate(
            SamplingContext&        sampling_context,
            const PixelContext&     pixel_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingResult&          shading_result) const OVERRIDE
        {
            m_surface_shaders[0]->evaluate(
                sampling_context,
                pixel_context,
                shading_context,
                shading_point,
                shading_result);

            shading_result.m_aovs.set(
                m_surface_shaders[0]->get_render_layer_index(),
                shading_result.m_color);

            for (size_t i = 1; i < MaxShaderCount; ++i)
            {
                if (m_surface_shaders[i])
                {
                    ShadingResult local_result;
                    local_result.m_alpha = Alpha(1.0);

                    m_surface_shaders[i]->evaluate(
                        sampling_context,
                        pixel_context,
                        shading_context,
                        shading_point,
                        local_result);

                    shading_result.m_aovs.set(
                        m_surface_shaders[i]->get_render_layer_index(),
                        local_result.m_color);
                }
            }
        }

      private:
        const SurfaceShader* m_surface_shaders[MaxShaderCount];
    };
}


//
// SurfaceShaderCollectionFactory class implementation.
//

const char* SurfaceShaderCollectionFactory::get_model() const
{
    return Model;
}

const char* SurfaceShaderCollectionFactory::get_human_readable_model() const
{
    return "Collection";
}

DictionaryArray SurfaceShaderCollectionFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    for (size_t i = 0; i < MaxShaderCount; ++i)
    {
        metadata.push_back(
            Dictionary()
                .insert("name", get_shader_name(i))
                .insert("label", get_shader_label(i))
                .insert("type", "entity")
                .insert("entity_types",
                    Dictionary().insert("surface_shader", "Surface Shaders"))
                .insert("default", "")
                .insert("use", i == 0 ? "required" : "optional"));
    }

    return metadata;
}

auto_release_ptr<SurfaceShader> SurfaceShaderCollectionFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<SurfaceShader>(
            new SurfaceShaderCollection(name, params));
}

}   // namespace renderer
