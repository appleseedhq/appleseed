
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
#include "surfaceshadercollection.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/shadingfragmentstack.h"
#include "renderer/kernel/shading/shadingfragment.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
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

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) APPLESEED_OVERRIDE
        {
            if (!SurfaceShader::on_frame_begin(project, parent, recorder, abort_switch))
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
            ShadingResult&          shading_result) const APPLESEED_OVERRIDE
        {
            m_surface_shaders[0]->evaluate(
                sampling_context,
                pixel_context,
                shading_context,
                shading_point,
                shading_result);

            shading_result.set_entity_aov(*m_surface_shaders[0]);

            for (size_t i = 1; i < MaxShaderCount; ++i)
            {
                if (m_surface_shaders[i])
                {
                    ShadingResult local_result;
                    local_result.m_main.m_alpha = shading_result.m_main.m_alpha;

                    m_surface_shaders[i]->evaluate(
                        sampling_context,
                        pixel_context,
                        shading_context,
                        shading_point,
                        local_result);

                    shading_result.set_entity_aov(*m_surface_shaders[i], local_result.m_main);
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

Dictionary SurfaceShaderCollectionFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Collection");
}

DictionaryArray SurfaceShaderCollectionFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    for (size_t i = 0; i < MaxShaderCount; ++i)
    {
        Dictionary dic;
        dic.insert("name", get_shader_name(i));
        dic.insert("label", get_shader_label(i));
        dic.insert("type", "entity");
        dic.insert("entity_types", Dictionary().insert("surface_shader", "Surface Shaders"));

        if (i == 0)
        {
            dic.insert("use", "required");
        }
        else
        {
            dic.insert("use", "optional");
            dic.insert("default", "");
        }

        metadata.push_back(dic);
    }

    return metadata;
}

auto_release_ptr<SurfaceShader> SurfaceShaderCollectionFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<SurfaceShader>(new SurfaceShaderCollection(name, params));
}

auto_release_ptr<SurfaceShader> SurfaceShaderCollectionFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<SurfaceShader>(new SurfaceShaderCollection(name, params));
}

}   // namespace renderer
