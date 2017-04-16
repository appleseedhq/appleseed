
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
#include "physicalsurfaceshader.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/aov/aovsettings.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/shadingfragmentstack.h"
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingfragment.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/colorspace.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class PixelContext; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Physical surface shader for physically-based rendering.
    //

    const char* Model = "physical_surface_shader";

    class PhysicalSurfaceShader
      : public SurfaceShader
    {
      public:
        PhysicalSurfaceShader(
            const char*                 name,
            const ParamArray&           params)
          : SurfaceShader(name, params)
          , m_lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg)
        {
            m_inputs.declare("color_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("alpha_multiplier", InputFormatFloat, "1.0");
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
            const Project&              project,
            const BaseGroup*            parent,
            OnFrameBeginRecorder&       recorder,
            IAbortSwitch*               abort_switch) APPLESEED_OVERRIDE
        {
            m_lighting_samples = m_params.get_optional<size_t>("lighting_samples", 1);
            return true;
        }

        virtual void evaluate(
            SamplingContext&            sampling_context,
            const PixelContext&         pixel_context,
            const ShadingContext&       shading_context,
            const ShadingPoint&         shading_point,
            AOVAccumulatorContainer&    aov_accumulators) const APPLESEED_OVERRIDE
        {
            // Evaluate the shader inputs.
            InputValues values;
            m_inputs.evaluate(
                shading_context.get_texture_cache(),
                shading_point.get_uv(0),
                &values);

            Spectrum radiance(Spectrum::Illuminance);

            compute_lighting(
                values,
                sampling_context,
                pixel_context,
                shading_context,
                shading_point,
                radiance);

            // Initialize the shading result.
            aov_accumulators.beauty().set(radiance);

            // Apply multipliers.
            aov_accumulators.beauty().apply_multiplier(values.m_color_multiplier);
            aov_accumulators.alpha().apply_multiplier(Alpha(values.m_alpha_multiplier));
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            float       m_color_multiplier;
            float       m_alpha_multiplier;
            Spectrum    m_translucency;
        };

        const LightingConditions    m_lighting_conditions;
        size_t                      m_lighting_samples;

        void compute_lighting(
            const InputValues&          values,
            SamplingContext&            sampling_context,
            const PixelContext&         pixel_context,
            const ShadingContext&       shading_context,
            const ShadingPoint&         shading_point,
            Spectrum&                   radiance) const
        {
            radiance.set(0.0f);

            for (size_t i = 0; i < m_lighting_samples; ++i)
            {
                shading_context.get_lighting_engine()->compute_lighting(
                    sampling_context,
                    pixel_context,
                    shading_context,
                    shading_point,
                    radiance);
            }

            if (m_lighting_samples > 1)
            {
                const float rcp_sample_count = 1.0f / m_lighting_samples;
                radiance *= rcp_sample_count;
            }
        }
    };
}


//
// PhysicalSurfaceShaderFactory class implementation.
//

const char* PhysicalSurfaceShaderFactory::get_model() const
{
    return Model;
}

Dictionary PhysicalSurfaceShaderFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Physical")
            .insert("default_model", "true");
}

DictionaryArray PhysicalSurfaceShaderFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "color_multiplier")
            .insert("label", "Color Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("default", "1.0")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha_multiplier")
            .insert("label", "Alpha Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("default", "1.0")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "lighting_samples")
            .insert("label", "Lighting Samples")
            .insert("type", "text")
            .insert("default", "1")
            .insert("use", "optional"));

    return metadata;
}

auto_release_ptr<SurfaceShader> PhysicalSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<SurfaceShader>(new PhysicalSurfaceShader(name, params));
}

auto_release_ptr<SurfaceShader> PhysicalSurfaceShaderFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<SurfaceShader>(new PhysicalSurfaceShader(name, params));
}

}   // namespace renderer
