
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/containers/specializedarrays.h"

// Forward declarations.
namespace renderer  { class Project; }
namespace renderer  { class TextureCache; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Physical surface shader.
    //

    const char* Model = "physical_surface_shader";

    class PhysicalSurfaceShader
      : public SurfaceShader
    {
      public:
        PhysicalSurfaceShader(
            const char*             name,
            const ParamArray&       params)
          : SurfaceShader(name, params)
          , m_has_alpha_mask(false)
        {
            m_inputs.declare("alpha_mask", InputFormatSpectrum, true);
        }

        virtual void release()
        {
            delete this;
        }

        virtual const char* get_model() const
        {
            return Model;
        }

        virtual void on_frame_begin(const Project& project)
        {
            m_has_alpha_mask = m_inputs.source("alpha_mask") != 0;
        }

        virtual void evaluate(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingResult&          shading_result) const
        {
            // Set color space to spectral.
            shading_result.m_color_space = ColorSpaceSpectral;

            // Retrieve the lighting engine.
            ILightingEngine* lighting_engine =
                shading_context.get_lighting_engine();
            assert(lighting_engine);

            // Compute the lighting.
            lighting_engine->compute_lighting(
                sampling_context,
                shading_context,
                shading_point,
                shading_result.m_color);

            // Set alpha channel to full opacity.
            shading_result.m_alpha = Alpha(1.0);
        }

        virtual void evaluate_alpha_mask(
            SamplingContext&        sampling_context,
            TextureCache&           texture_cache,
            const ShadingPoint&     shading_point,
            Alpha&                  alpha) const
        {
            if (!m_has_alpha_mask)
            {
                // Set alpha channel to full opacity.
                alpha = Alpha(1.0);
            }
            else
            {
                // Evaluate the inputs.
                InputValues values;
                m_inputs.evaluate(
                    texture_cache,
                    shading_point.get_input_params(),
                    &values);

                // Set alpha channel.
                alpha = values.m_alpha_mask_alpha;
            }
        }

      private:
        struct InputValues
        {
            Spectrum        m_alpha_mask_color;             // unused
            Alpha           m_alpha_mask_alpha;
        };

        bool                m_has_alpha_mask;
    };
}


//
// PhysicalSurfaceShaderFactory class implementation.
//

const char* PhysicalSurfaceShaderFactory::get_model() const
{
    return Model;
}

const char* PhysicalSurfaceShaderFactory::get_human_readable_model() const
{
    return "Physical";
}

DictionaryArray PhysicalSurfaceShaderFactory::get_widget_definitions() const
{
    DictionaryArray definitions;
    return definitions;
}

auto_release_ptr<SurfaceShader> PhysicalSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<SurfaceShader>(
            new PhysicalSurfaceShader(name, params));
}

}   // namespace renderer
