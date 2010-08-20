
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "constantsurfaceshader.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{

    //
    // Constant color surface shader.
    //

    class ConstantSurfaceShader
      : public SurfaceShader
    {
      public:
        // Constructor.
        ConstantSurfaceShader(
            const char*             name,
            const ParamArray&       params)
          : SurfaceShader(params)
          , m_name(name)
        {
            m_inputs.declare("color", InputFormatSpectrum);
        }

        // Delete this instance.
        virtual void release()
        {
            delete this;
        }

        // Return a string identifying the model of this surface shader.
        virtual const char* get_model() const
        {
            return ConstantSurfaceShaderFactory::get_model();
        }

        // Return the name of this surface shader.
        virtual const char* get_name() const
        {
            return m_name.c_str();
        }

        // Evaluate the shading at a given point.
        virtual void evaluate(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingResult&          shading_result) const
        {
            // Evaluate the inputs.
            InputValues values;
            m_inputs.evaluate(
                shading_context.get_texture_cache(),
                shading_point.get_input_params(),
                &values);

            // Set color space to spectral.
            shading_result.m_color_space = ColorSpaceSpectral;

            // Set color and alpha channels.
            shading_result.m_color = values.m_color;

            // Set alpha channel to full opacity.
            shading_result.m_alpha = Alpha(1.0);
        }

        // Evaluate the alpha mask at a given point.
        virtual void evaluate_alpha_mask(
            const SamplingContext&  sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            Alpha&                  alpha) const
        {
            // Evaluate the inputs.
            InputValues values;
            m_inputs.evaluate(
                shading_context.get_texture_cache(),
                shading_point.get_input_params(),
                &values);

            // Set alpha channel.
            alpha = values.m_alpha;
        }

      private:
        // Input values.
        struct InputValues
        {
            Spectrum    m_color;
            Alpha       m_alpha;
        };

        const string    m_name;
    };

}   // anonymous namespace


//
// ConstantSurfaceShaderFactory class implementation.
//

// Return a string identifying this surface shader model.
const char* ConstantSurfaceShaderFactory::get_model()
{
    return "constant_surface_shader";
}

// Create a new constant color surface shader.
auto_release_ptr<SurfaceShader> ConstantSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<SurfaceShader>(
            new ConstantSurfaceShader(name, params));
}

}   // namespace renderer
