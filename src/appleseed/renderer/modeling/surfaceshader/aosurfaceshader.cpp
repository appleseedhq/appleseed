
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
#include "aosurfaceshader.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/shading/ambientocclusion.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Forward declarations.
namespace renderer  { class PixelContext; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Ambient occlusion surface shader.
    //

    const char* Model = "ao_surface_shader";

    class AOSurfaceShader
      : public SurfaceShader
    {
      public:
        AOSurfaceShader(
            const char*                 name,
            const ParamArray&           params)
          : SurfaceShader(name, params)
          , m_samples(m_params.get_required<size_t>("samples", 16))
          , m_max_distance(m_params.get_required<double>("max_distance", 1.0))
        {
            const string sampling_method = m_params.get_required<string>("sampling_method", "uniform");

            if (sampling_method == "uniform")
                m_sampling_method = UniformSampling;
            else if (sampling_method == "cosine")
                m_sampling_method = CosineWeightedSampling;
            else
            {
                RENDERER_LOG_ERROR(
                    "invalid value \"%s\" for parameter \"sampling_method\", "
                    "using default value \"uniform\".",
                    sampling_method.c_str());
                m_sampling_method = UniformSampling;
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

        virtual void evaluate(
            SamplingContext&            sampling_context,
            const PixelContext&         pixel_context,
            const ShadingContext&       shading_context,
            const ShadingPoint&         shading_point,
            AOVAccumulatorContainer&    aov_accumulators) const APPLESEED_OVERRIDE
        {
            double occlusion;

            if (m_sampling_method == UniformSampling)
            {
                occlusion =
                    compute_ambient_occlusion(
                        sampling_context,
                        sample_hemisphere_uniform<double>,
                        shading_context.get_intersector(),
                        shading_point,
                        m_max_distance,
                        m_samples);
            }
            else
            {
                occlusion =
                    compute_ambient_occlusion(
                        sampling_context,
                        sample_hemisphere_cosine<double>,
                        shading_context.get_intersector(),
                        shading_point,
                        m_max_distance,
                        m_samples);
            }

            const float accessibility = static_cast<float>(1.0 - occlusion);

            aov_accumulators.beauty().set(Color3f(accessibility));
        }

      private:
        enum SamplingMethod
        {
            UniformSampling,
            CosineWeightedSampling
        };

        const size_t    m_samples;
        const double    m_max_distance;
        SamplingMethod  m_sampling_method;
    };
}


//
// AOSurfaceShaderFactory class implementation.
//

const char* AOSurfaceShaderFactory::get_model() const
{
    return Model;
}

Dictionary AOSurfaceShaderFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Ambient Occlusion");
}

DictionaryArray AOSurfaceShaderFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "sampling_method")
            .insert("label", "Sampling Method")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Uniform Sampling", "uniform")
                    .insert("Cosine-Weighted Sampling", "cosine"))
            .insert("use", "required")
            .insert("default", "uniform"));

    metadata.push_back(
        Dictionary()
            .insert("name", "samples")
            .insert("label", "Samples")
            .insert("type", "text")
            .insert("use", "required")
            .insert("default", "16"));

    metadata.push_back(
        Dictionary()
            .insert("name", "max_distance")
            .insert("label", "Maximum Occlusion Distance")
            .insert("type", "text")
            .insert("use", "required")
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<SurfaceShader> AOSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<SurfaceShader>(new AOSurfaceShader(name, params));
}

auto_release_ptr<SurfaceShader> AOSurfaceShaderFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<SurfaceShader>(new AOSurfaceShader(name, params));
}

}   // namespace renderer
