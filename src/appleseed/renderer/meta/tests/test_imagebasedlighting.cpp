
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/kernel/lighting/imagebasedlighting.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/specularbrdf.h"
#include "renderer/modeling/environmentedf/constantenvironmentedf.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/inputparams.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Kernel_Lighting_ImageBasedLighting)
{
    struct Fixture
      : public TestFixtureBase
    {
        const Intersector   m_intersector;
        MersenneTwister     m_rng;
        SamplingContext     m_sampling_context;
        TextureCache        m_texture_cache;
        ShadingContext      m_shading_context;

        Fixture()
          : m_intersector(m_project.get_trace_context(), false)
          , m_sampling_context(m_rng)
          , m_texture_cache(m_scene, 1)
          , m_shading_context(m_intersector, m_texture_cache)
        {
        }
    };

    TEST_CASE_F(ComputeImageBasedLighting_GivenSpecularBRDFAndUniformWhiteEnrironmentEDF, Fixture)
    {
        create_color_entity("gray", Spectrum(0.5f));
        create_color_entity("white", Spectrum(1.0f));

        ParamArray bsdf_params;
        bsdf_params.insert("reflectance", "gray");
        auto_release_ptr<BSDF> specular_brdf(
            SpecularBRDFFactory().create("specular_brdf", bsdf_params));
        BSDF& specular_brdf_ref = specular_brdf.ref();
        m_assembly.bsdfs().insert(specular_brdf);

        ParamArray env_edf_params;
        env_edf_params.insert("exitance", "white");
        auto_release_ptr<EnvironmentEDF> env_edf(
            ConstantEnvironmentEDFFactory().create("env_edf", env_edf_params));
        EnvironmentEDF& env_edf_ref = env_edf.ref();
        m_scene.environment_edfs().insert(env_edf);

        bind_inputs();

        specular_brdf_ref.on_frame_begin(m_project, m_assembly);
        env_edf_ref.on_frame_begin(m_project);

        InputEvaluator input_evaluator(m_texture_cache);
        InputParams input_params;
        specular_brdf_ref.evaluate_inputs(input_evaluator, input_params);
        const void* specular_brdf_data = input_evaluator.data();

        Spectrum radiance;
        compute_image_based_lighting(
            m_sampling_context,
            m_shading_context,
            env_edf_ref,
            Vector3d(0.0),
            Vector3d(0.0, 1.0, 0.0),
            Basis3d(Vector3d(0.0, 1.0, 0.0)),
            0.0,
            Vector3d(0.0, 1.0, 0.0),
            specular_brdf_ref,
            specular_brdf_data,
            1,              // number of samples in BSDF sampling
            0,              // number of samples in environment sampling
            radiance);

        env_edf_ref.on_frame_end(m_project);
        specular_brdf_ref.on_frame_end(m_project, m_assembly);

        EXPECT_EQ(Spectrum(0.5f), radiance);
    }
}
