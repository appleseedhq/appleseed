
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

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/kernel/lighting/null/null.h"
#include "renderer/kernel/lighting/imagebasedlighting.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/nullbsdf.h"
#include "renderer/modeling/bsdf/specularbrdf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/constantenvironmentedf.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/input/colorsource.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/inputparams.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;

FOUNDATION_TEST_SUITE(Renderer_Kernel_Lighting_ImageBasedLighting)
{
    struct Fixture
    {
        Scene                       m_scene;
        TraceContext                m_trace_context;
        const Intersector           m_intersector;
        MersenneTwister             m_rng;
        SamplingContext             m_sampling_context;
        const LightingConditions    m_lighting_conditions;
        TextureCache                m_texture_cache;
        NullLightingEngine          m_lighting_engine;
        ShadingContext              m_shading_context;

        Fixture()
          : m_trace_context(m_scene)
          , m_intersector(m_trace_context, false)
          , m_sampling_context(m_rng)
          , m_lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg)
          , m_texture_cache(m_scene, 1)
          , m_shading_context(
                m_intersector,
                m_sampling_context,
                m_lighting_conditions,
                m_texture_cache,
                m_lighting_engine)
        {
        }

        auto_release_ptr<ColorEntity> create_spectral_color_entity(
            const char*             name,
            const Spectrum&         spectrum)
        {
            ParamArray params;
            params.insert("color_space", "spectral");

            const ColorValueArray values(spectrum.Samples, &spectrum[0]);

            return ColorEntityFactory::create(name, params, values);
        }

        auto_release_ptr<BSDF> create_specular_brdf(
            const char*             name,
            const ColorEntity&      reflectance)
        {
            ParamArray params;
            params.insert("reflectance", "not used");

            auto_release_ptr<BSDF> brdf(SpecularBRDFFactory::create(name, params));

            brdf->get_inputs().find("reflectance").bind(new ColorSource(reflectance));

            return brdf;
        }

        auto_release_ptr<EnvironmentEDF> create_constant_environment_edf(
            const char*             name,
            const ColorEntity&      exitance)
        {
            ParamArray params;
            params.insert("exitance", "not used");

            auto_release_ptr<EnvironmentEDF> env_edf(
                ConstantEnvironmentEDFFactory::create(name, params));

            env_edf->get_inputs().find("exitance").bind(new ColorSource(exitance));

            return env_edf;
        }

        auto_release_ptr<Environment> create_environment(
            const char*             name,
            const EnvironmentEDF*   environment_edf)
        {
            ParamArray params;
            params.insert("environment_edf", "not used");

            return EnvironmentFactory::create(name, environment_edf);
        }
    };

    FOUNDATION_TEST_CASE_WITH_FIXTURE(ComputeImageBasedLighting_GivenSceneWithoutEnvironmentEDF_ReturnsZeroRadiance, Fixture)
    {
        NullBSDF bsdf;
        Spectrum radiance;

        compute_image_based_lighting(
            m_shading_context,
            m_scene,
            Vector3d(0.0),
            Vector3d(0.0, 1.0, 0.0),
            Basis3d(Vector3d(0.0, 1.0, 0.0)),
            Vector3d(0.0, 1.0, 0.0),
            bsdf,
            0,              // BSDF data
            1,              // number of samples in BSDF sampling
            0,              // number of samples in environment sampling
            radiance);

        FOUNDATION_EXPECT_EQ(Spectrum(0.0f), radiance);
    }

    FOUNDATION_TEST_CASE_WITH_FIXTURE(ComputeImageBasedLighting_GivenSpecularBRDFAndUniformWhiteEnrironmentEDF_ReturnsOne, Fixture)
    {
        auto_release_ptr<ColorEntity> gray(
            create_spectral_color_entity("gray", Spectrum(0.5f)));

        auto_release_ptr<ColorEntity> white(
            create_spectral_color_entity("white", Spectrum(1.0f)));

        auto_release_ptr<BSDF> specular_brdf(
            create_specular_brdf("specular_brdf", *gray.get()));

        auto_release_ptr<EnvironmentEDF> env_edf(
            create_constant_environment_edf("constant_environment_edf", *white.get()));

        m_scene.set_environment(
            create_environment("environment", env_edf.get()));

        InputEvaluator input_evaluator(m_texture_cache);
        const void* brdf_data =
            input_evaluator.evaluate(
                specular_brdf->get_inputs(),
                InputParams());

        specular_brdf->on_frame_begin(m_scene, brdf_data);
        env_edf->on_frame_begin(m_scene);

        Spectrum radiance;
        compute_image_based_lighting(
            m_shading_context,
            m_scene,
            Vector3d(0.0),
            Vector3d(0.0, 1.0, 0.0),
            Basis3d(Vector3d(0.0, 1.0, 0.0)),
            Vector3d(0.0, 1.0, 0.0),
            *specular_brdf.get(),
            brdf_data,
            1,              // number of samples in BSDF sampling
            0,              // number of samples in environment sampling
            radiance);

        env_edf->on_frame_end(m_scene);
        specular_brdf->on_frame_end(m_scene);

        FOUNDATION_EXPECT_EQ(Spectrum(0.5f), radiance);
    }
}
