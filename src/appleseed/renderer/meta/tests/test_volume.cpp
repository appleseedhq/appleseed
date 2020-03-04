
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Artem Bishev, The appleseedhq Organization
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/rendering/rendererservices.h"
#include "renderer/kernel/shading/oslshadergroupexec.h"
#include "renderer/kernel/shading/oslshadingsystem.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/texturing/oiiotexturesystem.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/entity/onframebeginrecorder.h"
#include "renderer/modeling/input/scalarsource.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/modeling/volume/genericvolume.h"
#include "renderer/modeling/volume/volume.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/memory/arena.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <memory>
#include <vector>

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_Volume)
{
    struct VolumeTestSceneContext
      : public TestSceneContext
    {
        // Number of MC samples for integration.
        static const size_t NumberOfSamples = 50000;

        // Number of samples for plots.
        static const size_t NumberOfSamplesPlot = 100;

        TextureStore                         m_texture_store;
        TextureCache                         m_texture_cache;
        std::shared_ptr<OIIOTextureSystem>   m_texture_system;
        RendererServices                     m_renderer_services;
        std::shared_ptr<OSLShadingSystem>    m_shading_system;
        Intersector                          m_intersector;
        Arena                                m_arena;
        OSLShaderGroupExec                   m_sg_exec;
        Tracer                               m_tracer;
        ShadingContext                       m_shading_context;

        explicit VolumeTestSceneContext(TestSceneBase& base)
          : TestSceneContext(base)
          , m_texture_store(base.m_scene)
          , m_texture_cache(m_texture_store)
          , m_texture_system(
              OIIOTextureSystemFactory::create(),
              [](OIIOTextureSystem* object) { object->release(); })
          , m_renderer_services(base.m_project, *m_texture_system)
          , m_shading_system(
              OSLShadingSystemFactory::create(&m_renderer_services, m_texture_system.get()),
              [](OSLShadingSystem* object) { object->release(); })
          , m_intersector(
              base.m_project.get_trace_context(),
              m_texture_cache)
          , m_sg_exec(*m_shading_system, m_arena)
          , m_tracer(
              base.m_scene,
              m_intersector,
              m_sg_exec)
          , m_shading_context(
              m_intersector,
              m_tracer,
              m_texture_cache,
              *m_texture_system,
              m_sg_exec,
              m_arena,
              0)  // thread index
        {
        }

        // Integrate PDF of volume using straightforward Monte Carlo approach.
        float integrate_volume_pdf(const Volume& volume)
        {
            ShadingRay shading_ray;
            shading_ray.m_org = Vector3d(0.0f, 0.0f, 0.0f);
            shading_ray.m_dir = Vector3d(1.0f, 0.0f, 0.0f);
            void* data = volume.evaluate_inputs(m_shading_context, shading_ray);
            volume.prepare_inputs(m_arena, shading_ray, data);

            SamplingContext::RNGType rng;
            SamplingContext sampling_context(rng, SamplingContext::RNGMode, 2, NumberOfSamples);

            float integral = 0.0f;
            for (size_t i = 0; i < NumberOfSamples; ++i)
            {
                const Vector2f s = sampling_context.next2<Vector2f>();
                const Vector3f incoming = sample_sphere_uniform<float>(s);
                integral += volume.evaluate(data, shading_ray, 0.5f, incoming);
            }

            return integral * foundation::FourPi<float>() / NumberOfSamples;
        }

        // Check if probabilistic sampling is consistent with the returned PDF values.
        Vector3f get_sampling_bias(const Volume& volume)
        {
            ShadingRay shading_ray;
            shading_ray.m_org = Vector3d(0.0f, 0.0f, 0.0f);
            shading_ray.m_dir = Vector3d(1.0f, 0.0f, 0.0f);

            void* data = volume.evaluate_inputs(m_shading_context, shading_ray);
            volume.prepare_inputs(m_arena, shading_ray, data);

            SamplingContext::RNGType rng;
            SamplingContext sampling_context(rng, SamplingContext::RNGMode);

            Vector3f bias(0.0f);
            for (size_t i = 0; i < NumberOfSamples; ++i)
            {
                Vector3f incoming;
                const float pdf = volume.sample(
                    sampling_context, data, shading_ray, 0.5f, incoming);
                bias += incoming / pdf;
            }

            return bias / static_cast<float>(NumberOfSamples);
        }

        // Sample a given volume and find average cosine of scattering angle.
        float get_aposteriori_average_cosine(const Volume& volume)
        {
            ShadingRay shading_ray;
            shading_ray.m_org = Vector3d(0.0f, 0.0f, 0.0f);
            shading_ray.m_dir = Vector3d(1.0f, 0.0f, 0.0f);

            void* data = volume.evaluate_inputs(m_shading_context, shading_ray);
            volume.prepare_inputs(m_arena, shading_ray, data);

            SamplingContext::RNGType rng;
            SamplingContext sampling_context(rng, SamplingContext::RNGMode);

            Vector3f bias(0.0f);
            for (size_t i = 0; i < NumberOfSamples; ++i)
            {
                Vector3f incoming;
                volume.sample(
                    sampling_context, data, shading_ray, 0.5f, incoming);
                bias += incoming;
            }

            return bias.x / NumberOfSamples;
        }

        std::vector<Vector2f> generate_samples_for_plot(const Volume& volume)
        {
            ShadingRay shading_ray;
            shading_ray.m_org = Vector3d(0.0f, 0.0f, 0.0f);
            shading_ray.m_dir = Vector3d(1.0f, 0.0f, 0.0f);

            void* data = volume.evaluate_inputs(m_shading_context, shading_ray);
            volume.prepare_inputs(m_arena, shading_ray, data);

            SamplingContext::RNGType rng;
            SamplingContext sampling_context(rng, SamplingContext::RNGMode);

            std::vector<Vector2f> points;
            points.reserve(NumberOfSamples);
            for (size_t i = 0; i < NumberOfSamplesPlot; ++i)
            {
                Vector3f incoming;
                const float pdf = volume.sample(
                    sampling_context, data, shading_ray, 0.5f, incoming);
                points.emplace_back(incoming.x * pdf, incoming.y * pdf);
            }

            return points;
        }
    };

    TEST_CASE(CheckHenyeyPdfIntegratesToOne)
    {
        static const float G[4] = { -0.5f, 0.0f, +0.3f, +0.8f };

        for (size_t i = 0; i < countof(G); ++i)
        {
            TestSceneBase test_scene;

            auto_release_ptr<Assembly> assembly(
                AssemblyFactory().create("assembly", ParamArray()));

            auto_release_ptr<Volume> volume =
                GenericVolumeFactory().create("volume",
                    ParamArray()
                        .insert("absorption", 0.5f)
                        .insert("scattering", 0.5f)
                        .insert("phase_function_model", "henyey")
                        .insert("average_cosine", G[i]));
            Volume& volume_ref = volume.ref();
            assembly->volumes().insert(volume);

            test_scene.m_scene.assemblies().insert(assembly);

            VolumeTestSceneContext context(test_scene);
            const float integral = context.integrate_volume_pdf(volume_ref);

            EXPECT_FEQ_EPS(1.0f, integral, 0.05f);
        }
    }

    TEST_CASE(CheckHenyeySamplingConsistency)
    {
        static const float G[4] = { -0.5f, 0.0f, +0.3f, +0.8f };

        for (size_t i = 0; i < countof(G); ++i)
        {
            TestSceneBase test_scene;

            auto_release_ptr<Assembly> assembly(
                AssemblyFactory().create("assembly", ParamArray()));

            auto_release_ptr<Volume> volume =
                GenericVolumeFactory().create("volume",
                    ParamArray()
                        .insert("absorption", 0.5f)
                        .insert("scattering", 0.5f)
                        .insert("phase_function_model", "henyey")
                        .insert("average_cosine", G[i]));
            Volume& volume_ref = volume.ref();
            assembly->volumes().insert(volume);

            test_scene.m_scene.assemblies().insert(assembly);

            VolumeTestSceneContext context(test_scene);
            const Vector3f bias = context.get_sampling_bias(volume_ref);

            EXPECT_FEQ_EPS(bias, Vector3f(0.0f), 0.2f);
        }
    }

    TEST_CASE(CheckHenyeyAverageCosine)
    {
        static const float G[4] = { -0.5f, 0.0f, +0.3f, +0.8f };

        for (size_t i = 0; i < countof(G); ++i)
        {
            TestSceneBase test_scene;

            auto_release_ptr<Assembly> assembly(
                AssemblyFactory().create("assembly", ParamArray()));

            auto_release_ptr<Volume> volume =
                GenericVolumeFactory().create("volume",
                    ParamArray()
                        .insert("absorption", 0.5f)
                        .insert("scattering", 0.5f)
                        .insert("phase_function_model", "henyey")
                        .insert("average_cosine", G[i]));
            Volume& volume_ref = volume.ref();
            assembly->volumes().insert(volume);

            test_scene.m_scene.assemblies().insert(assembly);

            VolumeTestSceneContext context(test_scene);
            const float average_cosine = context.get_aposteriori_average_cosine(volume_ref);

            EXPECT_FEQ_EPS(G[i], average_cosine, 0.05f);
        }
    }

    TEST_CASE(PlotHenyeySamples)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Samples of Henyey-Greenstein phase function (multiplied by PDF)");
        plotfile.set_xlabel("X");
        plotfile.set_ylabel("Y");
        plotfile.set_xrange(-0.6, +0.6);
        plotfile.set_yrange(-0.3, +0.3);

        static const char* Colors[3] = { "blue", "red", "magenta" };
        static const float G[3] = { -0.5f, +0.3f, 0.0f };

        for (size_t i = 0; i < countof(G); ++i)
        {
            TestSceneBase test_scene;

            auto_release_ptr<Assembly> assembly(
                AssemblyFactory().create("assembly", ParamArray()));

            auto_release_ptr<Volume> volume =
                GenericVolumeFactory().create("volume",
                    ParamArray()
                        .insert("absorption", 0.5f)
                        .insert("scattering", 0.5f)
                        .insert("phase_function_model", "henyey")
                        .insert("average_cosine", G[i]));
            Volume& volume_ref = volume.ref();
            assembly->volumes().insert(volume);

            test_scene.m_scene.assemblies().insert(assembly);

            VolumeTestSceneContext context(test_scene);
            const std::vector<Vector2f> points = context.generate_samples_for_plot(volume_ref);

            plotfile
                .new_plot()
                .set_points(points)
                .set_title("G = " + to_string(G[i]))
                .set_color(Colors[i])
                .set_style("points");
        }

        plotfile.write("unit tests/outputs/test_volume_henyey_samples.gnuplot");
    }
}
