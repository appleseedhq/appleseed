
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Artem Bishev, The appleseedhq Organization
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

// We are using standard library in this compile unit
// and don't need boost placeholders.
#define BOOST_BIND_NO_PLACEHOLDERS

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
#include "foundation/utility/arena.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <functional>
#include <memory>
#include <utility>
#include <vector>

using namespace foundation;
using namespace renderer;
using namespace std::placeholders;

TEST_SUITE(Renderer_Modeling_Volume)
{
    struct Fixture
      : public TestFixtureBase
    {
        // Number of MC samples to do integrations.
        static const size_t NumberOfSamples = 50000;

        // Number of samples to draw.
        static const size_t NumberOfSamplesPlot = 100;

        GenericVolumeFactory m_volume_factory;

        template <typename Procedure>
        typename std::result_of<Procedure(ShadingContext&, Arena&)>::type
            setup_environment_and_evaluate(Procedure procedure, Volume& volume)
        {
            TextureStore texture_store(m_scene);
            TextureCache texture_cache(texture_store);

            std::shared_ptr<OIIOTextureSystem> texture_system(
                OIIOTextureSystemFactory::create(),
                [](OIIOTextureSystem* object) { object->release(); });

            RendererServices renderer_services(
                m_project,
                *texture_system);

            std::shared_ptr<OSLShadingSystem> shading_system(
                OSLShadingSystemFactory::create(&renderer_services, texture_system.get()),
                [](OSLShadingSystem* object) { object->release(); });

            Intersector intersector(
                m_project.get_trace_context(),
                texture_cache);

            Arena arena;
            OSLShaderGroupExec sg_exec(*shading_system, arena);

            Tracer tracer(
                m_scene,
                intersector,
                texture_cache,
                sg_exec);

            ShadingContext shading_context(
                intersector,
                tracer,
                texture_cache,
                *texture_system,
                sg_exec,
                arena,
                0);

            OnFrameBeginRecorder recorder;
            APPLESEED_UNUSED const bool success = volume.on_frame_begin(m_project, &m_scene, recorder);
            assert(success);

            auto result = procedure(shading_context, arena);

            recorder.on_frame_end(m_project);

            return result;
        }

        // Integrate PDF of volume using straightforward Monte-Carlo approach.
        static float integrate_volume_pdf(
            Volume&             volume,
            ShadingContext&     shading_context,
            Arena&              arena)
        {
            ShadingRay shading_ray;
            shading_ray.m_org = Vector3d(0.0f, 0.0f, 0.0f);
            shading_ray.m_dir = Vector3d(1.0f, 0.0f, 0.0f);
            void* data = volume.evaluate_inputs(shading_context, shading_ray);
            volume.prepare_inputs(arena, shading_ray, data);

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
        static bool check_sampling_consistency(
            Volume&             volume,
            ShadingContext&     shading_context,
            Arena&              arena)
        {
            ShadingRay shading_ray;
            shading_ray.m_org = Vector3d(0.0f, 0.0f, 0.0f);
            shading_ray.m_dir = Vector3d(1.0f, 0.0f, 0.0f);
            void* data = volume.evaluate_inputs(shading_context, shading_ray);
            volume.prepare_inputs(arena, shading_ray, data);

            SamplingContext::RNGType rng;
            SamplingContext sampling_context(rng, SamplingContext::RNGMode);

            Vector3f bias = Vector3f(0.0f);
            for (size_t i = 0; i < NumberOfSamples; ++i)
            {
                Vector3f incoming;
                const float pdf = volume.sample(
                    sampling_context, data, shading_ray, 0.5f, incoming);
                bias += incoming / pdf;
            }

            return feq(bias / static_cast<float>(NumberOfSamples), Vector3f(0.0f), 0.1f);
        }

        // Sample a given volume and find average cosine of scattering angle.
        static float get_aposteriori_average_cosine(
            Volume&             volume,
            ShadingContext&     shading_context,
            Arena&              arena)
        {
            ShadingRay shading_ray;
            shading_ray.m_org = Vector3d(0.0f, 0.0f, 0.0f);
            shading_ray.m_dir = Vector3d(1.0f, 0.0f, 0.0f);
            void* data = volume.evaluate_inputs(shading_context, shading_ray);
            volume.prepare_inputs(arena, shading_ray, data);

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

        static std::vector<Vector2f> generate_samples_for_plot(
            Volume& volume,
            ShadingContext& shading_context,
            Arena& arena)
        {
            ShadingRay shading_ray;
            shading_ray.m_org = Vector3d(0.0f, 0.0f, 0.0f);
            shading_ray.m_dir = Vector3d(1.0f, 0.0f, 0.0f);
            void* data = volume.evaluate_inputs(shading_context, shading_ray);
            volume.prepare_inputs(arena, shading_ray, data);

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

    TEST_CASE_F(CheckHenyeyPdfIntegratesToOne, Fixture)
    {
        static const float G[4] = { -0.5f, 0.0f, +0.3f, +0.8f };

        for (size_t i = 0; i < countof(G); ++i)
        {
            auto_release_ptr<Volume> volume =
                m_volume_factory.create("volume",
                    ParamArray()
                        .insert("phase_function_model", "henyey")
                        .insert("average_cosine", G[i]));
            volume->get_inputs().find("average_cosine").bind(new ScalarSource(G[i]));

            const float integral = setup_environment_and_evaluate(
                std::bind(&Fixture::integrate_volume_pdf,
                    std::ref(*volume.get()), _1, _2), *volume.get());

            EXPECT_FEQ_EPS(1.0f, integral, 0.05f);
        }
    }

    TEST_CASE_F(CheckHenyeySamplingConsistency, Fixture)
    {
        static const float G[4] = { -0.5f, 0.0f, +0.3f, +0.8f };

        for (size_t i = 0; i < countof(G); ++i)
        {
            auto_release_ptr<Volume> volume =
                m_volume_factory.create("volume",
                    ParamArray()
                        .insert("phase_function_model", "henyey")
                        .insert("average_cosine", G[i]));
            volume->get_inputs().find("average_cosine").bind(new ScalarSource(G[i]));

            const bool consistent = setup_environment_and_evaluate(
                std::bind(&Fixture::check_sampling_consistency,
                    std::ref(*volume.get()), _1, _2), *volume.get());

            EXPECT_TRUE(consistent);
        }
    }

    TEST_CASE_F(CheckHenyeyAverageCosine, Fixture)
    {
        static const float G[4] = { -0.5f, 0.0f, +0.3f, +0.8f };

        for (size_t i = 0; i < countof(G); ++i)
        {
            auto_release_ptr<Volume> volume =
                m_volume_factory.create("volume",
                    ParamArray()
                        .insert("phase_function_model", "henyey")
                        .insert("average_cosine", G[i]));
            volume->get_inputs().find("average_cosine").bind(new ScalarSource(G[i]));

            const float average_cosine = setup_environment_and_evaluate(
                std::bind(&Fixture::get_aposteriori_average_cosine,
                    std::ref(*volume.get()), _1, _2), *volume.get());

            EXPECT_FEQ_EPS(G[i], average_cosine, 0.05f);
        }
    }

    TEST_CASE_F(PlotHenyeySamples, Fixture)
    {
        GnuplotFile plotfile;
        plotfile.set_title(
            "Samples of Henyey-Greenstein phase function"
            " (multiplied by PDF)");
        plotfile.set_xlabel("X");
        plotfile.set_ylabel("Y");
        plotfile.set_xrange(-0.6, +0.6);
        plotfile.set_yrange(-0.3, +0.3);

        static const char* Colors[3] = { "blue", "red", "magenta" };
        static const float G[3] = { -0.5f, +0.3f, 0.0f };

        for (size_t i = 0; i < countof(G); ++i)
        {
            auto_release_ptr<Volume> volume =
                m_volume_factory.create("volume",
                    ParamArray()
                        .insert("phase_function_model", "henyey")
                        .insert("average_cosine", G[i]));
            volume->get_inputs().find("average_cosine").bind(new ScalarSource(G[i]));

            const std::vector<Vector2f> points =
                setup_environment_and_evaluate(
                    std::bind(&Fixture::generate_samples_for_plot,
                        std::ref(*volume.get()), _1, _2), *volume.get());

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
