// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/rendering/rendererservices.h"
#include "renderer/kernel/shading/oslshadergroupexec.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/entity/onframebeginrecorder.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/constantenvironmentedf.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentedf/gradientenvironmentedf.h"
#include "renderer/modeling/environmentedf/latlongmapenvironmentedf.h"
#include "renderer/modeling/environmentedf/mirrorballmapenvironmentedf.h"
#include "renderer/modeling/phasefunction/phasefunction.h"
#include "renderer/modeling/phasefunction/henyeyphasefunction.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/testutils.h"

#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/arena.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_PhaseFunction_PhaseFunction)
{
    struct Fixture
        : public TestFixtureBase
    {
        static const int NumberOfSamples = 10000;

        ParamArray base_parameters;
        const HenyeyPhaseFunctionFactory phase_function_factory;

        Fixture()
          : phase_function_factory()
        {
            base_parameters
                .insert("scattering", "0.5")
                .insert("scattering_multiplier", "1.0")
                .insert("extinction", "0.5")
                .insert("extinction_coefficient", "1.0");
        }

        template <typename ReturnType, typename Procedure>
        ReturnType setup_environment_and_evaluate(Procedure procedure)
        {
            bind_inputs();

            TextureStore texture_store(m_scene);
            TextureCache texture_cache(texture_store);

            boost::shared_ptr<OIIO::TextureSystem> texture_system(
                OIIO::TextureSystem::create(),
                boost::bind(&OIIO::TextureSystem::destroy, _1));

            RendererServices renderer_services(
                m_project,
                *texture_system);

            boost::shared_ptr<OSL::ShadingSystem> shading_system(
                new OSL::ShadingSystem(&renderer_services, texture_system.get()));

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

            return procedure(shading_context, arena);
        }


        // Integrate PDF of phase function (direction sampling) using straightforward Monte-Carlo approach
        static float integrate_phase_function_direction_pdf(
            PhaseFunction& phase_function,
            ShadingContext& shading_context,
            Arena& arena)
        {
            void* data = 0;
            ShadingRay shading_ray;
            shading_ray.m_org = Vector3d(0.0f, 0.0f, 0.0f);
            shading_ray.m_dir = Vector3d(1.0f, 0.0f, 0.0f);
            data = phase_function.evaluate_inputs(shading_context, shading_ray);
            phase_function.prepare_inputs(arena, shading_ray, data);

            MersenneTwister rng;
            SamplingContext sampling_context(rng, SamplingContext::RNGMode);

            float integral = 0.0f;
            for (int i = 0; i < NumberOfSamples; ++i)
            {
                sampling_context.split_in_place(2, 1);
                Vector2f s = sampling_context.next2<Vector2f>();
                Vector3f incoming = sample_sphere_uniform<float>(s);
                float value = phase_function.evaluate(shading_ray, data, 0.5f, incoming);
                integral += value;
            }

            return integral * foundation::FourPi<float>() / NumberOfSamples;
        }
    };


    TEST_CASE_F(CheckDirectionPdfIntegratesToOne, Fixture)
    {
        static const char* const g[3] = { "-0.5", "0.0", "0.5" };
        for (int i = 0; i < countof(g); ++i)
        {
            ParamArray parameters = base_parameters;
            parameters.insert("average_cosine", g[i]);

            auto_release_ptr<PhaseFunction> phase_function =
                phase_function_factory.create("phase_function", parameters);

            float integral = setup_environment_and_evaluate<float>(
                boost::bind(
                    &Fixture::integrate_phase_function_direction_pdf,
                    boost::ref(*phase_function.get()), _1, _2));

            EXPECT_FEQ_EPS(1.0f, integral, 1.0e-3f);
        }
    }

}