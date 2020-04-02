
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/constantenvironmentedf.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentedf/gradientenvironmentedf.h"
#include "renderer/modeling/environmentedf/latlongmapenvironmentedf.h"
#include "renderer/modeling/environmentedf/mirrorballmapenvironmentedf.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/modeling/texture/tileptr.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/memory/arena.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/test.h"
#include "foundation/utility/uid.h"

// OSL headers.
#include "foundation/platform/_beginoslheaders.h"
#include "OSL/oslexec.h"
#include "foundation/platform/_endoslheaders.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/texture.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <memory>

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_EnvironmentEDF)
{
    class HorizontalGradientTexture
      : public Texture
    {
      public:
        HorizontalGradientTexture(const char* name)
          : Texture(name, ParamArray())
          , m_props(
                5, 5,
                5, 5,
                3,
                PixelFormatFloat)
        {
            m_tile.reset(
                new Tile(
                    m_props.m_canvas_width,
                    m_props.m_canvas_height,
                    m_props.m_channel_count,
                    m_props.m_pixel_format));

            create_horizontal_gradient();
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return "horizontal_gradient_texture";
        }

        ColorSpace get_color_space() const override
        {
            return ColorSpaceLinearRGB;
        }

        const CanvasProperties& properties() override
        {
            return m_props;
        }

        Source* create_source(
            const UniqueID          assembly_uid,
            const TextureInstance&  texture_instance) override
        {
            return new TextureSource(assembly_uid, texture_instance);
        }

        TilePtr load_tile(
            const size_t            tile_x,
            const size_t            tile_y) override
        {
            assert(tile_x == 0);
            assert(tile_y == 0);
            return TilePtr::make_non_owning(m_tile.get());
        }

      private:
        const CanvasProperties  m_props;
        std::unique_ptr<Tile>   m_tile;

        void create_horizontal_gradient()
        {
            for (size_t y = 0; y < m_props.m_canvas_height; ++y)
            {
                for (size_t x = 0; x < m_props.m_canvas_width; ++x)
                {
                    const float intensity =
                        static_cast<float>(x) / (m_props.m_canvas_width - 1);

                    m_tile->set_pixel(x, y, Color3f(intensity));
                }
            }
        }
    };

    template <typename TestScene>
    struct Fixture
      : public StaticTestSceneContext<TestScene>
    {
        typedef StaticTestSceneContext<TestScene> Base;

        TextureStore                        m_texture_store;
        TextureCache                        m_texture_cache;
        std::shared_ptr<OIIOTextureSystem>  m_texture_system;
        RendererServices                    m_renderer_services;
        std::shared_ptr<OSLShadingSystem>   m_shading_system;
        Intersector                         m_intersector;
        Arena                               m_arena;
        OSLShaderGroupExec                  m_sg_exec;
        Tracer                              m_tracer;
        ShadingContext                      m_shading_context;

        Fixture()
          : m_texture_store(Base::m_scene)
          , m_texture_cache(m_texture_store)
          , m_texture_system(
                OIIOTextureSystemFactory::create(),
                [](OIIOTextureSystem* object) { object->release(); })
          , m_renderer_services(Base::m_project, *m_texture_system)
          , m_shading_system(
                OSLShadingSystemFactory::create(&m_renderer_services, m_texture_system.get()),
                [](OSLShadingSystem* object) { object->release(); })
          , m_intersector(
                Base::m_project.get_trace_context(),
                m_texture_cache)
          , m_sg_exec(*m_shading_system, m_arena)
          , m_tracer(
                Base::m_scene,
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

        bool check_consistency(const EnvironmentEDF& env_edf) const
        {
            Vector3f outgoing;
            Spectrum value1(Spectrum::Illuminance);
            float probability1;
            env_edf.sample(
                m_shading_context,
                Vector2f(0.3f, 0.7f),
                outgoing,
                value1,
                probability1);

            Spectrum value2(Spectrum::Illuminance);
            env_edf.evaluate(m_shading_context, outgoing, value2);
            const float probability2 = env_edf.evaluate_pdf(outgoing);

            return
                feq(probability1, probability2) &&
                feq(value1, value2);
        }
    };

    struct ConstantEnvironmentEDFTestScene
      : public TestSceneBase
    {
        EnvironmentEDF* m_env_edf;

        ConstantEnvironmentEDFTestScene()
        {
            create_color_entity("blue", Color3f(0.2f, 0.5f, 0.9f));

            auto_release_ptr<EnvironmentEDF> env_edf(
                ConstantEnvironmentEDFFactory().create(
                    "env_edf",
                    ParamArray().insert("radiance", "blue")));
            m_env_edf = env_edf.get();
            m_scene.environment_edfs().insert(env_edf);

            m_scene.set_environment(
                EnvironmentFactory().create(
                    "environment",
                    ParamArray().insert("environment_edf", m_env_edf->get_name())));
        }
    };

    TEST_CASE_F(CheckConstantEnvironmentEDFConsistency, Fixture<ConstantEnvironmentEDFTestScene>)
    {
        EXPECT_TRUE(check_consistency(*m_env_edf));
    }

    struct GradientEnvironmentEDFTestScene
      : public TestSceneBase
    {
        EnvironmentEDF* m_env_edf;

        GradientEnvironmentEDFTestScene()
        {
            create_color_entity("red", Color3f(1.0f, 0.2f, 0.2f));
            create_color_entity("green", Color3f(0.2f, 1.0f, 0.2f));

            auto_release_ptr<EnvironmentEDF> env_edf(
                GradientEnvironmentEDFFactory().create(
                    "env_edf",
                    ParamArray()
                        .insert("horizon_radiance", "red")
                        .insert("zenith_radiance", "green")));
            m_env_edf = env_edf.get();
            m_scene.environment_edfs().insert(env_edf);

            m_scene.set_environment(
                EnvironmentFactory().create(
                    "environment",
                    ParamArray().insert("environment_edf", m_env_edf->get_name())));
        }
    };

    TEST_CASE_F(CheckGradientEnvironmentEDFConsistency, Fixture<GradientEnvironmentEDFTestScene>)
    {
        EXPECT_TRUE(check_consistency(*m_env_edf));
    }

    struct LatLongMapEnvironmentEDFTestScene
      : public TestSceneBase
    {
        EnvironmentEDF* m_env_edf;

        LatLongMapEnvironmentEDFTestScene()
        {
            m_scene.textures().insert(
                auto_release_ptr<Texture>(
                    new HorizontalGradientTexture("horiz_gradient_texture")));
            create_texture_instance("horiz_gradient_texture_inst", "horiz_gradient_texture");

            auto_release_ptr<EnvironmentEDF> env_edf(
                LatLongMapEnvironmentEDFFactory().create(
                    "env_edf",
                    ParamArray().insert("radiance", "horiz_gradient_texture_inst")));
            m_env_edf = env_edf.get();
            m_scene.environment_edfs().insert(env_edf);

            m_scene.set_environment(
                EnvironmentFactory().create(
                    "environment",
                    ParamArray().insert("environment_edf", m_env_edf->get_name())));
        }
    };

    TEST_CASE_F(CheckLatLongMapEnvironmentEDFConsistency, Fixture<LatLongMapEnvironmentEDFTestScene>)
    {
        EXPECT_TRUE(check_consistency(*m_env_edf));
    }

    struct MirrorBallMapEnvironmentEDFTestScene
      : public TestSceneBase
    {
        EnvironmentEDF* m_env_edf;

        MirrorBallMapEnvironmentEDFTestScene()
        {
            m_scene.textures().insert(
                auto_release_ptr<Texture>(
                    new HorizontalGradientTexture("horiz_gradient_texture")));
            create_texture_instance("horiz_gradient_texture_inst", "horiz_gradient_texture");

            auto_release_ptr<EnvironmentEDF> env_edf(
                MirrorBallMapEnvironmentEDFFactory().create(
                    "env_edf",
                    ParamArray().insert("radiance", "horiz_gradient_texture_inst")));
            m_env_edf = env_edf.get();
            m_scene.environment_edfs().insert(env_edf);

            m_scene.set_environment(
                EnvironmentFactory().create(
                    "environment",
                    ParamArray().insert("environment_edf", m_env_edf->get_name())));
        }
    };

    TEST_CASE_F(CheckMirrorBallMapEnvironmentEDFConsistency, Fixture<MirrorBallMapEnvironmentEDFTestScene>)
    {
        EXPECT_TRUE(check_consistency(*m_env_edf));
    }
}
