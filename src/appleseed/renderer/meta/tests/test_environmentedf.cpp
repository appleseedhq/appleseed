
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#ifdef APPLESEED_WITH_OSL
#include "renderer/kernel/rendering/rendererservices.h"
#include "renderer/kernel/shading/oslshadergroupexec.h"
#endif
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
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/texture/texture.h"
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
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/test.h"

// OSL headers.
#ifdef APPLESEED_WITH_OSL
#include "foundation/platform/oslheaderguards.h"
BEGIN_OSL_INCLUDES
#include "OSL/oslexec.h"
END_OSL_INCLUDES
#endif

// OpenImageIO headers.
#ifdef APPLESEED_WITH_OIIO
#include "foundation/platform/oiioheaderguards.h"
BEGIN_OIIO_INCLUDES
#include "OpenImageIO/texture.h"
END_OIIO_INCLUDES
#endif

// Boost headers.
#include "boost/bind.hpp"
#include "boost/shared_ptr.hpp"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <memory>

using namespace foundation;
using namespace renderer;
using namespace std;

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

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return "horizontal_gradient_texture";
        }

        virtual ColorSpace get_color_space() const APPLESEED_OVERRIDE
        {
            return ColorSpaceLinearRGB;
        }

        virtual const CanvasProperties& properties() APPLESEED_OVERRIDE
        {
            return m_props;
        }

        virtual Tile* load_tile(
            const size_t    tile_x,
            const size_t    tile_y) APPLESEED_OVERRIDE
        {
            assert(tile_x == 0);
            assert(tile_y == 0);

            return m_tile.get();
        }

        virtual void unload_tile(
            const size_t    tile_x,
            const size_t    tile_y,
            const Tile*     tile) APPLESEED_OVERRIDE
        {
        }

      private:
        const CanvasProperties  m_props;
        auto_ptr<Tile>          m_tile;

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

    struct Fixture
      : public TestFixtureBase
    {
        void create_horizontal_gradient_texture(const char* name)
        {
            m_scene.textures().insert(
                auto_release_ptr<Texture>(
                    new HorizontalGradientTexture(name)));
        }

        bool check_consistency(EnvironmentEDF& env_edf)
        {
            auto_release_ptr<Environment> environment(
                EnvironmentFactory().create(
                    "environment", ParamArray().insert("environment_edf", env_edf.get_name())));

            m_scene.set_environment(environment);

            bind_inputs();

            OnFrameBeginRecorder recorder;
            APPLESEED_UNUSED const bool success = env_edf.on_frame_begin(m_project, &m_scene, recorder);
            assert(success);

            TextureStore texture_store(m_scene);
            TextureCache texture_cache(texture_store);

#ifdef APPLESEED_WITH_OIIO
            boost::shared_ptr<OIIO::TextureSystem> texture_system(
                OIIO::TextureSystem::create(),
                boost::bind(&OIIO::TextureSystem::destroy, _1));
#endif

#ifdef APPLESEED_WITH_OSL
            RendererServices renderer_services(
                m_project,
                *texture_system);

            boost::shared_ptr<OSL::ShadingSystem> shading_system(
                new OSL::ShadingSystem(&renderer_services, texture_system.get()));
#endif

            Intersector intersector(
                m_project.get_trace_context(),
                texture_cache);

#ifdef APPLESEED_WITH_OSL
            OSLShaderGroupExec sg_exec(*shading_system);
#endif

            Tracer tracer(
                m_scene,
                intersector,
                texture_cache
#ifdef APPLESEED_WITH_OSL
                , sg_exec
#endif
                );

            ShadingContext shading_context(
                intersector,
                tracer,
                texture_cache,
#ifdef APPLESEED_WITH_OIIO
                *texture_system,
#endif
#ifdef APPLESEED_WITH_OSL
                sg_exec,
#endif
                0);

            InputEvaluator input_evaluator(texture_cache);

            Vector3f outgoing;
            Spectrum value1;
            float probability1;

            env_edf.sample(
                shading_context,
                input_evaluator,
                Vector2f(0.3f, 0.7f),
                outgoing,
                value1,
                probability1);

            Spectrum value2;

            env_edf.evaluate(
                shading_context,
                input_evaluator,
                outgoing,
                value2);

            const float probability2 =
                env_edf.evaluate_pdf(
                    input_evaluator,
                    outgoing);

            recorder.on_frame_end(m_project);

            const bool consistent =
                feq(probability1, probability2) &&
                feq(value1, value2);

            return consistent;
        }
    };

    TEST_CASE_F(CheckConstantEnvironmentEDFConsistency, Fixture)
    {
        create_color_entity("blue", Color3f(0.2f, 0.5f, 0.9f));

        auto_release_ptr<EnvironmentEDF> env_edf(
            ConstantEnvironmentEDFFactory().create(
                "env_edf",
                ParamArray().insert("radiance", "blue")));
        EnvironmentEDF& env_edf_ref = env_edf.ref();
        m_scene.environment_edfs().insert(env_edf);

        const bool consistent = check_consistency(env_edf_ref);

        EXPECT_TRUE(consistent);
    }

    TEST_CASE_F(CheckGradientEnvironmentEDFConsistency, Fixture)
    {
        create_color_entity("red", Color3f(1.0f, 0.2f, 0.2f));
        create_color_entity("green", Color3f(0.2f, 1.0f, 0.2f));

        auto_release_ptr<EnvironmentEDF> env_edf(
            GradientEnvironmentEDFFactory().create(
                "env_edf",
                ParamArray()
                    .insert("horizon_radiance", "red")
                    .insert("zenith_radiance", "green")));
        EnvironmentEDF& env_edf_ref = env_edf.ref();
        m_scene.environment_edfs().insert(env_edf);

        const bool consistent = check_consistency(env_edf_ref);

        EXPECT_TRUE(consistent);
    }

    TEST_CASE_F(CheckLatLongMapEnvironmentEDFConsistency, Fixture)
    {
        create_horizontal_gradient_texture("horiz_gradient_texture");
        create_texture_instance("horiz_gradient_texture_inst", "horiz_gradient_texture");

        auto_release_ptr<EnvironmentEDF> env_edf(
            LatLongMapEnvironmentEDFFactory().create(
                "env_edf",
                ParamArray().insert("radiance", "horiz_gradient_texture_inst")));
        EnvironmentEDF& env_edf_ref = env_edf.ref();
        m_scene.environment_edfs().insert(env_edf);

        const bool consistent = check_consistency(env_edf_ref);

        EXPECT_TRUE(consistent);
    }

    TEST_CASE_F(CheckMirrorBallMapEnvironmentEDFConsistency, Fixture)
    {
        create_horizontal_gradient_texture("horiz_gradient_texture");
        create_texture_instance("horiz_gradient_texture_inst", "horiz_gradient_texture");

        auto_release_ptr<EnvironmentEDF> env_edf(
            MirrorBallMapEnvironmentEDFFactory().create(
                "env_edf",
                ParamArray().insert("radiance", "horiz_gradient_texture_inst")));
        EnvironmentEDF& env_edf_ref = env_edf.ref();
        m_scene.environment_edfs().insert(env_edf);

        const bool consistent = check_consistency(env_edf_ref);

        EXPECT_TRUE(consistent);
    }
}
