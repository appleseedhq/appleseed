
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/environmentedf/constantenvironmentedf.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentedf/gradientenvironmentedf.h"
#include "renderer/modeling/environmentedf/latlongmapenvironmentedf.h"
#include "renderer/modeling/environmentedf/mirrorballmapenvironmentedf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/tile.h"
#include "foundation/utility/test.h"

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

        virtual void release() override
        {
            delete this;
        }

        virtual const char* get_model() const override
        {
            return "horizontal_gradient_texture";
        }

        virtual ColorSpace get_color_space() const override
        {
            return ColorSpaceLinearRGB;
        }

        virtual const CanvasProperties& properties() override
        {
            return m_props;
        }

        virtual Tile* load_tile(
            const size_t    tile_x,
            const size_t    tile_y) override
        {
            assert(tile_x == 0);
            assert(tile_y == 0);

            return m_tile.get();
        }

        virtual void unload_tile(
            const size_t    tile_x,
            const size_t    tile_y,
            const Tile*     tile) override
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
        size_t create_horizontal_gradient_texture(const char* name)
        {
            return
                m_scene.textures().insert(
                    auto_release_ptr<Texture>(
                        new HorizontalGradientTexture(name)));
        }

        bool check_consistency(EnvironmentEDF& env_edf)
        {
            bind_inputs();

            env_edf.on_frame_begin(m_project);

            TextureStore texture_store(m_scene);
            TextureCache texture_cache(texture_store);
            InputEvaluator input_evaluator(texture_cache);

            Vector3d outgoing;
            Spectrum value1;
            double probability1;

            env_edf.sample(
                input_evaluator,
                Vector2d(0.3, 0.7),
                outgoing,
                value1,
                probability1);

            Spectrum value2;
            
            env_edf.evaluate(
                input_evaluator,
                outgoing,
                value2);

            const double probability2 =
                env_edf.evaluate_pdf(
                    input_evaluator,
                    outgoing);

            env_edf.on_frame_end(m_project);

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
                ParamArray().insert("exitance", "blue")));
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
                    .insert("horizon_exitance", "red")
                    .insert("zenith_exitance", "green")));
        EnvironmentEDF& env_edf_ref = env_edf.ref();
        m_scene.environment_edfs().insert(env_edf);

        const bool consistent = check_consistency(env_edf_ref);

        EXPECT_TRUE(consistent);
    }

    TEST_CASE_F(CheckLatLongMapEnvironmentEDFConsistency, Fixture)
    {
        const size_t texture_index =
            create_horizontal_gradient_texture("horiz_gradient_texture");

        create_texture_instance("horiz_gradient_texture_inst", texture_index);

        auto_release_ptr<EnvironmentEDF> env_edf(
            LatLongMapEnvironmentEDFFactory().create(
                "env_edf",
                ParamArray().insert("exitance", "horiz_gradient_texture_inst")));
        EnvironmentEDF& env_edf_ref = env_edf.ref();
        m_scene.environment_edfs().insert(env_edf);

        const bool consistent = check_consistency(env_edf_ref);

        EXPECT_TRUE(consistent);
    }

    TEST_CASE_F(CheckMirrorBallMapEnvironmentEDFConsistency, Fixture)
    {
        const size_t texture_index =
            create_horizontal_gradient_texture("horiz_gradient_texture");

        create_texture_instance("horiz_gradient_texture_inst", texture_index);

        auto_release_ptr<EnvironmentEDF> env_edf(
            MirrorBallMapEnvironmentEDFFactory().create(
                "env_edf",
                ParamArray().insert("exitance", "horiz_gradient_texture_inst")));
        EnvironmentEDF& env_edf_ref = env_edf.ref();
        m_scene.environment_edfs().insert(env_edf);

        const bool consistent = check_consistency(env_edf_ref);

        EXPECT_TRUE(consistent);
    }
}
