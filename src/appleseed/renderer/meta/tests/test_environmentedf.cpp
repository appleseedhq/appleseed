
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
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/environmentedf/constantenvironmentedf.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentedf/gradientenvironmentedf.h"
#include "renderer/modeling/environmentedf/latlongmapenvironmentedf.h"
#include "renderer/modeling/environmentedf/mirrorballmapenvironmentedf.h"
#include "renderer/modeling/input/colorsource.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/utility/test.h"

FOUNDATION_TEST_SUITE(Renderer_Modeling_EnvironmentEDF)
{
    using namespace foundation;
    using namespace renderer;
    using namespace std;

    bool check_consistency(Scene& scene, auto_release_ptr<EnvironmentEDF>& env_edf)
    {
        env_edf->on_frame_begin(scene);

        TextureCache texture_cache(scene, 64 * 1024);
        InputEvaluator input_evaluator(texture_cache);

        Vector3d outgoing;
        Spectrum value1;
        double probability1;

        env_edf->sample(
            input_evaluator,
            Vector2d(0.3, 0.7),
            outgoing,
            value1,
            probability1);

        Spectrum value2;
        
        env_edf->evaluate(
            input_evaluator,
            outgoing,
            value2);

        const double probability2 =
            env_edf->evaluate_pdf(
                input_evaluator,
                outgoing);

        env_edf->on_frame_end(scene);

        const bool consistent =
            feq(probability1, probability2) &&
            feq(value1, value2);

        return consistent;
    }

    bool check_consistency(auto_release_ptr<EnvironmentEDF>& env_edf)
    {
        Scene scene;

        return check_consistency(scene, env_edf);
    }

    auto_release_ptr<ColorEntity> create_linear_rgb_color_entity(const Color3f& linear_rgb)
    {
        const ColorValueArray color_values(3, &linear_rgb[0]);

        ParamArray color_params;
        color_params.insert("color_space", "linear_rgb");

        return ColorEntityFactory::create("color", color_params, color_values);
    }

    void bind_color_to_input(
        auto_release_ptr<EnvironmentEDF>&   entity,
        const char*                         input_name,
        const Color3f&                      linear_rgb)
    {
        auto_release_ptr<ColorEntity> color =
            create_linear_rgb_color_entity(linear_rgb);

        ColorSource* source = new ColorSource(*color.get());

        entity->get_inputs().find(input_name).bind(source);
    }

    void bind_texture_to_input(
        Scene&                              scene,
        auto_release_ptr<EnvironmentEDF>&   entity,
        const char*                         input_name,
        Texture*                            texture)
    {
        const size_t texture_index =
            scene.textures().insert(auto_release_ptr<Texture>(texture));

        ParamArray texture_instance_params;
        texture_instance_params.insert("addressing_mode", "clamp");
        texture_instance_params.insert("filtering_mode", "bilinear");

        TextureInstance* texture_instance =
            TextureInstanceFactory::create(
                "texture_instance",
                texture_instance_params,
                texture_index).release();

        scene.texture_instances().insert(
            auto_release_ptr<TextureInstance>(texture_instance));

        TextureSource* source =
            new TextureSource(
                ~UniqueID(0),
                *texture_instance,
                texture->properties());

        entity->get_inputs().find(input_name).bind(source);
    }

    FOUNDATION_TEST_CASE(CheckConstantEnvironmentEDFConsistency)
    {
        auto_release_ptr<EnvironmentEDF> env_edf(
            ConstantEnvironmentEDFFactory().create("env_edf", ParamArray()));

        bind_color_to_input(env_edf, "exitance", Color3f(0.2f, 0.5f, 0.9f));

        const bool consistent = check_consistency(env_edf);

        FOUNDATION_EXPECT_TRUE(consistent);
    }

    FOUNDATION_TEST_CASE(CheckGradientEnvironmentEDFConsistency)
    {
        auto_release_ptr<EnvironmentEDF> env_edf(
            GradientEnvironmentEDFFactory().create("env_edf", ParamArray()));

        bind_color_to_input(env_edf, "horizon_exitance", Color3f(1.0f, 0.2f, 0.2f));
        bind_color_to_input(env_edf, "zenith_exitance", Color3f(0.2f, 1.0f, 0.2f));

        const bool consistent = check_consistency(env_edf);

        FOUNDATION_EXPECT_TRUE(consistent);
    }

    class HorizontalGradientTexture
      : public Texture
    {
      public:
        // Constructor.
        HorizontalGradientTexture()
          : Texture(ParamArray())
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

        // Delete this instance.
        virtual void release()
        {
            delete this;
        }

        // Return the name of this instance.
        virtual const char* get_name() const
        {
            return "horizontal_gradient_texture";
        }

        // Return a string identifying the model of this entity.
        virtual const char* get_model() const
        {
            return "horizontal_gradient_texture";
        }

        // Return the color space of the texture.
        virtual ColorSpace get_color_space() const
        {
            return ColorSpaceLinearRGB;
        }

        // Access canvas properties.
        virtual const CanvasProperties& properties()
        {
            return m_props;
        }

        // Load a given tile.
        virtual Tile* load_tile(
            const size_t    tile_x,
            const size_t    tile_y)
        {
            assert(tile_x == 0);
            assert(tile_y == 0);

            return m_tile.get();
        }

        // Unload a given tile.
        virtual void unload_tile(
            const size_t    tile_x,
            const size_t    tile_y,
            Tile*           tile)
        {
        }

      private:
        CanvasProperties    m_props;
        auto_ptr<Tile>      m_tile;

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

    FOUNDATION_TEST_CASE(CheckLatLongMapEnvironmentEDFConsistency)
    {
        Scene scene;

        Texture* texture = new HorizontalGradientTexture();

        ParamArray params;
        params.insert("exitance", "texture_instance");
        auto_release_ptr<EnvironmentEDF> env_edf(
            LatLongMapEnvironmentEDFFactory().create("env_edf", params));

        bind_texture_to_input(scene, env_edf, "exitance", texture);

        const bool consistent = check_consistency(scene, env_edf);

        FOUNDATION_EXPECT_TRUE(consistent);
    }

    FOUNDATION_TEST_CASE(CheckMirrorBallMapEnvironmentEDFConsistency)
    {
        Scene scene;

        Texture* texture = new HorizontalGradientTexture();

        ParamArray params;
        params.insert("exitance", "texture_instance");
        auto_release_ptr<EnvironmentEDF> env_edf(
            MirrorBallMapEnvironmentEDFFactory().create("env_edf", params));

        bind_texture_to_input(scene, env_edf, "exitance", texture);

        const bool consistent = check_consistency(scene, env_edf);

        FOUNDATION_EXPECT_TRUE(consistent);
    }
}
