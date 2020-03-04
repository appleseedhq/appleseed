
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Francois Beaune, The appleseedhq Organization
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
#include "screenspacevelocityaov.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

namespace renderer
{

Color3f compute_screen_space_velocity_color(
    const ShadingPoint& shading_point,
    const double        max_displace)
{
    Color3f result;

    const Camera* camera = shading_point.get_scene().get_render_data().m_active_camera;
    assert(camera);

    const Vector3d& p = shading_point.get_point();
    const Vector3d& v = shading_point.get_world_space_point_velocity();

    Vector2d a_ndc, b_ndc;
    if (camera->project_segment(shading_point.get_time().m_absolute, p, p + v, a_ndc, b_ndc))
    {
        Vector2d v_ndc = b_ndc - a_ndc;

        if (max_displace <= 0.0)
        {
            // R and G encode the unit direction vector; B encodes the magnitude of the vector.
            const double vn_ndc = norm(v_ndc);
            if (vn_ndc > 0.0)
            {
                v_ndc /= vn_ndc;
                result.r = static_cast<float>((v_ndc[0] + 1.0) * 0.5);
                result.g = static_cast<float>((v_ndc[1] + 1.0) * 0.5);
                result.b = static_cast<float>(vn_ndc);
            }
            else result.set(0.0f);
        }
        else
        {
            // R and G encode the direction vector normalized to MaxDisplace.
            v_ndc = saturate(v_ndc / max_displace);
            result.r = static_cast<float>((v_ndc[0] + 1.0) * 0.5);
            result.g = static_cast<float>((v_ndc[1] + 1.0) * 0.5);
            result.b = 0.0f;
        }
    }
    else result.set(0.0f);

    return result;
}

namespace
{
    //
    // Screen Space Velocity AOV accumulator.
    //

    class ScreenSpaceVelocityAOVAccumulator
      : public UnfilteredAOVAccumulator
    {
      public:
        ScreenSpaceVelocityAOVAccumulator(
            Image&                      image,
            const float                 max_displace)
          : UnfilteredAOVAccumulator(image)
          , m_max_displace(max_displace)
        {
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            const AOVComponents&        aov_components,
            ShadingResult&              shading_result) override
        {
            const Vector2i& pi = pixel_context.get_pixel_coords();

            // Ignore samples outside the tile.
            if (!m_cropped_tile_bbox.contains(pi))
                return;

            float* out =
                reinterpret_cast<float*>(
                    m_tile->pixel(
                        pi.x - m_tile_origin_x,
                        pi.y - m_tile_origin_y));

            if (shading_point.hit_surface())
            {
                const Color3f c = compute_screen_space_velocity_color(shading_point, m_max_displace);
                out[0] = c[0];
                out[1] = c[1];
                out[2] = c[2];
            }
            else
            {
                out[0] = 0.0f;
                out[1] = 0.0f;
                out[2] = 0.0f;
            }
        }

      private:
        const float m_max_displace;
    };


    //
    // Screen Space Velocity AOV.
    //

    const char* ScreenSpaceVelocityAOVModel = "screen_space_velocity_aov";

    class ScreenSpaceVelocityAOV
      : public UnfilteredAOV
    {
      public:
        explicit ScreenSpaceVelocityAOV(const ParamArray& params)
          : UnfilteredAOV("screen_space_velocity", params)
          , m_max_displace(params.get_optional<float>("max_displace", 0.0f))
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return ScreenSpaceVelocityAOVModel;
        }

        void clear_image() override
        {
            m_image->clear(Color3f(0.0f));
        }

      private:
        const float m_max_displace;

        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(
                new ScreenSpaceVelocityAOVAccumulator(get_image(), m_max_displace));
        }
    };
}


//
// ScreenSpaceVelocityAOVFactory class implementation.
//

void ScreenSpaceVelocityAOVFactory::release()
{
    delete this;
}

const char* ScreenSpaceVelocityAOVFactory::get_model() const
{
    return ScreenSpaceVelocityAOVModel;
}

Dictionary ScreenSpaceVelocityAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Screen-Space Velocity");
}

DictionaryArray ScreenSpaceVelocityAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> ScreenSpaceVelocityAOVFactory::create(const ParamArray& params) const
{
    return auto_release_ptr<AOV>(new ScreenSpaceVelocityAOV(params));
}

}   // namespace renderer
