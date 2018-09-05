
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Jino Park, The appleseedhq Organization
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
#include "fisheyelenscamera.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/camera/perspectivecamera.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/dual.h"
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/autoreleaseptr.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class OnRenderBeginRecorder; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    const char* Model = "fisheyelens_camera";

    class FisheyeLensCamera
      : public PerspectiveCamera
    {
      public:
        FisheyeLensCamera(
            const char*             name,
            const ParamArray&       params)
          : PerspectiveCamera(name, params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        void print_settings() const override
        {
            const char* projection_type;
            switch (m_projection_type)
            {
              case Projection::EquisolidAngle:
                projection_type = "equisolid angle";
                break;
                
              case Projection::Equidistant:
                projection_type = "equidistant";
                break;

              case Projection::Stereographic:
                projection_type = "stereographic";
                break;

              case Projection::Thoby:
                projection_type = "thoby";
                break;

              default:
                projection_type = "unknown";
                break;
            }

            RENDERER_LOG_INFO(
                "camera \"%s\" settings:\n"
                "  model                         %s\n"
                "  film width                    %f\n"
                "  film height                   %f\n"
                "  focal length                  %f\n"
                "  near-z                        %f\n"
                "  shutter open begin time       %f\n"
                "  shutter open end time         %f\n"
                "  shutter close begin time      %f\n"
                "  shutter close end time        %f\n"
                "  projection type               %s",
                get_path().c_str(),
                Model,
                m_film_dimensions[0],
                m_film_dimensions[1],
                m_focal_length,
                m_near_z,
                m_shutter_open_begin_time,
                m_shutter_open_end_time,
                m_shutter_close_begin_time,
                m_shutter_close_end_time,
                projection_type);
        }

        bool on_render_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnRenderBeginRecorder&  recorder,
            IAbortSwitch*           abort_switch) override
        {
            if (!PerspectiveCamera::on_render_begin(project, parent, recorder, abort_switch))
                return false;

            const string projection_type = m_params.get_required<string>("projection_type", "equisolid_angle");

            if (projection_type == "equisolid_angle")
                 m_projection_type = Projection::EquisolidAngle;
             else if (projection_type == "equidistant")
                 m_projection_type = Projection::Equidistant;
             else if (projection_type == "stereographic")
                 m_projection_type = Projection::Stereographic;
             else if (projection_type == "thoby")
                 m_projection_type = Projection::Thoby;
             else
             {
                 RENDERER_LOG_ERROR(
                     "invalid value \"%s\" for parameter \"projection_type\", "
                     "using default value \"equisolid_angle\".",
                     projection_type.c_str());
                 m_projection_type = Projection::EquisolidAngle;
             }

            return true;
        }

        void spawn_ray(
            SamplingContext&        sampling_context,
            const Dual2d&           ndc,
            ShadingRay&             ray) const override
        {
            // Initialize the ray.
            initialize_ray(sampling_context, ray);

            // Retrieve the camera transform.
            Transformd scratch;
            const Transformd& transform =
                m_transform_sequence.evaluate(ray.m_time.m_absolute, scratch);

            // Compute ray origin and direction.
            ray.m_org = transform.get_local_to_parent().extract_translation();
            ray.m_dir = normalize(transform.vector_to_parent(-ndc_to_camera(ndc.get_value())));

            // Compute ray derivatives.
            if (ndc.has_derivatives())
            {
                const Vector2d px(ndc.get_value() + ndc.get_dx());
                const Vector2d py(ndc.get_value() + ndc.get_dy());
                ray.m_rx.m_org = ray.m_org;
                ray.m_ry.m_org = ray.m_org;
                ray.m_rx.m_dir = normalize(transform.vector_to_parent(-ndc_to_camera(px)));
                ray.m_ry.m_dir = normalize(transform.vector_to_parent(-ndc_to_camera(py)));
                ray.m_has_differentials = true;
            }
        }

        bool connect_vertex(
            SamplingContext&        sampling_context,
            const float             time,
            const Vector3d&         point,
            Vector2d&               ndc,
            Vector3d&               outgoing,
            float&                  importance) const override
        {
            // Project the point onto the film plane.
            if (!project_point(time, point, ndc))
                return false;

            // The connection is impossible if the projected point lies outside the film.
            if (ndc[0] < 0.0 || ndc[0] >= 1.0 ||
                ndc[1] < 0.0 || ndc[1] >= 1.0)
                return false;

            // Retrieve the camera transform.
            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(time, scratch);

            // Compute the outgoing direction vector in world space.
            outgoing = point - transform.get_local_to_parent().extract_translation();

            // Compute the emitted importance.
            const Vector3d film_point = ndc_to_camera(ndc);
            const double square_dist_film_lens = square_norm(film_point);
            const double dist_film_lens = sqrt(square_dist_film_lens);
            const double cos_theta = m_focal_length / dist_film_lens;
            const double solid_angle = m_pixel_area * cos_theta / square_dist_film_lens;
            importance = 1.0f / static_cast<float>(square_norm(outgoing) * solid_angle);

            // The connection was possible.
            return true;
        }

      private:
        enum class Projection
        {
            EquisolidAngle, 
            Equidistant, 
            Stereographic, 
            Thoby
        };

        Projection m_projection_type;


//
//                    |  axis 
//                    |
//        #-----------------------------------  _
//       # \                                ^ \  \ 
//      #   \                             ^    \  \  radius_2
//     #     \        |                 ^       \  \ 
//    #       \                       ^          \  \ 
//    #        \                    ^             \  \ 
//   #         *------------------^----------------\  \  _
//   #        *  \    |         ^   )           ""  \  \  \ 
//   #       *    \           ^       )    ""        \  \  \ radius_1
//   #      *      \        ^        "")              \  \  \ 
//   #      *       \     ^   ""       ) theta2        \  \  \ 
//   #      *        \| ^"  ) theta1  )                 \  -  -
//   #      *         o--------------------------------------------------> axis
//    #      *       /                                   |
//     #      *     /                               m_focal_length
//      #      *   / 
//       #      * / radius_1                                """""""" : original ray direction.
//        #      /                                          ^^^^^^^^ : transformed ray direction.
//          #   /   
//            #/ radius_2
//            /
//           /
//          /  axis
//

        Vector3d ndc_to_camera(const Vector2d& point) const
        {
            const double x = (0.5 - point.x) * m_film_dimensions[0];
            const double y = (point.y - 0.5) * m_film_dimensions[1];

            const double radius_1 = sqrt(x * x + y * y);
            const double rcp_radius_1 = 1.0 / radius_1;

            const double tan_theta1 = radius_1 / m_focal_length;
            double theta2 = 0.0;

            switch (m_projection_type) 
            {
              case Projection::EquisolidAngle:
                theta2 = 2.0 * asin(tan_theta1 * 0.5);
                break;
                
              case Projection::Equidistant:
                theta2 = tan_theta1;
                break;

              case Projection::Stereographic:
                theta2 = 2.0 * atan(tan_theta1 * 0.5);
                break;

              case Projection::Thoby:
                theta2 = asin(tan_theta1 * 0.68027) * 1.40252;
                break;

              default:
                assert(false);
            }

            const double radius_diff = tan(theta2) * m_focal_length - radius_1;

            return
                Vector3d(
                    x + radius_diff * x * rcp_radius_1,
                    y + radius_diff * y * rcp_radius_1,
                    m_focal_length);
        }

        Vector2d camera_to_ndc(const Vector3d& point) const
        {
            const double k = m_focal_length / point.z;
            
            const double x = 0.5 - (point.x * k * m_rcp_film_width);
            const double y = 0.5 + (point.y * k * m_rcp_film_height);
            
            const double radius_2 = sqrt(x * x + y * y);
            const double rcp_radius_2 = 1.0 / radius_2;

            const double cos_ = x * rcp_radius_2;
            const double sin_ = y * rcp_radius_2;
            
            const double theta2 = atan(radius_2 / m_focal_length);
            double tan_theta1 = 0.0;

            switch (m_projection_type) 
            {
              case Projection::EquisolidAngle:
                tan_theta1 = 2.0 * sin(theta2 * 0.5);
                break;
                
              case Projection::Equidistant:
                tan_theta1 = theta2;
                break;

              case Projection::Stereographic:
                tan_theta1 = 2.0 * tan(theta2 * 0.5);
                break;

              case Projection::Thoby:
                tan_theta1 = 1.47 * sin(0.713 * theta2);
                break;

              default:
                assert(false);
            }

            const double radius_1 = tan_theta1 * m_focal_length;

            return
                Vector2d(
                    radius_1 * cos_,
                    radius_1 * sin_);
        }
    };
}

//
// FisheyeLensCameraFactory class implementation.
//

void FisheyeLensCameraFactory::release()
{
    delete this;
}

const char* FisheyeLensCameraFactory::get_model() const
{
    return Model;
}

Dictionary FisheyeLensCameraFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Fisheye Lens Camera")
            .insert("default_model", "true");
}

DictionaryArray FisheyeLensCameraFactory::get_input_metadata() const
{
    DictionaryArray metadata = CameraFactory::get_input_metadata();

    CameraFactory::add_film_metadata(metadata);
    CameraFactory::add_lens_metadata(metadata);
    CameraFactory::add_clipping_metadata(metadata);
    CameraFactory::add_shift_metadata(metadata);

    metadata.push_back(
    Dictionary()
        .insert("name", "projection_type")
        .insert("label", "Projection Type")
        .insert("type", "enumeration")
        .insert("items",
            Dictionary()
                .insert("Equisolid angle", "equisolid_angle")
                .insert("Equidistant", "equidistant")
                .insert("Stereographic", "stereographic")
                .insert("Thoby", "thoby"))
        .insert("default", "equisolid_angle")
        .insert("use", "required"));

    return metadata;
}

auto_release_ptr<Camera> FisheyeLensCameraFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Camera>(new FisheyeLensCamera(name, params));
}

}   // namespace renderer
