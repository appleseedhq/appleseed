
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

// Interface header.
#include "pinholecamera.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/camera/perspectivecamera.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/dual.h"
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class OnRenderBeginRecorder; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Pinhole camera.
    //
    // Geometry of the camera:
    //
    //                                          Y+
    //
    //       Film Plane                         ^
    //   Z = m_focal_length                     |
    //                                          |
    //            +-----------------------------+
    //            |                             |
    //            |                             |
    //            |                             |
    //            |                             |
    //            |                             |
    //     Z+  <--+-----------------------------o
    //            |                             | O
    //            |                             |
    //            |                             |
    //            |                             |
    //            |                             |
    //            +-----------------------------+
    //                                          |
    //                                          |
    //                                          +
    //
    //                                      Lens Plane
    //                                        Z = 0
    //

    const char* Model = "pinhole_camera";

    class PinholeCamera
      : public PerspectiveCamera
    {
      public:
        PinholeCamera(
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
            RENDERER_LOG_INFO(
                "camera \"%s\" (#" FMT_UNIQUE_ID ") parameters:\n"
                "  model                         %s\n"
                "  film width                    %f\n"
                "  film height                   %f\n"
                "  focal length                  %f\n"
                "  near z                        %f\n"
                "  shift                         %f, %f\n"
                "  shutter open begin time       %f\n"
                "  shutter open end time         %f\n"
                "  shutter close begin time      %f\n"
                "  shutter close end time        %f",
                get_path().c_str(),
                get_uid(),
                Model,
                m_film_dimensions[0],
                m_film_dimensions[1],
                m_focal_length,
                m_near_z,
                m_shift.x,
                m_shift.y,
                m_shutter_open_begin_time,
                m_shutter_open_end_time,
                m_shutter_close_begin_time,
                m_shutter_close_end_time);
        }

        void spawn_ray(
            SamplingContext&        sampling_context,
            const Dual2d&           ndc,
            ShadingRay&             ray) const override
        {
            //
            // We do as if the ray originated on the film plane at Z = m_focal_length
            // and passed through the pin hole at Z = 0, except we make the ray start
            // at the pin hole (i.e. at the origin in camera space) since appleseed's
            // convention is that camera rays originate at the lens.
            //

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
                ray.m_rx_org = ray.m_org;
                ray.m_ry_org = ray.m_org;
                ray.m_rx_dir = normalize(transform.vector_to_parent(-ndc_to_camera(px)));
                ray.m_ry_dir = normalize(transform.vector_to_parent(-ndc_to_camera(py)));
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
            const double dist_film_lens = std::sqrt(square_dist_film_lens);
            const double cos_theta = m_focal_length / dist_film_lens;
            const double solid_angle = m_pixel_area * cos_theta / square_dist_film_lens;
            importance = 1.0f / static_cast<float>(square_norm(outgoing) * solid_angle);

            // The connection was possible.
            return true;
        }
    };
}


//
// PinholeCameraFactory class implementation.
//

void PinholeCameraFactory::release()
{
    delete this;
}

const char* PinholeCameraFactory::get_model() const
{
    return Model;
}

Dictionary PinholeCameraFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Pinhole Camera")
            .insert("default_model", "true");
}

DictionaryArray PinholeCameraFactory::get_input_metadata() const
{
    DictionaryArray metadata = CameraFactory::get_input_metadata();

    CameraFactory::add_film_metadata(metadata);
    CameraFactory::add_lens_metadata(metadata);
    CameraFactory::add_clipping_metadata(metadata);
    CameraFactory::add_shift_metadata(metadata);

    return metadata;
}

auto_release_ptr<Camera> PinholeCameraFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Camera>(new PinholeCamera(name, params));
}

}   // namespace renderer
