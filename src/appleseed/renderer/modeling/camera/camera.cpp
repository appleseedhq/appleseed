
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

// Interface header.
#include "camera.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Camera class implementation.
//

// Utility function to retrieve the film dimensions from a parameter array.
Vector2d Camera::get_film_dimensions(
    const ParamArray&   params,
    const string&       camera_name)
{
    const Vector2d DefaultFilmDimensions(0.025, 0.025);     // in meters

    Vector2d film_dimensions =
        params.get_required<Vector2d>("film_dimensions", DefaultFilmDimensions);

    if (film_dimensions[0] <= 0.0 || film_dimensions[1] <= 0.0)
    {
        RENDERER_LOG_ERROR(
            "while defining camera \"%s\": invalid value \"%f %f\" for parameter \"%s\", "
            "using default value \"%f %f\"",
            camera_name.c_str(),
            film_dimensions[0],
            film_dimensions[1],
            "film_dimensions",
            DefaultFilmDimensions[0],
            DefaultFilmDimensions[1]);

        film_dimensions = DefaultFilmDimensions;
    }

    return film_dimensions;
}

namespace
{
    // Compute the focal length (in meters), given the film width (in meters)
    // and the horizontal field of view (in degrees).
    double hfov_to_focal_length(const double film_width, const double hfov)
    {
        return 0.5 * film_width / tan(0.5 * deg_to_rad(hfov));
    }
}

// Utility function to retrieve the focal length from a parameter array.
double Camera::get_focal_length(
    const ParamArray&   params,
    const double        film_width,
    const string&       camera_name)
{
    const double DefaultFocalLength = 0.035;    // in meters
    const double DefaultHFov = 54.0;            // in degrees

    if (params.strings().exist("focal_length"))
    {
        if (params.strings().exist("horizontal_fov"))
        {
            RENDERER_LOG_WARNING(
                "while defining camera \"%s\": the parameter \"horizontal_fov\" "
                "has precedence over \"focal_length\"",
                camera_name.c_str());

            const double hfov = params.get_required<double>("horizontal_fov", DefaultHFov);
            return hfov_to_focal_length(film_width, hfov);
        }
        else
        {
            double focal_length = params.get_required<double>("focal_length", DefaultFocalLength);

            if (focal_length <= 0.0)
            {
                RENDERER_LOG_ERROR(
                    "while defining camera \"%s\": invalid value \"%f\" for parameter \"%s\", "
                    "using default value \"%f\"",
                    camera_name.c_str(),
                    focal_length,
                    "focal_length",
                    DefaultFocalLength);

                focal_length = DefaultFocalLength;
            }

            return focal_length;
        }
    }
    else if (params.strings().exist("horizontal_fov"))
    {
        const double hfov = params.get_required<double>("horizontal_fov", DefaultHFov);
        return hfov_to_focal_length(film_width, hfov);
    }
    else
    {
        RENDERER_LOG_ERROR(
            "while defining camera \"%s\": no \"horizontal_fov\" or \"focal_length\" parameter found, "
            "using default focal length value \"%f\"",
            camera_name.c_str(),
            DefaultFocalLength);

        return DefaultFocalLength;
    }
}

// Utility function to retrieve the f-stop value from a parameter array.
double Camera::get_f_stop(
    const ParamArray&   params,
    const string&       camera_name)
{
    const double DefaultFStop = 8.0;

    double f_stop = params.get_required<double>("f_stop", DefaultFStop);

    if (f_stop <= 0.0)
    {
        RENDERER_LOG_ERROR(
            "while defining camera \"%s\": invalid value \"%f\" for parameter \"%s\", "
            "using default value \"%f\"",
            camera_name.c_str(),
            f_stop,
            "f_stop",
            DefaultFStop);

        f_stop = DefaultFStop;
    }

    return f_stop;
}

// Utility function to retrieve the focal distance from a parameter array.
void Camera::get_focal_distance(
    const ParamArray&   params,
    const string&       camera_name,
    bool&               autofocus_enabled,
    Vector2d&           autofocus_target,
    double&             focal_distance)
{
    const Vector2d DefaultAFTarget(0.0);        // in NDC
    const double DefaultFocalDistance = 1.0;    // in meters

    if (params.strings().exist("focal_distance"))
    {
        if (params.strings().exist("autofocus_target"))
        {
            RENDERER_LOG_WARNING(
                "while defining camera \"%s\": autofocus is enabled; \"focal_distance\" parameter "
                "will be ignored",
                camera_name.c_str());

            autofocus_enabled = true;
            autofocus_target = params.get_required<Vector2d>("autofocus_target", DefaultAFTarget);
            focal_distance = 0.0;
        }
        else
        {
            autofocus_enabled = false;
            autofocus_target = Vector2d(0.0);
            focal_distance = params.get_required<double>("focal_distance", DefaultFocalDistance);
        }
    }
    else if (params.strings().exist("autofocus_target"))
    {
        autofocus_enabled = true;
        autofocus_target = params.get_required<Vector2d>("autofocus_target", DefaultAFTarget);
        focal_distance = 0.0;
    }
    else
    {
        RENDERER_LOG_ERROR(
            "while defining camera \"%s\": no \"focal_distance\" or \"autofocus_target\" parameter found, "
            "using default focal distance value \"%f\"",
            camera_name.c_str(),
            DefaultFocalDistance);

        autofocus_enabled = false;
        autofocus_target = Vector2d(0.0);
        focal_distance = DefaultFocalDistance;
    }
}

}   // namespace renderer
