
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2022 Lars Zawallich, The appleseedhq Organization
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
#include "lenscamera.h"

// appleseed.renderer headers.

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/iostreamop.h"

// Standard headers.

// Forward declarations.

using namespace foundation;

namespace renderer
{
LensCamera::LensCamera(const char* name, const ParamArray& params)
  : PerspectiveCamera(name, params)
{
}

void LensCamera::extract_diaphragm_blade_count()
{
    const int blade_count = m_params.get_optional<int>("diaphragm_blades", 0);

    if (blade_count == 0 || blade_count >= 3)
        m_diaphragm_blade_count = static_cast<size_t>(blade_count);
    else
    {
        m_diaphragm_blade_count = 0;
        RENDERER_LOG_ERROR(
            "while defining camera \"%s\": invalid value \"%d\" for parameter \"%s\", "
            "using default value \"" FMT_SIZE_T "\".",
            get_path().c_str(),
            blade_count,
            "diaphragm_blades",
            m_diaphragm_blade_count);
    }
}

void LensCamera::extract_diaphragm_tilt_angle()
{
    m_diaphragm_tilt_angle =
        deg_to_rad(m_params.get_optional<double>("diaphragm_tilt_angle", 0.0));
}

void LensCamera::extract_focal_distance(
    const bool  autofocus_enabled,
    Vector2d&   autofocus_target,
    double&     focal_distance) const
{
    const Vector2d DefaultAFTarget(0.5);        // in NDC
    const double DefaultFocalDistance = 1.0;    // in meters

    if (autofocus_enabled)
    {
        if (has_param("autofocus_target"))
            autofocus_target = m_params.get_required<Vector2d>("autofocus_target", DefaultAFTarget);
        else
        {
            RENDERER_LOG_ERROR(
                "while defining camera \"%s\": no \"autofocus_target\" parameter found; "
                "using default value \"%f, %f\".",
                get_path().c_str(),
                DefaultAFTarget[0],
                DefaultAFTarget[1]);
            autofocus_target = DefaultAFTarget;
        }

        focal_distance = DefaultFocalDistance;
    }
    else
    {
        if (has_param("focal_distance"))
            focal_distance = m_params.get_required<double>("focal_distance", DefaultFocalDistance);
        else
        {
            RENDERER_LOG_ERROR(
                "while defining camera \"%s\": no \"focal_distance\" parameter found; "
                "using default value \"%f\".",
                get_path().c_str(),
                DefaultFocalDistance);
            focal_distance = DefaultFocalDistance;
        }

        autofocus_target = DefaultAFTarget;
    }
}

void LensCameraFactory::add_lens_metadata(DictionaryArray& metadata)
{
    CameraFactory::add_lens_metadata(metadata);

    metadata.push_back(
        Dictionary()
        .insert("name", "f_stop")
        .insert("label", "F-number")
        .insert("type", "numeric")
        .insert("min",
            Dictionary()
            .insert("value", "0.5")
            .insert("type", "soft"))
        .insert("max",
            Dictionary()
            .insert("value", "256.0")
            .insert("type", "soft"))
        .insert("use", "required"));

    metadata.push_back(
        Dictionary()
        .insert("name", "autofocus_enabled")
        .insert("label", "Enable autofocus")
        .insert("type", "boolean")
        .insert("use", "optional")
        .insert("default", "true")
        .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "focal_distance")
            .insert("label", "Focal Distance")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("visible_if",
                Dictionary()
                    .insert("autofocus_enabled", "false")));

    metadata.push_back(
        Dictionary()
            .insert("name", "autofocus_target")
            .insert("label", "Autofocus Target")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "0.5 0.5")
            .insert("visible_if",
                Dictionary()
                    .insert("autofocus_enabled", "true")));

    metadata.push_back(
        Dictionary()
            .insert("name", "diaphragm_blades")
            .insert("label", "Diaphragm Blades")
            .insert("type", "integer")
            .insert("min",
                Dictionary()
                    .insert("value", "3")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "256")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "diaphragm_tilt_angle")
            .insert("label", "Diaphragm Tilt Angle")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-360.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "360.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "diaphragm_map")
            .insert("label", "Diaphragm Map")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional"));
}
}   // namespace renderer
