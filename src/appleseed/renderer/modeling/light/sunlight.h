
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

#pragma once

// appleseed.renderer headers.
#include "renderer/modeling/light/ilightfactory.h"
#include "renderer/modeling/light/light.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace renderer      { class ParamArray; }
namespace renderer      { class EnvironmentEDF; }

namespace renderer
{

//
// Physically-based Sun light.
//


class SunLight
  :public Light
{
  public:
    SunLight(
        const char*                         name,
        const ParamArray&                   params);

    bool on_frame_begin(
        const Project&                      project,
        const BaseGroup*                    parent,
        OnFrameBeginRecorder&               recorder,
        foundation::IAbortSwitch*           abort_switch) override;

    void sample(
        const ShadingContext&               shading_context,
        const foundation::Transformd&       light_transform,
        const foundation::Vector2d&         s,
        foundation::Vector3d&               position,
        foundation::Vector3d&               outgoing,
        Spectrum&                           value,
        float&                              probability) const override;

    void sample(
        const ShadingContext&               shading_context,
        const foundation::Transformd&       light_transform,
        const foundation::Vector3d&         target_point,
        const foundation::Vector2d&         s,
        foundation::Vector3d&               position,
        foundation::Vector3d&               outgoing,
        Spectrum&                           value,
        float&                              probability) const override;

    void sample(
        const ShadingContext&               shading_context,
        const foundation::Transformd&       light_transform,
        const foundation::Vector2d&         s,
        const LightTargetArray&             targets,
        foundation::Vector3d&               position,
        foundation::Vector3d&               outgoing,
        Spectrum&                           value,
        float&                              probability) const override;

    float compute_distance_attenuation(
        const foundation::Vector3d&         target,
        const foundation::Vector3d&         position) const override;

    virtual void evaluate(
        const foundation::Vector3d&         outgoing,
        Spectrum&                           value) const;

  protected:
    float compute_limb_darkening(const float squared_distance_to_center) const;

    virtual void compute_sun_radiance(
        const foundation::Vector3d&         outgoing,
        const float                         turbidity,
        const float                         radiance_multiplier,
        foundation::RegularSpectrum31f&     radiance,
        const float                         squared_distance_to_center = 0) const = 0;

    APPLESEED_DECLARE_INPUT_VALUES(InputValues)
    {
        float           m_turbidity;                // atmosphere turbidity
        float           m_radiance_multiplier;      // emitted radiance multiplier
        float           m_size_multiplier;          // Sun size multiplier
        float           m_distance;                 // distance between Sun and scene, in millions of km
    };

    bool                    m_visible;               // visible of the sun
    foundation::Vector3d    m_scene_center;             // world space
    double                  m_scene_radius;             // world space
    double                  m_safe_scene_diameter;      // world space
    float                   m_sun_solid_angle;          // Sun's solid angle, in steradians
    float                   m_sun_size;

    InputValues             m_values;

  private:
    void apply_env_edf_overrides(EnvironmentEDF* env_edf);

    void sample_disk(
        const foundation::Transformd&   light_transform,
        const foundation::Vector2d&     s,
        const foundation::Vector3d&     disk_center,
        const double                    disk_radius,
        foundation::Vector3d&           position,
        foundation::Vector3d&           outgoing,
        Spectrum&                       value,
        float&                          probability) const;

    void sample_sun_surface(
        const foundation::Transformd&   light_transform,
        const foundation::Vector3d&     target_point,
        const foundation::Vector2d&     s,
        foundation::Vector3d&           position,
        foundation::Vector3d&           outgoing,
        Spectrum&                       value,
        float&                          probability) const;
};

}   // namespace renderer
