
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 João Marcos Costa, The appleseedhq Organization
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
#include "HosekSun.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/color/wavelengths.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/light/lighttarget.h"
#include "renderer/modeling/light/sunlight.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/basis.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class ShadingContext; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Hosek & Wilkie model data.
    //
    
    #include "renderer/modeling/light/ArHosekSkyModelData_Spectral.h"

    //
    // Hosek Sun light.
    //
    // References:
    //
    //   http://cgg.mff.cuni.cz/projects/SkylightModelling/
    //
    
    const char* Model = "Hosek_sun_light";
    
    constexpr float SolidAngleSun = 6.807e-5f;
            
    class HosekSunLight
        :public SunLight
    {
      public:
        HosekSunLight(
            const char*                 name,
            const ParamArray&           params)
            : SunLight(name, params)
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
    
        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            if (!SunLight::on_frame_begin(project, parent, recorder, abort_switch))
                return false;
    
            return true;
        }
    
      private:
        void compute_sun_radiance(
            const Vector3d&         outgoing,
            const float             turbidity,
            const float             radiance_multiplier,
            RegularSpectrum31f&     radiance,
            const float             squared_distance_to_center = 0) const override
        {
            const float cos_theta = -static_cast<float>(outgoing.y);
            const float sun_theta = std::acos(cos_theta);
    
            compute_coefficients(
                radiance,
                turbidity,
                sun_theta);
    
            float limb_darkening = compute_limb_darkening(squared_distance_to_center);
    
            // Compute the attenuated radiance of the Sun.
            for (size_t i = 0; i < 31; ++i)
            {
                radiance[i] *=
                    limb_darkening *
                    radiance_multiplier *
                    SolidAngleSun;
            }
        }
    
        float compute_coefficients2(
            int                     turbidity,
            int                     wl,
            float                   elevation) const
        {
            constexpr int Pieces = 45;
            constexpr int Order = 4;
    
            int pos =
                (int)(pow(2.0f * elevation / Pi<float>(), 1.0f / 3.0f) * Pieces); // floor
    
            if (pos > 44) pos = 44;
    
            const float break_x =
                pow(((float)pos / (float)Pieces), 3.0f) * (HalfPi<float>());
    
            const double* coefs =
                solarDatasets[wl] + (Order * Pieces * turbidity + Order * (pos + 1) - 1);
    
            float res = 0.0f;
            const float x = elevation - break_x;
            float x_exp = 1.0f;
    
            for (int i = 0; i < Order; ++i)
            {
                res += x_exp * static_cast<float>(*coefs--);
                x_exp *= x;
            }
    
            return res;
        }
    
        void compute_coefficients(
            RegularSpectrum31f&     radiance,
            const float             turbidity,
            const float             sun_theta) const
        {
            const float clamped_turbidity = clamp(turbidity, 1.0f, 10.0f);
            int turbidity_low = truncate<int>(clamped_turbidity) - 1;
            float turbidity_frac = clamped_turbidity - static_cast<float>(turbidity_low + 1);
    
            // Compute solar elevation.
            const float eta = HalfPi<float>() - sun_theta;
    
            if (turbidity_low == 9)
            {
                turbidity_low = 8;
                turbidity_frac = 1.0f;
            }
    
            int i = 0;
            for (float wavelength = 400.0f; wavelength <= 700.0f; wavelength += 10.0f, i++)
            {
                int    wl_low = (int)((wavelength - 320.0f) / 40.0f);
                float wl_frac =  fmod(wavelength, 40.0f) / 40.0f;
    
                if (wl_low == 10)
                {
                    wl_low = 9;
                    wl_frac = 1.0f;
                }
    
                radiance[i] =
                    (1.0f - turbidity_frac) *
                    ((1.0f - wl_frac) * compute_coefficients2(turbidity_low, wl_low, eta)
                        + wl_frac * compute_coefficients2(turbidity_low, wl_low + 1, eta))
                    + turbidity_frac *
                    ((1.0f - wl_frac) * compute_coefficients2(turbidity_low + 1, wl_low, eta)
                        + wl_frac * compute_coefficients2(turbidity_low + 1 , wl_low + 1, eta));
            }
        }
    };
}
    
//
// HosekSunLightFactory class implementation.

void HosekSunLightFactory::release()
{
    delete this;
}

const char* HosekSunLightFactory::get_model() const
{
    return Model;
}

Dictionary HosekSunLightFactory::get_model_metadata() const
{
    return
        Dictionary()
        .insert("name", Model)
        .insert("label", "Hosek Sun Light")
        .insert("help", "Hosek's Physically-based sun light");
}

DictionaryArray HosekSunLightFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_sun_input_metadata(metadata);
    add_common_input_metadata(metadata);

    return metadata;
}

auto_release_ptr<Light> HosekSunLightFactory::create(
    const char* name,
    const ParamArray& params) const
{
    return auto_release_ptr<Light>(new HosekSunLight(name, params));
}

}   // namespace renderer
