
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
#include "PreethamSun.h"

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
namespace foundation { class IAbortSwitch; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Physically-based Sun light.
    //
    // References:
    //
    //   http://www.cs.utah.edu/~shirley/papers/sunsky/sunsky.pdf
    //   http://ompf2.com/viewtopic.php?f=3&t=33
    //

    const char* Model = "Preetham_sun_light";

    constexpr float SolidAngleSun = 6.807e-5f;


    class PreethamSunLight
        :public SunLight
    {
      public:
        PreethamSunLight(
            const char* name,
            const ParamArray& params)
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
            const Project& project,
            const BaseGroup* parent,
            OnFrameBeginRecorder& recorder,
            IAbortSwitch* abort_switch) override
        {
            if (!SunLight::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            precompute_constants();

            return true;
        }

      private:

        RegularSpectrum31f  m_k1;
        RegularSpectrum31f  m_k2;

        void compute_sun_radiance(
            const Vector3d& outgoing,
            const float                 turbidity,
            const float                 radiance_multiplier,
            RegularSpectrum31f& radiance,
            const float                 squared_distance_to_center = 0.0f) const override
        {
            // Compute the relative optical mass.
            const float cos_theta = -static_cast<float>(outgoing.y);
            const float theta = std::acos(cos_theta);
            const float theta_delta = 93.885f - rad_to_deg(theta);
            if (theta_delta < 0.0f)
            {
                radiance.set(0.0f);
                return;
            }
            const float m = 1.0f / (cos_theta + 0.15f * std::pow(theta_delta, -1.253f));

            // Compute transmittance due to Rayleigh scattering.
            RegularSpectrum31f tau_r;
            for (size_t i = 0; i < 31; ++i)
                tau_r[i] = std::exp(m * m_k1[i]);

            // Compute transmittance due to aerosols.
            const float beta = 0.04608f * turbidity - 0.04586f;
            RegularSpectrum31f tau_a;
            for (size_t i = 0; i < 31; ++i)
                tau_a[i] = std::exp(-beta * m * m_k2[i]);

            // Compute transmittance due to ozone absorption.
            const float L = 0.0035f;                  // amount of ozone in m
            static const float Ko[31] =
            {
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.300f, 0.600f, 0.900f,
                1.400f, 2.100f, 3.000f, 4.000f,
                4.800f, 6.300f, 7.500f, 8.500f,
                10.30f, 12.00f, 12.00f, 11.50f,
                12.50f, 12.00f, 10.50f, 9.000f,
                7.900f, 6.700f, 5.700f, 4.800f,
                3.600f, 2.800f, 2.300f
            };
            RegularSpectrum31f tau_o;
            for (size_t i = 0; i < 31; ++i)
                tau_o[i] = std::exp(-Ko[i] * L * m);

#ifdef COMPUTE_REDUNDANT
            // Compute transmittance due to mixed gases absorption.
            // Disabled since all coefficients are zero in the wavelength range of the simulation.
            static const float Kg[31] =
            {
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f
            };
            RegularSpectrum31f tau_g;
            for (size_t i = 0; i < 31; ++i)
                tau_g[i] = std::exp(-1.41f * Kg[i] * m / std::pow(1.0f + 118.93f * Kg[i] * m, 0.45f));
#endif

            // Compute transmittance due to water vapor absorption.
            const float W = 0.02f;                   // precipitable water vapor in m
            static const float Kwa[31] =
            {
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 1.600f, 2.400f
            };
            RegularSpectrum31f tau_wa;
            for (size_t i = 0; i < 31; ++i)
                tau_wa[i] = std::exp(-0.2385f * Kwa[i] * W * m / std::pow(1.0f + 20.07f * Kwa[i] * W * m, 0.45f));

            // Sun radiance in W.m^-2.sr^-1.um^-1.
            // The units in the paper are W.cm^-2.sr^-1.um^-1. We must multiply the values
            // by 10000 to obtain W.m^-2.sr^-1.um^-1. We must then divide them by 1000 to
            // obtain W.m^-2.sr^-1.nm^-1.
            float SunRadianceValues[31] =
            {
                21127.5f, 25888.2f, 25829.1f, 24232.3f,
                26760.5f, 29658.3f, 30545.4f, 30057.5f,
                30663.7f, 28830.4f, 28712.1f, 27825.0f,
                27100.6f, 27233.6f, 26361.3f, 25503.8f,
                25060.2f, 25311.6f, 25355.9f, 25134.2f,
                24631.5f, 24173.2f, 23685.3f, 23212.1f,
                22827.7f, 22339.8f, 21970.2f, 21526.7f,
                21097.9f, 20728.3f, 20240.4f
            };

            for (size_t i = 0; i < 31; ++i)
            {
                SunRadianceValues[i] *= SolidAngleSun;
            }

            float limb_darkening = compute_limb_darkening(squared_distance_to_center);

            // Compute the attenuated radiance of the Sun.
            for (size_t i = 0; i < 31; ++i)
            {
                radiance[i] =
                    SunRadianceValues[i] *
                    tau_r[i] *
                    tau_a[i] *
                    tau_o[i] *
#ifdef COMPUTE_REDUNDANT
                        tau_g[i] *      // always 1.0
#endif
                    tau_wa[i] *
                    limb_darkening *
                    radiance_multiplier;
            }
        }

        void precompute_constants()
        {
            for (size_t i = 0; i < 31; ++i)
                m_k1[i] = -0.008735f * std::pow(g_light_wavelengths_um[i], -4.08f);

            const float Alpha = 1.3f;               // ratio of small to large particle sizes (0 to 4, typically 1.3)

            for (size_t i = 0; i < 31; ++i)
                m_k2[i] = std::pow(g_light_wavelengths_um[i], -Alpha);
        }
    };
}
//
// PreethamSunLightFactory class implementation.
//

void PreethamSunLightFactory::release()
{
    delete this;
}

const char* PreethamSunLightFactory::get_model() const
{
    return Model;
}

Dictionary PreethamSunLightFactory::get_model_metadata() const
{
    return
        Dictionary()
        .insert("name", Model)
        .insert("label", "Preetham sun Light")
        .insert("help", "Preetham's Physically-based sun light");
}

DictionaryArray PreethamSunLightFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_sun_input_metadata(metadata);
    add_common_input_metadata(metadata);

    return metadata;
}

auto_release_ptr<Light> PreethamSunLightFactory::create(
    const char* name,
    const ParamArray& params) const
{
    return auto_release_ptr<Light>(new PreethamSunLight(name, params));
}

}   // namespace renderer
