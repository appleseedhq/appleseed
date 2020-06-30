
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
#include "nishita93environmentedf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/fastmath.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cassert>

// Forward declarations.
namespace foundation { class IAbortSwitch; }
namespace renderer { class Project; }

using namespace foundation;

constexpr float PI = 3.14159265358979323846;

namespace {

    IrradianceSpectrum NewSolarSpectrum() {
        // Values from "Reference Solar Spectral Irradiance: ASTM G-173", ETR column
        // (see http://rredc.nrel.gov/solar/spectra/am1.5/ASTMG173/ASTMG173.html),
        // summed and averaged in each bin (e.g. the value for 360nm is the average of
        // the ASTM G-173 values for all wavelengths between 360 and 371.75nm).
        static const float kSpectralIrradiance[40] = {
          1.13419, 1.09801, 1.03541, 1.45086, 1.72453, 1.654, 1.70536, 1.97393,
          2.03543, 2.00643, 1.95531, 1.95426, 1.92438, 1.82092, 1.88517, 1.85545,
          1.85083, 1.82758, 1.84475, 1.78771, 1.76683, 1.70858, 1.68278, 1.63849,
          1.59608, 1.52211, 1.52468, 1.47836, 1.4485, 1.40522, 1.35526, 1.32788,
          1.28834, 1.26938, 1.23241, 1.20345, 1.17087, 1.1344, 1.11012, 1.07147
        };
        IrradianceSpectrum result;
        for (unsigned int i = 0; i < result.size(); ++i) {
            result[i] = kSpectralIrradiance[i] * watt_per_square_meter_per_nm;
        }
        return result;
    }

    ScatteringSpectrum NewRayleighScattering() {
        // Values from Table III in Penndorf 1957 "Tables of the Refractive Index for
        // Standard Air and the Rayleigh Scattering Coefficient for the Spectral
        // Region between 0.2 and 20.0 μ and Their Application to Atmospheric Optics".
        static const float kPenndorf[48] = {
          70.45E-6, 62.82E-6, 56.20E-6, 50.43E-6, 45.40E-6, 40.98E-6, 37.08E-6,
          33.65E-6, 30.60E-6, 27.89E-6, 25.48E-6, 23.33E-6, 21.40E-6, 19.66E-6,
          18.10E-6, 16.69E-6, 15.42E-6, 14.26E-6, 13.21E-6, 12.26E-6, 11.39E-6,
          10.60E-6, 9.876E-6, 9.212E-6, 8.604E-6, 8.045E-6, 7.531E-6, 7.057E-6,
          6.620E-6, 6.217E-6, 5.844E-6, 5.498E-6, 5.178E-6, 4.881E-6, 4.605E-6,
          4.348E-6, 4.109E-6, 3.886E-6, 3.678E-6, 3.484E-6, 3.302E-6, 3.132E-6,
          2.973E-6, 2.824E-6, 2.684E-6, 2.583E-6, 2.481E-6, 2.380E-6
        };
        std::vector<ScatteringCoefficient> penndorf_samples;
        for (int i = 0; i < 48; ++i) {
            // The above values are for T_0=0°C. For T=15°C, a correction factor
            // T_0 / T must be applied (Eq. (12) in Penndorf paper).
            constexpr double T_0 = 273.16;
            constexpr double T = T_0 + 15.0;
            penndorf_samples.push_back(kPenndorf[i] * (T_0 / T) / m);
        }
        return ScatteringSpectrum(360.0 * nm, 830.0 * nm, penndorf_samples);
    }

    ScatteringSpectrum NewMieExtinction(double angstrom_alpha,
        double angstrom_beta) {
        ScatteringSpectrum mie;
        for (unsigned int i = 0; i < mie.size(); ++i) {
            double lambda = mie.GetSample(i).to(1000.0 * nm);
            mie[i] = angstrom_beta * pow(lambda, -angstrom_alpha) / MieScaleHeight;
        }
        return mie;
    }

    ScatteringSpectrum NewMieScattering(double angstrom_alpha,
        double angstrom_beta) {
        const double kSingleScatteringAlbedo = 0.8;
        return
            NewMieExtinction(angstrom_alpha, angstrom_beta) * kSingleScatteringAlbedo;
    }

    DimensionlessSpectrum NewGroundAlbedo() {
        // Grass spectral albedo from Uwe Feister and Rolf Grewe, "Spectral albedo
        // measurements in the UV and visible region over different types of
        // surfaces", Photochemistry and Photobiology, 62, 736-744, 1995.
        static const float kGrassAlbedo[45] = {
          0.018, 0.019, 0.019, 0.020, 0.022, 0.024, 0.027, 0.029, 0.030, 0.031, 0.032,
          0.032, 0.032, 0.033, 0.035, 0.040, 0.055, 0.073, 0.084, 0.089, 0.089, 0.079,
          0.069, 0.063, 0.061, 0.057, 0.052, 0.051, 0.048, 0.042, 0.039, 0.035, 0.035,
          0.043, 0.087, 0.156, 0.234, 0.334, 0.437, 0.513, 0.553, 0.571, 0.579, 0.581,
          0.587
        };
        std::vector<Number> grass_albedo_samples;
        for (int i = 0; i < 45; ++i) {
            grass_albedo_samples.push_back(kGrassAlbedo[i]);
        }
        return DimensionlessSpectrum(360.0 * nm, 800.0 * nm, grass_albedo_samples);
    }

    const IrradianceSpectrum solar_spectrum = NewSolarSpectrum();
    const ScatteringSpectrum rayleigh_scattering = NewRayleighScattering();
    const ScatteringSpectrum mie_extinction =
        NewMieExtinction(MieAngstromAlpha, MieAngstromBeta);
    const ScatteringSpectrum mie_scattering =
        NewMieScattering(MieAngstromAlpha, MieAngstromBeta);
    const DimensionlessSpectrum ground_albedo = NewGroundAlbedo();

}  // anonymous namespace

const IrradianceSpectrum& SolarSpectrum() { return solar_spectrum; }
const ScatteringSpectrum& RayleighScattering() { return rayleigh_scattering; }
const ScatteringSpectrum& MieExtinction() { return mie_extinction; }
const ScatteringSpectrum& MieScattering() { return mie_scattering; }

ScatteringSpectrum MieExtinction(double angstrom_alpha, double angstrom_beta) {
    return NewMieExtinction(angstrom_alpha, angstrom_beta);
}

ScatteringSpectrum MieScattering(double angstrom_alpha, double angstrom_beta) {
    return NewMieScattering(angstrom_alpha, angstrom_beta);
}

InverseSolidAngle RayleighPhaseFunction(double scattering_angle) {
    return RayleighPhaseFunction(cos(scattering_angle));
}

InverseSolidAngle RayleighPhaseFunction(Number scattering_angle_cosine) {
    constexpr InverseSolidAngle kRayleigh =
        3.0 / (16.0 * PI) * InverseSolidAngle::Unit();
    return kRayleigh * (1.0 + scattering_angle_cosine * scattering_angle_cosine);
}

InverseSolidAngle MiePhaseFunction(double scattering_angle) {
    return MiePhaseFunction(cos(scattering_angle));
}

InverseSolidAngle MiePhaseFunction(Number scattering_angle_cosine) {
    constexpr double g = MiePhaseFunctionG;
    constexpr InverseSolidAngle kMie = 3.0 / (8.0 * PI) *
        (1.0 - g * g) / (2.0 + g * g) * InverseSolidAngle::Unit();
    return kMie * (1.0 + scattering_angle_cosine * scattering_angle_cosine) /
        pow(1.0 + g * g - 2.0 * g * scattering_angle_cosine, 1.5);
}

InverseSolidAngle MiePhaseFunction(double g, Number scattering_angle_cosine) {
    const InverseSolidAngle kMie = 3.0 / (8.0 * PI) *
        (1.0 - g * g) / (2.0 + g * g) * InverseSolidAngle::Unit();
    return kMie * (1.0 + scattering_angle_cosine * scattering_angle_cosine) /
        pow(1.0 + g * g - 2.0 * g * scattering_angle_cosine, 1.5);
}

const DimensionlessSpectrum& GroundAlbedo() { return ground_albedo; }

namespace renderer
{

    namespace
    {
        //
        // Nishita93 environment EDF.
        //
        // Conventions:
        //
        //   * All direction vectors are expressed in world space.
        //
        //   * All direction vectors are unit-length and pointing toward the environment.
        //
        //   * All probability densities are measured with respect to solid angle.
        //

        const char* Model = "nishita93_environment_edf";

        class Nishita93EnvironmentEDF
            : public EnvironmentEDF
        {
        public:

            // Constructor.
            Nishita93EnvironmentEDF(
                const char*             name,
                const ParamArray&       params)
                : EnvironmentEDF(name, params)
            {
                m_inputs.declare("sun_theta", InputFormatFloat);
                m_inputs.declare("sun_phi", InputFormatFloat);
                m_inputs.declare("turbidity", InputFormatFloat);
                m_inputs.declare("turbidity_multiplier", InputFormatFloat, "2.0");
                m_inputs.declare("luminance_multiplier", InputFormatFloat, "1.0");
                m_inputs.declare("luminance_gamma", InputFormatFloat, "1.0");
                m_inputs.declare("saturation_multiplier", InputFormatFloat, "1.0");
                m_inputs.declare("horizon_shift", InputFormatFloat, "0.0");
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

                // Check validity of user input parameters, return false if not correct
                if (!EnvironmentEDF::on_frame_begin(project, parent, recorder, abort_switch))
                    return false;

                // Evaluate uniform values.
                m_inputs.evaluate_uniforms(&m_uniform_values);

                // Compute the sun direction.
                m_sun_theta = deg_to_rad(m_uniform_values.m_sun_theta);
                m_sun_phi = deg_to_rad(m_uniform_values.m_sun_phi);
                m_sun_dir = Vector3f::make_unit_vector(m_sun_theta, m_sun_phi);
                m_cos_sun_theta = std::cos(m_sun_theta);

                // Do any precomputations that are needed for all evaluations

                // COPY_PASTA_START -->
                // Precomputes the optical length lookup tables.
                
                precompute_optical_lengths();
                // <-- COPY_PASTA_END

                return true;
            }

            // Sample the EDF and compute the emission direction, its probability
            // density and the value of the EDF for this direction.
            void sample(
                const ShadingContext&   shading_context,
                const Vector2f&         s,                                  // sample in [0,1)^2
                Vector3f&               outgoing,                           // world space emission direction, unit-length
                Spectrum&               value,                              // EDF value for this direction
                float&                  probability) const override         // PDF value
            {
                const Vector3f local_outgoing = sample_hemisphere_cosine(s);

                Transformd scratch;
                const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
                outgoing = transform.vector_to_parent(local_outgoing);
                const Vector3f shifted_outgoing = shift(local_outgoing);

                RegularSpectrum31f radiance;
                if (shifted_outgoing.y > 0.0f)
                    compute_sky_radiance(shading_context, shifted_outgoing, radiance);
                else radiance.set(0.0f);

                value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
                probability = shifted_outgoing.y > 0.0f ? shifted_outgoing.y * RcpPi<float>() : 0.0f;
                assert(probability >= 0.0f);
            }

            // Evaluate the EDF for a given emission direction.
            void evaluate(
                const ShadingContext&   shading_context,
                const Vector3f&         outgoing,                           // world space emission direction, unit-length
                Spectrum&               value) const override               // EDF value for this direction
            {
                assert(is_normalized(outgoing));

                Transformd scratch;
                const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
                const Vector3f local_outgoing = transform.vector_to_local(outgoing);
                const Vector3f shifted_outgoing = shift(local_outgoing);

                RegularSpectrum31f radiance;
                if (shifted_outgoing.y > 0.0f)
                    compute_sky_radiance(shading_context, shifted_outgoing, radiance);
                else radiance.set(0.0f);

                value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
            }

            void evaluate(
                const ShadingContext&   shading_context,
                const Vector3f&         outgoing,                           // world space emission direction, unit-length
                Spectrum&               value,                              // EDF value for this direction
                float&                  probability) const override         // PDF value
            {
                assert(is_normalized(outgoing));

                Transformd scratch;
                const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
                const Vector3f local_outgoing = transform.vector_to_local(outgoing);
                const Vector3f shifted_outgoing = shift(local_outgoing);

                RegularSpectrum31f radiance;
                if (shifted_outgoing.y > 0.0f)
                    compute_sky_radiance(shading_context, shifted_outgoing, radiance);
                else radiance.set(0.0f);

                value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
                probability = shifted_outgoing.y > 0.0f ? shifted_outgoing.y * RcpPi<float>() : 0.0f;
                assert(probability >= 0.0f);
            }

            // Evaluate the PDF for a given emission direction.
            float evaluate_pdf(
                const Vector3f&         outgoing) const override            // world space emission direction, unit-length
            {
                assert(is_normalized(outgoing));

                Transformd scratch;
                const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
                const Vector3f local_outgoing = transform.vector_to_local(outgoing);
                const Vector3f shifted_outgoing = shift(local_outgoing);

                const float probability = shifted_outgoing.y > 0.0f ? shifted_outgoing.y * RcpPi<float>() : 0.0f;
                assert(probability >= 0.0f);

                return probability;
            }


        protected:

            // BEGIN-COPY-PASTA -->
            // Number of virtual spheres and cylinders for the optical length lookup table
            // described in Section 4.3.4 and Fig. 4 of Nishita93.
            static constexpr int kNumSphere = 64;
            static constexpr int kNumCylinder = 64;

            int GetOriginalNumberOfWavelengths() const {
                return DimensionlessSpectrum::SIZE;
            }

            double GetViewSunAngle(double sun_zenith, double view_zenith,
                double view_sun_azimuth) {
                return acos(cos(view_sun_azimuth) * sin(view_zenith) * sin(sun_zenith) +
                    cos(view_zenith) * cos(sun_zenith));
            }

            // Optical lengths between a point of the intersection circle between a sphere
            // and a cylinder, and the Sun. The cylinder axis being the Sun direction.
            dimensional::BinaryFunction<kNumSphere, kNumCylinder, Length>
                rayleigh_optical_length_;
            dimensional::BinaryFunction<kNumSphere, kNumCylinder, Length>
                mie_optical_length_;
            // Same, with the Sun in the opposite direction of the cylinder axis.
            dimensional::BinaryFunction<kNumSphere, kNumCylinder, Length>
                rayleigh_opposite_optical_length_;
            dimensional::BinaryFunction<kNumSphere, kNumCylinder, Length>
                mie_opposite_optical_length_;
            // <-- END-COPY-PASTA

            void precompute_optical_lengths() {
                for (int i = 0; i < kNumSphere; ++i) {
                    Length r = GetSphereRadius(i);
                    Length h = r - EarthRadius;
                    for (int j = 0; j < kNumCylinder; ++j) {
                        Length c = std::min(GetCylinderRadius(j), r);
                        Length rmu = sqrt(r * r - c * c);

                        Length rayleigh_length = 0.0 * m;
                        Length mie_length = 0.0 * m;
                        Number previous_rayleigh_density = exp(-h / RayleighScaleHeight);
                        Number previous_mie_density = exp(-h / MieScaleHeight);
                        Length distance_to_previous_sphere = 0.0 * m;
                        for (int k = i + 1; k < kNumSphere; ++k) {
                            Length r_k = GetSphereRadius(k);
                            Length h_k = r_k - EarthRadius;
                            Length distance_to_sphere = DistanceToSphere(r, rmu, r_k);
                            Number rayleigh_density = exp(-h_k / RayleighScaleHeight);
                            Number mie_density = exp(-h_k / MieScaleHeight);
                            Length segment_length =
                                distance_to_sphere - distance_to_previous_sphere;
                            rayleigh_length += (rayleigh_density + previous_rayleigh_density) / 2 *
                                segment_length;
                            mie_length += (mie_density + previous_mie_density) / 2 * segment_length;
                            previous_rayleigh_density = rayleigh_density;
                            previous_mie_density = mie_density;
                            distance_to_previous_sphere = distance_to_sphere;
                        }
                        rayleigh_optical_length_.Set(i, j, rayleigh_length);
                        mie_optical_length_.Set(i, j, mie_length);

                        rmu = -rmu;
                        rayleigh_length = 0.0 * m;
                        mie_length = 0.0 * m;
                        previous_rayleigh_density = exp(-h / RayleighScaleHeight);
                        previous_mie_density = exp(-h / MieScaleHeight);
                        distance_to_previous_sphere = 0.0 * m;
                        for (int k = i - 1; k > -kNumSphere; --k) {
                            Length r_k = GetSphereRadius(std::abs(k));
                            Length h_k = r_k - EarthRadius;
                            Length distance_to_sphere = DistanceToSphere(r, rmu, r_k);
                            if (distance_to_sphere == 0.0 * m) {
                                continue;
                            }
                            Number rayleigh_density = exp(-h_k / RayleighScaleHeight);
                            Number mie_density = exp(-h_k / MieScaleHeight);
                            Length segment_length =
                                distance_to_sphere - distance_to_previous_sphere;
                            rayleigh_length += (rayleigh_density + previous_rayleigh_density) / 2 *
                                segment_length;
                            mie_length += (mie_density + previous_mie_density) / 2 * segment_length;
                            previous_rayleigh_density = rayleigh_density;
                            previous_mie_density = mie_density;
                            distance_to_previous_sphere = distance_to_sphere;
                        }
                        rayleigh_opposite_optical_length_.Set(i, j, rayleigh_length);
                        mie_opposite_optical_length_.Set(i, j, mie_length);
                    }
                }
            }

            IrradianceSpectrum GetSunIrradiance(Length altitude,
                double sun_zenith) const {
                Length rayleigh_length;
                Length mie_length;
                GetOpticalLengths(EarthRadius + altitude, cos(sun_zenith), &rayleigh_length,
                    &mie_length);
                DimensionlessSpectrum optical_depth = RayleighScattering() * rayleigh_length +
                    MieExtinction() * mie_length;
                DimensionlessSpectrum transmittance(exp(-optical_depth));
                return transmittance * SolarSpectrum();
            }

            RadianceSpectrum GetSkyRadiance(Length altitude, double sun_zenith,
                double view_zenith, double view_sun_azimuth) const {
                Length r = EarthRadius + altitude;
                Number mu_s = cos(sun_zenith);
                Number mu = cos(view_zenith);
                Number nu =
                    cos(view_sun_azimuth) * sin(view_zenith) * sin(sun_zenith) + mu * mu_s;
                Length rmu = r * mu;
                Length rmu_s = r * mu_s;

                WavelengthFunction<1, 0, 0, 0, 0> rayleigh_integral(0.0 * m);
                WavelengthFunction<1, 0, 0, 0, 0> mie_integral(0.0 * m);
                Length rayleigh_length = 0.0 * m;
                Length mie_length = 0.0 * m;
                Number previous_rayleigh_density = exp(-altitude / RayleighScaleHeight);
                Number previous_mie_density = exp(-altitude / MieScaleHeight);
                Length distance_to_previous_sphere = 0.0 * m;
                DimensionlessSpectrum previous_rayleigh_sample(0.0);
                DimensionlessSpectrum previous_mie_sample(0.0);
                for (int i = 0; i < kNumSphere; ++i) {
                    Length r_i = GetSphereRadius(i);
                    if (r_i <= r) {
                        continue;
                    }
                    Length h_i = r_i - EarthRadius;
                    Number rayleigh_density = exp(-h_i / RayleighScaleHeight);
                    Number mie_density = exp(-h_i / MieScaleHeight);
                    Length distance_to_sphere = DistanceToSphere(r, rmu, r_i);
                    Length half_segment_length =
                        (distance_to_sphere - distance_to_previous_sphere) * 0.5;
                    rayleigh_length +=
                        (rayleigh_density + previous_rayleigh_density) * half_segment_length;
                    mie_length += (mie_density + previous_mie_density) * half_segment_length;

                    Length rayleigh_sun_length;
                    Length mie_sun_length;
                    Number mu_s_i = (rmu_s + distance_to_sphere * nu) / r_i;
                    GetOpticalLengths(r_i, mu_s_i, &rayleigh_sun_length, &mie_sun_length);

                    DimensionlessSpectrum optical_depth =
                        RayleighScattering() * (rayleigh_length + rayleigh_sun_length) +
                        MieExtinction() * (mie_length + mie_sun_length);
                    DimensionlessSpectrum transmittance(exp(-optical_depth));
                    DimensionlessSpectrum rayleigh_sample(transmittance * rayleigh_density);
                    DimensionlessSpectrum mie_sample(transmittance * mie_density);
                    rayleigh_integral +=
                        (rayleigh_sample + previous_rayleigh_sample) * half_segment_length;
                    mie_integral += (mie_sample + previous_mie_sample) * half_segment_length;

                    previous_rayleigh_density = rayleigh_density;
                    previous_mie_density = mie_density;
                    distance_to_previous_sphere = distance_to_sphere;
                    previous_rayleigh_sample = rayleigh_sample;
                    previous_mie_sample = mie_sample;
                }

                InverseSolidAngle rayleigh_phase = RayleighPhaseFunction(nu);
                InverseSolidAngle mie_phase = MiePhaseFunction(nu);
                return (rayleigh_integral * RayleighScattering() * rayleigh_phase +
                    mie_integral * MieScattering() * mie_phase) * SolarSpectrum();
            }

            // Returns the optical lengths for rayleigh and mie particles between a point
            // at radius r and the top of the atmosphere in a direction whose angle with
            // the local vertical is acos(mu). This is done by using the precomputed
            // optical length lookup tables.
            void GetOpticalLengths(Length r, Number mu, Length* rayleigh_length,
                Length* mie_length) const {
                const Number a = exp(-(AtmosphereRadius - EarthRadius) / RayleighScaleHeight) - 1.0;
                Number x = (exp(-(r - EarthRadius) / RayleighScaleHeight) - 1.0) / a;
                Number y = r * sqrt(1.0 - mu * mu) / AtmosphereRadius;
                x = 0.5 / kNumSphere + (kNumSphere - 1.0) / kNumSphere * x;
                y = 0.5 / kNumCylinder + (kNumCylinder - 1.0) / kNumCylinder * y;
                if (mu >= 0.0) {
                    *rayleigh_length = rayleigh_optical_length_(x(), y());
                    *mie_length = mie_optical_length_(x(), y());
                }
                else {
                    *rayleigh_length = rayleigh_opposite_optical_length_(x(), y());
                    *mie_length = mie_opposite_optical_length_(x(), y());
                }
            }


            // COPY_PASTA_START -->
            // Computes the sphere radius noted r_i in Nishita93.
            Length GetSphereRadius(int sphere_index) const {
                const Number a = exp(-(AtmosphereRadius - EarthRadius) / RayleighScaleHeight) - 1.0;
                double x = sphere_index / static_cast<double>(kNumSphere - 1);
                return EarthRadius - RayleighScaleHeight * log(a * x + 1.0);
            }
            // <-- COPY_PASTA_END

            // COPY_PASTA_START -->
            // Computes the cylinder radius noted C_j in Nishita93.
            Length GetCylinderRadius(int cylinder_index) const {
                double x = cylinder_index / static_cast<double>(kNumCylinder - 1);
                return AtmosphereRadius * x;
            }
            // <-- COPY_PASTA_END

            // COPY_PASTA_START -->
            // Returns the distance from a point at radius r to the sphere of radius
            // sphere_radius in a direction whose angle with the local vertical is
            // acos(rmu / r), or 0 if there is no intersection.
            Length DistanceToSphere(Length r, Length rmu, Length sphere_radius) const {
                Area delta_sq = sphere_radius * sphere_radius - r * r + rmu * rmu;
                return delta_sq < 0.0 * m2 ? 0.0 * m :
                    (r < sphere_radius ? -rmu + sqrt(delta_sq) : -rmu - sqrt(delta_sq));
            }

        private:
            APPLESEED_DECLARE_INPUT_VALUES(InputValues)
            {
                float   m_sun_theta;                    // sun zenith angle in degrees, 0=zenith
                float   m_sun_phi;                      // degrees
                float   m_turbidity;                    // atmosphere turbidity
                float   m_turbidity_multiplier;
                float   m_luminance_multiplier;
                float   m_luminance_gamma;
                float   m_saturation_multiplier;
                float   m_horizon_shift;
            };

            InputValues                 m_uniform_values;

            float                       m_sun_theta;    // sun zenith angle in radians, 0=zenith
            float                       m_sun_phi;      // radians
            Vector3f                    m_sun_dir;
            float                       m_cos_sun_theta;

            // Compute the sky radiance along a given direction.
            void compute_sky_radiance(
                const ShadingContext&   shading_context,
                const Vector3f&         outgoing,
                RegularSpectrum31f&     radiance) const
            {
                
                Length altitude = 1 * m;     // Must be parametrizable
                double sun_zenith = 1.39626; // Must be parametrizable
                Vector3f *w_zenith = new Vector3f(0.0f, 1.0f, 0.0f);
                double view_zenith = acos(dot(outgoing, *w_zenith));
                delete w_zenith;

                Vector3f *outgoing_flat = new Vector3f(outgoing.x, 0.0f, outgoing.z);
                float mag = sqrt(square(outgoing.x) + square(outgoing.z));
                outgoing_flat->x = outgoing.x / mag;
                outgoing_flat->z = outgoing.z / mag;
                Vector3f *sun_flat = new Vector3f(0.0f, 0.0f, -1.0f);
                double view_sun_azimuth = acos(dot(*outgoing_flat, *sun_flat));


                RadianceSpectrum sky_radiance = GetSkyRadiance(altitude, sun_zenith, view_zenith, view_sun_azimuth);
                std::vector<double> vec = sky_radiance.to(1 * watt_per_square_meter_per_sr_per_nm);
                double* double_array = &vec[0];

                float float_array[31];
                for (int i = 0; i < 31; i++)
                {
                    float_array[i] = (float) double_array[i];
                }

                const RegularSpectrum31f test_radiance(RegularSpectrum31f::from_array(float_array));
                radiance = test_radiance;

                return;
            }

  

        protected:

          

            Vector3f shift(Vector3f v) const
            {
                v.y -= m_uniform_values.m_horizon_shift;
                return normalize(v);
            }

        };


    }


    //
    // ConstantEnvironmentEDFFactory class implementation.
    //

    void Nishita93EnvironmentEDFFactory::release()
    {
        delete this;
    }

    const char* Nishita93EnvironmentEDFFactory::get_model() const
    {
        return Model;
    }

    Dictionary Nishita93EnvironmentEDFFactory::get_model_metadata() const
    {
        return
            Dictionary()
            .insert("name", Model)
            .insert("label", "Nishita93 Environment EDF")
            .insert("help", "Physical sky with single scattering environment");
    }

    DictionaryArray Nishita93EnvironmentEDFFactory::get_input_metadata() const
    {
        DictionaryArray metadata;

        add_common_sky_input_metadata(metadata);

        add_common_input_metadata(metadata);

        return metadata;
    }

    auto_release_ptr<EnvironmentEDF> Nishita93EnvironmentEDFFactory::create(
        const char*         name,
        const ParamArray&   params) const
    {
        return
            auto_release_ptr<EnvironmentEDF>(
                new Nishita93EnvironmentEDF(name, params));
    }

}   // namespace renderer
