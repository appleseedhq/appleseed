
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

#include "b_binary_function.h"
#include "skyconstants.h"

// Forward declarations.
namespace foundation { class IAbortSwitch; }
namespace renderer { class Project; }

using namespace foundation;

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

            Vector3f shift(Vector3f v) const
            {
                v.y -= m_uniform_values.m_horizon_shift;
                return normalize(v);
            }

            // BEGIN-COPY-PASTA -->
            // Number of virtual spheres and cylinders for the optical length lookup table
            // described in Section 4.3.4 and Fig. 4 of Nishita93.
            static constexpr int kNumSphere = 64;
            static constexpr int kNumCylinder = 64;

            // Optical lengths between a point of the intersection circle between a sphere
            // and a cylinder, and the Sun. The cylinder axis being the Sun direction.
            dimensional::BinaryFunction<kNumSphere, kNumCylinder, double> rayleigh_optical_length_;
            dimensional::BinaryFunction<kNumSphere, kNumCylinder, double> mie_optical_length_;

            // Same, with the Sun in the opposite direction of the cylinder axis.
            dimensional::BinaryFunction<kNumSphere, kNumCylinder, double> rayleigh_opposite_optical_length_;
            dimensional::BinaryFunction<kNumSphere, kNumCylinder, double> mie_opposite_optical_length_;

            void precompute_optical_lengths() {
                for (int sphere_index = 0; sphere_index < kNumSphere; ++sphere_index) {
                    double sphere_radius = GetSphereRadius(sphere_index);
                    double sphere_height = sphere_radius - EarthRadius;
                    for (int cylinder_index = 0; cylinder_index < kNumCylinder; ++cylinder_index) {

                        double c = std::min(GetCylinderRadius(cylinder_index), sphere_radius);
                        double rmu = sqrt(sphere_radius * sphere_radius - c * c);

                        double rayleigh_length = 0.0;
                        double mie_length = 0.0;

                        double previous_rayleigh_density = exp(-sphere_height / RayleighScaleHeight);
                        double previous_mie_density = exp(-sphere_height / MieScaleHeight);

                        double distance_to_previous_sphere = 0.0;

                        for (int k = sphere_index + 1; k < kNumSphere; ++k) {
                            double r_k = GetSphereRadius(k);
                            double h_k = r_k - EarthRadius;
                            double distance_to_sphere = DistanceToSphere(sphere_radius, rmu, r_k);

                            double rayleigh_density = exp(-h_k / RayleighScaleHeight);
                            double mie_density = exp(-h_k / MieScaleHeight);

                            double segment_length = distance_to_sphere - distance_to_previous_sphere;

                            rayleigh_length += (rayleigh_density + previous_rayleigh_density) / 2 * segment_length;
                            mie_length += (mie_density + previous_mie_density) / 2 * segment_length;

                            previous_rayleigh_density = rayleigh_density;
                            previous_mie_density = mie_density;

                            distance_to_previous_sphere = distance_to_sphere;
                        }

                        rayleigh_optical_length_.Set(sphere_index, cylinder_index, rayleigh_length);
                        mie_optical_length_.Set(sphere_index, cylinder_index, mie_length);

                        rmu = -rmu;

                        rayleigh_length = 0.0;
                        mie_length = 0.0;

                        previous_rayleigh_density = exp(-sphere_height / RayleighScaleHeight);
                        previous_mie_density = exp(-sphere_height / MieScaleHeight);

                        distance_to_previous_sphere = 0.0;

                        for (int k = sphere_index - 1; k > -kNumSphere; --k) {

                            double r_k = GetSphereRadius(std::abs(k));
                            double h_k = r_k - EarthRadius;
                            double distance_to_sphere = DistanceToSphere(sphere_radius, rmu, r_k);

                            if (distance_to_sphere == 0.0) {
                                continue;
                            }

                            double rayleigh_density = exp(-h_k / RayleighScaleHeight);
                            double mie_density = exp(-h_k / MieScaleHeight);

                            double segment_length = distance_to_sphere - distance_to_previous_sphere;

                            rayleigh_length += (rayleigh_density + previous_rayleigh_density) / 2 * segment_length;
                            mie_length += (mie_density + previous_mie_density) / 2 * segment_length;

                            previous_rayleigh_density = rayleigh_density;
                            previous_mie_density = mie_density;

                            distance_to_previous_sphere = distance_to_sphere;
                        }

                        rayleigh_opposite_optical_length_.Set(sphere_index, cylinder_index, rayleigh_length);
                        mie_opposite_optical_length_.Set(sphere_index, cylinder_index, mie_length);
                    }
                }
            }

            RegularSpectrum31f GetSunIrradiance(double altitude, double sun_zenith) const {
                double rayleigh_length;
                double mie_length;

                GetOpticalLengths(EarthRadius + altitude, cos(sun_zenith), &rayleigh_length, &mie_length);

                RegularSpectrum31f optical_depth = RayleighScattering() * RegularSpectrum31f(rayleigh_length) + MieExtinction() * RegularSpectrum31f(mie_length);

                float depths[31];
                for (int i = 0; i < 31; i++) {
                    depths[i] = exp(-optical_depth[i]);
                }

                RegularSpectrum31f transmittance = RegularSpectrum31f::from_array(depths);
                return transmittance * SolarSpectrum();
            }

            // Returns the Spectral radiance for a point in sky, where the viewer is
            // 'altitude' meter in altitude, the sun is 'sun_zenith' rad below the zenith, the viewer looks at a point
            // 'view_zenith' rad under the zenith, and the angle in rad between the sun and the viewer is 'view_sun_azimuth'
            RegularSpectrum31f GetSkyRadiance(double altitude, double sun_zenith, double view_zenith, double view_sun_azimuth) const {
                double r = EarthRadius + altitude;
                double mu_s = cos(sun_zenith);
                double mu = cos(view_zenith);
                double nu = cos(view_sun_azimuth) * sin(view_zenith) * sin(sun_zenith) + mu * mu_s;
                double rmu = r * mu;
                double rmu_s = r * mu_s;

                RegularSpectrum31f rayleigh_integral(0.0);
                RegularSpectrum31f mie_integral(0.0);

                double rayleigh_length = 0.0;
                double mie_length = 0.0 ;

                double previous_rayleigh_density = exp(-altitude / RayleighScaleHeight);
                double previous_mie_density = exp(-altitude / MieScaleHeight);

                double distance_to_previous_sphere = 0.0;

                RegularSpectrum31f previous_rayleigh_sample(0.0);
                RegularSpectrum31f previous_mie_sample(0.0);

                for (int i = 0; i < kNumSphere; ++i) {

                    double r_i = GetSphereRadius(i);
                    if (r_i <= r) {
                        continue;
                    }

                    double h_i = r_i - EarthRadius;

                    double rayleigh_density = exp(-h_i / RayleighScaleHeight);
                    double mie_density = exp(-h_i / MieScaleHeight);

                    double distance_to_sphere = DistanceToSphere(r, rmu, r_i);
                    double half_segment_length = (distance_to_sphere - distance_to_previous_sphere) * 0.5;

                    rayleigh_length += (rayleigh_density + previous_rayleigh_density) * half_segment_length;
                    mie_length += (mie_density + previous_mie_density) * half_segment_length;

                    double rayleigh_sun_length;
                    double mie_sun_length;

                    double mu_s_i = (rmu_s + distance_to_sphere * nu) / r_i;

                    GetOpticalLengths(r_i, mu_s_i, &rayleigh_sun_length, &mie_sun_length);

                    RegularSpectrum31f optical_depth = RayleighScattering() * RegularSpectrum31f(rayleigh_length + rayleigh_sun_length) + MieExtinction() * RegularSpectrum31f(mie_length + mie_sun_length);

                    float depths[31];
                    for (int i = 0; i < 31; i++) {
                        depths[i] = exp(-optical_depth[i]);
                    }
                    RegularSpectrum31f transmittance = RegularSpectrum31f::from_array(depths);

                    RegularSpectrum31f rayleigh_sample(transmittance * RegularSpectrum31f(rayleigh_density));
                    RegularSpectrum31f mie_sample(transmittance * RegularSpectrum31f(mie_density));

                    rayleigh_integral += (rayleigh_sample + previous_rayleigh_sample) * RegularSpectrum31f(half_segment_length);
                    mie_integral += (mie_sample + previous_mie_sample) * RegularSpectrum31f(half_segment_length);

                    previous_rayleigh_density = rayleigh_density;
                    previous_mie_density = mie_density;

                    distance_to_previous_sphere = distance_to_sphere;

                    previous_rayleigh_sample = rayleigh_sample;
                    previous_mie_sample = mie_sample;
                }

                double rayleigh_phase = RayleighPhaseFunction(nu);
                double mie_phase = MiePhaseFunction(nu);

                return (rayleigh_integral * RayleighScattering() * RegularSpectrum31f(rayleigh_phase) + mie_integral * MieScattering() * RegularSpectrum31f(mie_phase)) * SolarSpectrum();
            }

            // Returns the optical lengths for rayleigh and mie particles between a point
            // at radius r and the top of the atmosphere in a direction whose angle with
            // the local vertical is acos(mu). This is done by using the precomputed
            // optical length lookup tables.
            void GetOpticalLengths(double r, double mu, double* rayleigh_length, double* mie_length) const {
                const double a = exp(-(AtmosphereRadius - EarthRadius) / RayleighScaleHeight) - 1.0;
                double x = (exp(-(r - EarthRadius) / RayleighScaleHeight) - 1.0) / a;
                double y = r * sqrt(1.0 - mu * mu) / AtmosphereRadius;
                x = 0.5 / kNumSphere + (kNumSphere - 1.0) / kNumSphere * x;
                y = 0.5 / kNumCylinder + (kNumCylinder - 1.0) / kNumCylinder * y;
                if (mu >= 0.0) {
                    *rayleigh_length = rayleigh_optical_length_(x, y);
                    *mie_length = mie_optical_length_(x, y);
                }
                else {
                    *rayleigh_length = rayleigh_opposite_optical_length_(x, y);
                    *mie_length = mie_opposite_optical_length_(x, y);
                }
            }

            // Computes the sphere radius noted r_i in Nishita93.
            double GetSphereRadius(int sphere_index) const {
                const double a = exp(-(AtmosphereRadius - EarthRadius) / RayleighScaleHeight) - 1.0;
                double x = sphere_index / static_cast<double>(kNumSphere - 1);
                return EarthRadius - RayleighScaleHeight * log(a * x + 1.0);
            }

            // Computes the cylinder radius noted C_j in Nishita93.
            double GetCylinderRadius(int cylinder_index) const {
                double x = cylinder_index / static_cast<double>(kNumCylinder - 1);
                return AtmosphereRadius * x;
            }

            // Returns the distance from a point at radius r to the sphere of radius
            // sphere_radius in a direction whose angle with the local vertical is
            // acos(rmu / r), or 0 if there is no intersection.
            double DistanceToSphere(double r, double rmu, double sphere_radius) const {
                double delta_sq = sphere_radius * sphere_radius - r * r + rmu * rmu;
                return std::max(0.0, -rmu + sqrt(delta_sq));
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
                
                double altitude = 1;     // Must be parametrizable
                Vector3f w_zenith(0.0f, 1.0f, 0.0f);
                double view_zenith = acos(dot(outgoing, w_zenith));

                Vector3f *outgoing_flat = new Vector3f(outgoing.x, 0.0f, outgoing.z);
                float mag = sqrt(square(outgoing.x) + square(outgoing.z));
                outgoing_flat->x = outgoing.x / mag;
                outgoing_flat->z = outgoing.z / mag;
                Vector3f *sun_flat = new Vector3f(0.0f, 0.0f, -1.0f);
                double view_sun_azimuth = acos(dot(*outgoing_flat, *sun_flat)) + m_sun_phi;

                radiance = GetSkyRadiance(altitude, m_sun_theta, view_zenith, view_sun_azimuth) * RegularSpectrum31f(0.8);

                return;
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
