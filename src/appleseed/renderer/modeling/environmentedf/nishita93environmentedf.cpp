
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Joel Barmettler, The appleseedhq Organization
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
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/environmentedf/environmentedf.h"

// appleseed.foundation headers.
#include "foundation/math/ray.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cassert>
#include <cmath>

#include "physicalsky.h"

// Forward declarations.
namespace foundation { class IAbortSwitch; }
namespace renderer { class Project; }

using namespace foundation;

namespace renderer
{

    namespace
    {

        //
        // An environment EDF implementing the Nishita93 day sky model.
        //
        // Reference:
        //
        //   http://nishitalab.org/user/nis/cdrom/sig93_nis.pdf
        //

        const char* Model = "nishita93_environment_edf";

        class Nishita93EnvironmentEDF
            : public EnvironmentEDF
        {
        public:
            Nishita93EnvironmentEDF(
                const char*             name,
                const ParamArray&       params)
                : EnvironmentEDF(name, params)
            {
                m_inputs.declare("sun_theta", InputFormat::Float, "45.0");
                m_inputs.declare("sun_phi", InputFormat::Float, "0.0");
                m_inputs.declare("sun_intensity_multiplier", InputFormat::Float, "1.0");
                m_inputs.declare("elevation", InputFormat::Float, "0.0");
                m_inputs.declare("air_molecule_density", InputFormat::Float, "1.0");
                m_inputs.declare("dust_molecule_density", InputFormat::Float, "1.0");
                m_inputs.declare("ozone_molecule_density", InputFormat::Float, "1.0");
                m_inputs.declare("haze", InputFormat::Float, "0.8");
                m_inputs.declare("horizon_shift", InputFormat::Float, "0.0");
                m_inputs.declare("sun_angular_diameter", InputFormat::Float, "0.545");
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
                if (!EnvironmentEDF::on_frame_begin(project, parent, recorder, abort_switch))
                    return false;

                // Evaluate uniform values.
                m_inputs.evaluate_uniforms(&m_uniform_values);

                // Set user input values from user interface.
                m_sun_theta = deg_to_rad(m_uniform_values.m_sun_theta);
                m_sun_phi = deg_to_rad(m_uniform_values.m_sun_phi);
                m_sun_intensity_multiplier = m_uniform_values.m_sun_intensity_multiplier;
                m_elevation = m_uniform_values.m_elevation;
                m_air_molecule_density = m_uniform_values.m_air_molecule_density;
                m_dust_molecule_density = m_uniform_values.m_dust_molecule_density;
                m_ozone_molecule_density = m_uniform_values.m_ozone_molecule_density;
                m_haze = m_uniform_values.m_haze;
                m_sun_angular_diameter = deg_to_rad(m_uniform_values.m_sun_angular_diameter);
                m_precompute = m_params.get_optional<bool>("precompute", true);

                // Compute the sun direction.
                sun_dir = Vector3f::make_unit_vector(m_sun_theta, m_sun_phi);

                // Precompute nishitas lookup table for optical depths.
                sky_precomputations();

                return true;
            }

            void sample(
                const ShadingContext&   shading_context,
                const Vector2f&         s,
                Vector3f&               outgoing,
                Spectrum&               value,
                float&                  probability) const override
            {
                const Vector3f local_outgoing = sample_hemisphere_cosine(s);

                Transformd scratch;
                const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
                outgoing = transform.vector_to_parent(local_outgoing);
                const Vector3f shifted_outgoing = shift(local_outgoing);

                RegularSpectrum31f radiance;
                compute_sky_radiance(shading_context, shifted_outgoing, radiance);

                value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
                probability = shifted_outgoing.y > 0.0f ? shifted_outgoing.y * RcpPi<float>() : 0.0f;
                assert(probability >= 0.0f);
            }

            void evaluate(
                const ShadingContext&   shading_context,
                const Vector3f&         outgoing,
                Spectrum&               value) const override
            {
                assert(is_normalized(outgoing));

                Transformd scratch;
                const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
                const Vector3f local_outgoing = transform.vector_to_local(outgoing);
                const Vector3f shifted_outgoing = shift(local_outgoing);

                RegularSpectrum31f radiance;
                compute_sky_radiance(shading_context, shifted_outgoing, radiance);

                value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
            }

            void evaluate(
                const ShadingContext&   shading_context,
                const Vector3f&         outgoing,
                Spectrum&               value,
                float&                  probability) const override
            {
                assert(is_normalized(outgoing));

                Transformd scratch;
                const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
                const Vector3f local_outgoing = transform.vector_to_local(outgoing);
                const Vector3f shifted_outgoing = shift(local_outgoing);

                RegularSpectrum31f radiance;
                compute_sky_radiance(shading_context, shifted_outgoing, radiance);

                value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
                probability = shifted_outgoing.y > 0.0f ? shifted_outgoing.y * RcpPi<float>() : 0.0f;
                assert(probability >= 0.0f);
            }

            float evaluate_pdf(
                const Vector3f&         outgoing) const override
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

        private:
            APPLESEED_DECLARE_INPUT_VALUES(InputValues)
            {
                float   m_sun_theta;                    // angle (deg) between sun and zenith, 0=zenith
                float   m_sun_phi;                      // angle (deg) between sun and north, 0=north
                float   m_sun_intensity_multiplier;     // increases or decreases the returned radiance values
                float   m_elevation;                    // elevation of camera above earth surface (m)   
                float   m_air_molecule_density;         // multiplier of air molecule density, affecting rayleigh scattering
                float   m_dust_molecule_density;        // multiplier of dust molecule density, affecting mie scattering
                float   m_ozone_molecule_density;       // multiplier of ozone molecule density, affecting ozone scattering
                float   m_haze;                         // u parameter for Cornette phase function used for Mie scattering
                float   m_horizon_shift;
                float   m_sun_angular_diameter;         // rays with angle (rad) +- sun_angular_diameter will return sun radiance directly
            };

            InputValues                 m_uniform_values;

            float                       m_sun_theta;    // angle (rad) between sun and zenith, 0=zenith
            float                       m_sun_phi;      // angle (rad) between sun and north, 0=north
            Vector3f                    sun_dir;        // vector pointing into the direction of the sun

            float                       m_sun_intensity_multiplier;         // increases or decreases the returned radiance values
            float                       m_elevation;                        // elevation of camera above earth surface (m)    
            float                       m_air_molecule_density;             // multiplier of air molecule density, affecting rayleigh scattering
            float                       m_dust_molecule_density;            // multiplier of dust molecule density, affecting mie scattering
            float                       m_ozone_molecule_density;           // multiplier of ozone molecule density, affecting ozone scattering
            float                       m_haze;                             // u parameter for Cornette phase function used for Mie scattering
            float                       m_sun_angular_diameter;             // rays with angle (rad) +- sun_angular_diameter will return sun radiance directly
            bool                        m_precompute;                       // use 2D lookup table to precompute sky optical depths

            // Fills a 3D precomputation table with optical depths value along the sun direction.
            void sky_precomputations() {
                nishita::precompute_mie_g(m_haze);
                nishita::precompute_shells();
                if (m_precompute)
                    nishita::precompute_optical_depths(sun_dir, m_air_molecule_density, m_dust_molecule_density, m_ozone_molecule_density);
            }

            // Compute the sky radiance along a given direction.
            void compute_sky_radiance(
                const ShadingContext&   shading_context,
                const Vector3f&         outgoing,
                RegularSpectrum31f&     radiance) const
            {

                // Position of the camera at least 1.2 meters above earth radius to avoid numerical errors.
                Vector3f camera_position = Vector3f(0.0f, earth_radius + 1.2f + m_elevation, 0.0f);
                Ray3f ray = Ray3f(camera_position, outgoing);

                // If the outoing vector points to the sun, return suns spectrum.
                float sun_angular_radius = m_sun_angular_diameter / 2.0f;
                float is_sun = norm(outgoing - sun_dir) < sun_angular_radius;
                if (is_sun) {
                    const bool sun_hit = nishita::sun_disk(
                        ray,
                        m_air_molecule_density,     // air molecule density (Rayleigh scattering)
                        m_dust_molecule_density,    // dust molecule density (Mie scattering)
                        m_ozone_molecule_density,   // ozone molecule density (Ozone scattering)
                        sun_angular_radius,
                        radiance
                    );
                    if (sun_hit)
                        return;
                }

                // Compute the final sky radiance.
                nishita::single_scattering(
                    ray,
                    sun_dir,                    // sun direction
                    m_air_molecule_density,     // air molecule density (Rayleigh scattering)
                    m_dust_molecule_density,    // dust molecule density (Mie scattering)
                    m_ozone_molecule_density,   // ozone molecule density (Ozone scattering)
                    m_precompute,               // use precomputed lookup table
                    radiance
                );
                radiance *=
                    m_uniform_values.m_sun_intensity_multiplier     // multiply sun intensity
                    * 1.5f;         // since nishita93 underestimates radiance by 1/3 according to Bruneton
            }

            Vector3f shift(Vector3f v) const
            {
                v.y -= m_uniform_values.m_horizon_shift;
                return normalize(v);
            }
        };
    }


    //
    // Nishita93EnvironmentEDFFactory class implementation.
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

        metadata.push_back(
            Dictionary()
            .insert("name", "sun_theta")
            .insert("label", "Sun Theta Angle")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                .insert("value", "0.0")
                .insert("type", "hard"))
            .insert("max",
                Dictionary()
                .insert("value", "100.0")
                .insert("type", "hard"))
            .insert("use", "required")
            .insert("default", "45.0")
            .insert("help", "Sun polar (vertical) angle in degrees"));

        metadata.push_back(
            Dictionary()
            .insert("name", "sun_phi")
            .insert("label", "Sun Phi Angle")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                .insert("value", "-360.0")
                .insert("type", "soft"))
            .insert("max",
                Dictionary()
                .insert("value", "360.0")
                .insert("type", "soft"))
            .insert("use", "required")
            .insert("default", "0.0")
            .insert("help", "Sun azimuthal (horizontal) angle in degrees"));

        metadata.push_back(
            Dictionary()
            .insert("name", "sun_intensity_multiplier")
            .insert("label", "Sun intensity multiplier")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                .insert("value", "0.0")
                .insert("type", "hard"))
            .insert("max",
                Dictionary()
                .insert("value", "10.0")
                .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Multiplies sun intensity with constant factor"));

        metadata.push_back(
            Dictionary()
            .insert("name", "air_molecule_density")
            .insert("label", "Air Molecule Density")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                .insert("value", "0.0")
                .insert("type", "hard"))
            .insert("max",
                Dictionary()
                .insert("value", "5.0")
                .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Air molecule density affect Rayleigh scattering (blueness of sky)"));

        metadata.push_back(
            Dictionary()
            .insert("name", "dust_molecule_density")
            .insert("label", "Dust Molecule Density")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                .insert("value", "0.0")
                .insert("type", "hard"))
            .insert("max",
                Dictionary()
                .insert("value", "5.0")
                .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Dust molecule density affect Mie scattering (turbidity)"));

        metadata.push_back(
            Dictionary()
            .insert("name", "ozone_molecule_density")
            .insert("label", "Ozone Molecule Density")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                .insert("value", "0.0")
                .insert("type", "hard"))
            .insert("max",
                Dictionary()
                .insert("value", "5.0")
                .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Ozone molecules effect the sky color at twilight"));

        metadata.push_back(
            Dictionary()
            .insert("name", "haze")
            .insert("label", "Haze in the atmosphere")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                .insert("value", "0.7")
                .insert("type", "hard"))
            .insert("max",
                Dictionary()
                .insert("value", "0.8")
                .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "0.8")
            .insert("help", "Haze changes effect of dust particles on sunlight"));

        metadata.push_back(
            Dictionary()
            .insert("name", "elevation")
            .insert("label", "Elevation")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "0")
            .insert("help", "Elevates camera above earths surface"));

        metadata.push_back(
            Dictionary()
            .insert("name", "sun_angular_diameter")
            .insert("label", "Sun Size")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "0.545")
            .insert("help", "Angular diameter of sun disk, make 0 to hide sun"));

        metadata.push_back(
            Dictionary()
            .insert("name", "horizon_shift")
            .insert("label", "Horizon Shift")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "0.0")
            .insert("help", "Shift the horizon vertically"));

        metadata.push_back(
            Dictionary()
            .insert("name", "precompute")
            .insert("label", "Precalculate sky")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "true")
            .insert("help", "If enabled, precalculations make sky fast but slightly less accurate"));

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

}
