
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Artem Bishev, The appleseedhq Organization
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
#include "randomwalkbssrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/volume/distancesample.h"
#include "renderer/modeling/volume/volume.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <algorithm>
#include <cstddef>

// Forward declarations.
namespace foundation    { class Arena; }
namespace renderer      { class BSDFSample; }
namespace renderer      { class BSSRDFSample; }
namespace renderer      { class ShadingContext; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Random-walk BSSRDF.
    //
    // Reference:
    //
    //   Path Traced Subsurface Scattering using Anisotropic Phase Functions
    //   and Non-Exponential Free Flights, Pixar Technical Memo 17-07
    //   https://graphics.pixar.com/library/PathTracedSubsurface/paper.pdf
    //

    const char* Model = "randomwalk_bssrdf";

    class RandomWalkBSSRDF
      : public BSSRDF
    {
      public:
        RandomWalkBSSRDF(
            const char*             name,
            const ParamArray&       params)
          : BSSRDF(name, params)
        {
            m_inputs.declare("weight", InputFormatFloat, "1.0");
            m_inputs.declare("reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("mfp", InputFormatSpectralReflectance);
            m_inputs.declare("mfp_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("ior", InputFormatFloat);
            m_inputs.declare("fresnel_weight", InputFormatFloat, "1.0");

            const string volume_name = string(name) + "_volume";
            m_volume = GenericVolumeFactory().create(volume_name.c_str(), ParamArray()).release();

            const string brdf_name = string(name) + "_brdf";
            m_brdf = LambertianBRDFFactory().create(brdf_name.c_str(), ParamArray()).release();
            m_brdf_data.m_reflectance.set(1.0f);
            m_brdf_data.m_reflectance_multiplier = 1.0f;
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        size_t compute_input_data_size() const override
        {
            return sizeof(RandomWalkBSSRDFInputValues);
        }

        void prepare_inputs(
            Arena&                  arena,
            const ShadingPoint&     shading_point,
            void*                   data) const override
        {
            RandomWalkBSSRDFInputValues* values =
                static_cast<RandomWalkBSSRDFInputValues*>(data);

            new (&values->m_volume_data) GenericVolumeInputValues();

            // Apply multipliers to input values.
            values->m_reflectance *= values->m_reflectance_multiplier;
            values->m_mfp *= values->m_mfp_multiplier;

            // Clamp input values.
            clamp_in_place(values->m_reflectance, 0.001f, 0.999f);
            clamp_low_in_place(values->m_mfp, 1.0e-6f);

            // Precompute scaling factor. 
            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                const float albedo = albedo_from_reflectance(values->m_reflectance[i]);
                const float s = normalized_diffusion_s_mfp(values->m_reflectance[i]);
                const float extinction = rcp(values->m_mfp[i] * s);

                values->m_volume_data.m_precomputed.m_extinction[i] = extinction;
                values->m_volume_data.m_scattering[i] = albedo * extinction;
                values->m_volume_data.m_absorption[i] = (1.0f - albedo) * extinction;
            }
        }

        static float albedo_from_reflectance(const float r)
        {
            return 1.0f - exp(r * (-5.09406f + r * (2.61188f - 4.31805f * r)));
        }

        bool sample(
            const ShadingContext&   shading_context,
            SamplingContext&        sampling_context,
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            BSSRDFSample&           bssrdf_sample,
            BSDFSample&             bsdf_sample) const override
        {
            const RandomWalkBSSRDFInputValues* values =
                static_cast<const RandomWalkBSSRDFInputValues*>(data);

            // Pick initial random-walk direction.
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next2<Vector2d>();
            Vector3d initial_dir = sample_hemisphere_cosine(s);
            initial_dir.y = -initial_dir.y;
            initial_dir = outgoing_point.get_shading_basis().transform_to_parent(initial_dir);
            ShadingRay ray = ShadingRay(
                outgoing_point.get_biased_point(initial_dir),
                initial_dir,
                outgoing_point.get_time(),
                VisibilityFlags::ShadowRay,
                outgoing_point.get_ray().m_depth + 1
            );

            ShadingPoint shading_points[2];
            int next_shading_point_idx = 0;

            DistanceSample distance_sample;
            distance_sample.m_pivot = nullptr;
            distance_sample.m_outgoing_point = &outgoing_point;
            distance_sample.m_incoming_point = &shading_points[next_shading_point_idx];
            distance_sample.m_incoming_point->clear();
            distance_sample.m_volume_ray = &ray;
            distance_sample.m_transmitted = false;

            // Initialize BSSRDF value.
            bssrdf_sample.m_value.set(1.0f);
            bssrdf_sample.m_probability = 1.0f;

            // Do random walk until we reach the surface from inside.
            int n_iteration = 0;
            const int MaxIterationsCount = 64;
            while (!distance_sample.m_transmitted)
            {
                if (++n_iteration > MaxIterationsCount) break;

                m_volume->sample_distance(
                    shading_context,
                    sampling_context,
                    &values->m_volume_data,
                    distance_sample);

                assert(distance_sample.m_incoming_point->is_valid());

                if (!distance_sample.m_transmitted)
                {
                    bssrdf_sample.m_value *= values->m_volume_data.m_scattering;
                    bssrdf_sample.m_value /= values->m_volume_data.m_precomputed.m_extinction;

                    // Find next random-walk direction using isotropic scattering.
                    sampling_context.split_in_place(2, 1);
                    const Vector2d s = sampling_context.next2<Vector2d>();
                    ray = ShadingRay(
                        shading_points[next_shading_point_idx].get_point(),
                        sample_sphere_uniform(s),
                        outgoing_point.get_time(),
                        VisibilityFlags::ShadowRay,
                        outgoing_point.get_ray().m_depth + 1
                    );
                    distance_sample.m_value.set(1.0f);
                    distance_sample.m_volume_ray = &ray;

                    // Update shading points.
                    next_shading_point_idx = 1 - next_shading_point_idx;
                    distance_sample.m_outgoing_point = distance_sample.m_incoming_point;
                    distance_sample.m_incoming_point = &shading_points[next_shading_point_idx];
                    distance_sample.m_incoming_point->clear();
                }
            }

            if (!distance_sample.m_transmitted)
            {
                bssrdf_sample.m_value.set(0.0f);
                return false; // path got lost inside the object
            }

            bssrdf_sample.m_brdf = m_brdf;
            bssrdf_sample.m_brdf_data = &m_brdf_data;
            bssrdf_sample.m_incoming_point = *distance_sample.m_incoming_point;
            bssrdf_sample.m_incoming_point.flip_side();

            // Sample the BSDF at the incoming point.
            bsdf_sample.m_shading_point = &bssrdf_sample.m_incoming_point;
            bsdf_sample.m_geometric_normal = Vector3f(bssrdf_sample.m_incoming_point.get_geometric_normal());
            bsdf_sample.m_shading_basis = Basis3f(bssrdf_sample.m_incoming_point.get_shading_basis());
            bsdf_sample.m_outgoing = Dual3f(outgoing_dir);      // chosen arbitrarily (no outgoing direction at the incoming point)
            bssrdf_sample.m_brdf->sample(
                sampling_context,
                bssrdf_sample.m_brdf_data,
                false,
                true,
                ScatteringMode::All,
                bsdf_sample);

            return true;
        }

        void evaluate(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3f&         incoming_dir,
            Spectrum&               value) const override
        {
            const RandomWalkBSSRDFInputValues* values =
                static_cast<const RandomWalkBSSRDFInputValues*>(data);

            throw ExceptionNotImplemented();
        }

        const Volume* m_volume;
        const BSDF*                     m_brdf;
        LambertianBRDFInputValues       m_brdf_data;
    };
}


//
// RandomWalkBSSRDFFactory class implementation.
//

void RandomWalkBSSRDFFactory::release()
{
    delete this;
}

const char* RandomWalkBSSRDFFactory::get_model() const
{
    return Model;
}

Dictionary RandomWalkBSSRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Random-Walk BSSRDF");
}

DictionaryArray RandomWalkBSSRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "weight")
            .insert("label", "Weight")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance")
            .insert("label", "Diffuse Surface Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance_multiplier")
            .insert("label", "Diffuse Surface Reflectance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "mfp")
            .insert("label", "Mean Free Path")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "mfp_multiplier")
            .insert("label", "Mean Free Path Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ior")
            .insert("label", "Index of Refraction")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "2.5")
                    .insert("type", "hard"))
            .insert("use", "required")
            .insert("default", "1.3"));

    metadata.push_back(
        Dictionary()
            .insert("name", "fresnel_weight")
            .insert("label", "Fresnel Weight")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<BSSRDF> RandomWalkBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new RandomWalkBSSRDF(name, params));
}

}   // namespace renderer
