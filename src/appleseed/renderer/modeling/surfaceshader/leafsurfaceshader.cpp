
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "leafsurfaceshader.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/lighting/directlighting.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingpointbuilder.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aovcollection.h"
#include "renderer/modeling/bsdf/bsdf.h"
//#include "renderer/modeling/environment/environment.h"
//#include "renderer/modeling/environmentshader/environmentshader.h"
//#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/project/project.h"
//#include "renderer/modeling/scene/scene.h"
//#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
//#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
//#include <algorithm>
//#include <cmath>
#include <cstddef>
#include <memory>

// Forward declarations.
//namespace renderer  { class Assembly; }
namespace renderer      { class Scene; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Leaf surface shader.
    //

    const char* Model = "leaf_surface_shader";

    class LeafSurfaceShader
      : public PhysicalSurfaceShader
    {
      public:
        LeafSurfaceShader(
            const char*             name,
            const ParamArray&       params)
          : PhysicalSurfaceShader(name, params)
        {
        }

        virtual void release() override
        {
            delete this;
        }

        virtual const char* get_model() const override
        {
            return Model;
        }

        virtual void LeafSurfaceShader::on_frame_begin(
            const Project&          project,
            const Assembly&         assembly) override
        {
            PhysicalSurfaceShader::on_frame_begin(project, assembly);

            const Scene& scene = *project.get_scene();
            m_light_sampler.reset(new LightSampler(scene));
        }

        virtual void evaluate(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingResult&          shading_result) const override
        {
            if (shading_point.get_side() == ObjectInstance::FrontSide)
            {
                PhysicalSurfaceShader::evaluate(
                    sampling_context,
                    shading_context,
                    shading_point,
                    shading_result);
            }
            else
            {
                ShadingRay flipped_ray(shading_point.get_ray());
                flipped_ray.m_dir =
                    -reflect(
                        shading_point.get_ray().m_dir,
                        shading_point.get_original_shading_normal());
                flipped_ray.m_org = shading_point.get_point() - shading_point.get_distance() * flipped_ray.m_dir;

                ShadingPoint flipped_shading_point(shading_point);
                flipped_shading_point.set_ray(flipped_ray);

                // Retrieve the lighting engine.
                ILightingEngine* lighting_engine =
                    shading_context.get_lighting_engine();
                assert(lighting_engine);

                // Compute the lighting.
                shading_result.m_color_space = ColorSpaceSpectral;
                lighting_engine->compute_lighting(
                    sampling_context,
                    shading_context,
                    flipped_shading_point,
                    shading_result.m_color,
                    shading_result.m_aovs);

                // Handle alpha mapping.
                const Material* material = flipped_shading_point.get_material();
                if (material && material->get_alpha_map())
                {
                    // Evaluate the alpha map at the shading point.
                    material->get_alpha_map()->evaluate(
                        shading_context.get_texture_cache(),
                        flipped_shading_point.get_uv(0),
                        shading_result.m_alpha);
                }
                else shading_result.m_alpha = Alpha(1.0);

#if 0
                const size_t BSDFSampleCount = 1;
                const size_t LightSampleCount = 1;

                const Material* material = shading_point.get_material();
                if (material == 0)
                {
                    shading_result.set_to_solid_pink();
                    return;
                }

                const BSDF* bsdf = material->get_bsdf();
                if (bsdf == 0)
                {
                    shading_result.set_to_solid_pink();
                    return;
                }

                // Evaluate the input values of the BSDF.
                InputEvaluator bsdf_input_evaluator(shading_context.get_texture_cache());
                bsdf->evaluate_inputs(bsdf_input_evaluator, shading_point.get_uv(0));

                const Vector3d to_camera = -shading_point.get_ray().m_dir;
                const Vector3d outgoing = -reflect(to_camera, shading_point.get_shading_normal());

                DirectLightingIntegrator integrator(
                    shading_context,
                    *m_light_sampler.get(),
                    shading_point.get_point(),
                    -shading_point.get_geometric_normal(),
                    Basis3d(-shading_point.get_shading_normal()),
                    shading_point.get_ray().m_time,
                    outgoing,
                    *bsdf,
                    bsdf_input_evaluator.data(),
                    BSDFSampleCount,
                    LightSampleCount,
                    &shading_point);
                Spectrum radiance;
                //AOVCollection vertex_aovs(m_path_aovs.size());
                AOVCollection aovs;
                integrator.sample_lights(sampling_context, shading_result.m_color, aovs);
                shading_result.m_color_space = ColorSpaceSpectral;

                // Handle alpha mapping.
                if (material && material->get_alpha_map())
                {
                    // Evaluate the alpha map at the shading point.
                    material->get_alpha_map()->evaluate(
                        shading_context.get_texture_cache(),
                        shading_point.get_uv(0),
                        shading_result.m_alpha);
                }
                else shading_result.m_alpha = Alpha(1.0);
#endif
            }
        }

      private:
        auto_ptr<LightSampler> m_light_sampler;
    };
}


//
// LeafSurfaceShaderFactory class implementation.
//

const char* LeafSurfaceShaderFactory::get_model() const
{
    return Model;
}

const char* LeafSurfaceShaderFactory::get_human_readable_model() const
{
    return "Leaf";
}

DictionaryArray LeafSurfaceShaderFactory::get_widget_definitions() const
{
    DictionaryArray definitions = PhysicalSurfaceShaderFactory::get_widget_definitions();

    return definitions;
}

auto_release_ptr<SurfaceShader> LeafSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<SurfaceShader>(
            new LeafSurfaceShader(name, params));
}

}   // namespace renderer
