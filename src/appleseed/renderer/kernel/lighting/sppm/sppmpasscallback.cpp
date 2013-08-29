
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "sppmpasscallback.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/sppm/sppmgatherpoint.h"
#include "renderer/kernel/lighting/sppm/sppmgatherpointtracer.h"
#include "renderer/kernel/lighting/sppm/sppmphoton.h"
#include "renderer/kernel/lighting/sppm/sppmphotonmap.h"
#include "renderer/kernel/lighting/sppm/sppmphotontracer.h"
#include "renderer/kernel/rendering/generic/genericframerenderer.h"
#include "renderer/kernel/rendering/generic/generictilerenderer.h"
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/kernel/rendering/itilerenderer.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/knn.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cmath>
#include <memory>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// SPPMPassCallback class implementation.
//
// References:
//
//   Progressive Photon Mapping: A Probabilistic Approach
//   Claude Knaus, Matthias Zwicker
//   http://www.cgg.unibe.ch/publications/2011/progressive-photon-mapping-a-probabilistic-approach
//
//   Progressive Photon Mapping
//   Toshiya Hachisuka, Shinji Ogaki, Henrik Wann Jensen
//   http://cs.au.dk/~toshiya/ppm.pdf
//
//   Stochastic Progressive Photon Mapping
//   Toshiya Hachisuka, Henrik Wann Jensen
//   http://cs.au.dk/~toshiya/sppm.pdf
//

SPPMPassCallback::SPPMPassCallback(
    const Scene&                    scene,
    const LightSampler&             light_sampler,
    const TraceContext&             trace_context,
    TextureStore&                   texture_store,
    const ParamArray&               params)
  : m_params(params)
  , m_scene(scene)
  , m_light_sampler(light_sampler)
  , m_trace_context(trace_context)
  , m_texture_store(texture_store)
  , m_pass_number(0)
  , m_emitted_photon_count(0)
{
}

void SPPMPassCallback::release()
{
    delete this;
}

void SPPMPassCallback::pre_render(const Frame& frame)
{
    RENDERER_LOG_INFO("sppm pass %s...", pretty_uint(m_pass_number).c_str());

    // Create pixel statistics if they don't exist yet.
    if (m_pixel_statistics.empty())
    {
        RENDERER_LOG_INFO("initializing sppm pixel statistics...");

        const CanvasProperties& props = frame.image().properties();

        m_frame_bbox.min.x = 0;
        m_frame_bbox.min.y = 0;
        m_frame_bbox.max.x = static_cast<int>(props.m_canvas_width) - 1;
        m_frame_bbox.max.y = static_cast<int>(props.m_canvas_height) - 1;

        m_frame_width = props.m_canvas_width;

        SPPMPixelStatistics initial;
        initial.m_radius = m_params.m_initial_radius;
        initial.m_tau.set(0.0f);

        m_pixel_statistics.resize(props.m_pixel_count, initial);
    }

    // Generate a new hash at each pass.
    const size_t pass_hash = hashint32(m_pass_number);

    // Create a new set of gather points.
    SPPMGatherPointVector gather_points;
    create_gather_points(
        gather_points,
        pass_hash,
        frame);

    // Create a new set of photons.
    SPPMPhotonVector photons;
    SPPMPhotonTracer photon_tracer(
        m_light_sampler,
        m_trace_context,
        m_texture_store);
    photon_tracer.trace_photons(
        photons,
        pass_hash,
        m_params.m_photon_count_per_pass);
    m_emitted_photon_count += m_params.m_photon_count_per_pass;

    // Build a photon map.
    const SPPMPhotonMap photon_map(photons);

    // Update pixel statistics.
    update_pixel_statistics(gather_points, photons, photon_map);

    ++m_pass_number;
}

void SPPMPassCallback::create_gather_points(
    SPPMGatherPointVector&          gather_points,
    const size_t                    pass_hash,
    const Frame&                    frame)
{
    RENDERER_LOG_INFO("creating sppm gather points...");

    auto_ptr<IPixelRendererFactory> pixel_renderer_factory(
        new SPPMGatherPointTracerFactory(
            m_scene,
            m_trace_context,
            m_texture_store,
            pass_hash,
            gather_points));

    auto_ptr<ITileRendererFactory> tile_renderer_factory(
        new GenericTileRendererFactory(
            frame,
            pixel_renderer_factory.get(),
            ParamArray()));

    auto_ptr<IFrameRenderer> frame_renderer(
        GenericFrameRendererFactory::create(
            frame,
            tile_renderer_factory.get(),
            0,                                              // no tile callback factory
            0,                                              // no pass callback
            GenericFrameRendererFactory::SinglePass,
            ParamArray()));

    frame_renderer->render();
}

void SPPMPassCallback::update_pixel_statistics(
    const SPPMGatherPointVector&    gather_points,
    const SPPMPhotonVector&         photons,
    const SPPMPhotonMap&            photon_map)
{
    assert(!m_pixel_statistics.empty());

    const size_t point_count = gather_points.size();

    RENDERER_LOG_INFO(
        "updating sppm pixel statistics from %s %s...",
        pretty_uint(point_count).c_str(),
        point_count > 1 ? "gather points" : "gather point");

    TextureCache texture_cache(m_texture_store);

    knn::Answer<float> answer(m_params.m_photon_count_per_estimate);
    knn::Query3f query(photon_map, answer);

    // Loop over the gather points.
    for (size_t i = 0; i < point_count; ++i)
    {
        const SPPMGatherPoint& gather_point = gather_points[i];
        SPPMPixelStatistics& stats = m_pixel_statistics[gather_point.m_pixel_index];
        Spectrum tau(0.0f);

        // Gather photons within the search radius.
        query.run(gather_point.m_position, stats.m_radius);
        const size_t photon_count = answer.size();
        if (photon_count == 0)
            continue;

        // Loop over the photons within the search radius.
        for (size_t j = 0; j < photon_count; ++j)
        {
            // Retrieve the j'th photon.
            const knn::Answer<float>::Entry& photon = answer.get(j);
            const SPPMPhotonData& data = photons.m_data[photon.m_index];

#if 0
            // Simple check to reduce the bias intrinsic to photon mapping.
            if (dot(Vector3d(data.m_geometric_normal), gather_point.m_geometric_normal) <= -0.1)
                continue;
#endif

            // Evaluate the BSDF's inputs.
            InputEvaluator input_evaluator(texture_cache);
            gather_point.m_bsdf->evaluate_inputs(input_evaluator, gather_point.m_uv);

            // Evaluate the BSDF for this photon.
            assert(gather_point.m_bsdf);
            Spectrum bsdf_value;
            const double bsdf_prob =
                gather_point.m_bsdf->evaluate(
                    input_evaluator.data(),
                    true,                                       // adjoint
                    true,                                       // multiply by |cos(incoming, normal)|
                    gather_point.m_geometric_normal,
                    gather_point.m_shading_basis,
                    gather_point.m_incoming,                    // toward the camera
                    normalize(Vector3d(data.m_incoming)),       // toward the light
                    BSDF::AllScatteringModes,
                    bsdf_value);
            if (bsdf_prob == 0.0)
                continue;

            // Accumulate reflected flux.
            bsdf_value *= data.m_flux;
            tau += bsdf_value;
        }

        // Density estimation.
        tau /= static_cast<float>(Pi * stats.m_radius * stats.m_radius);

        // Apply gather point throughput.
        tau *= gather_point.m_throughput;

        // Update shared statistics.
        const float ratio = (m_pass_number + m_params.m_alpha) / (m_pass_number + 1.0f);
        assert(ratio <= 1.0);
        stats.m_radius *= sqrt(ratio);
        stats.m_tau += tau;
        stats.m_radiance = stats.m_tau / static_cast<float>(m_emitted_photon_count);
    }
}


//
// SPPMPassCallback::Parameters class implementation.
//

SPPMPassCallback::Parameters::Parameters(const ParamArray& params)
  : m_initial_radius(params.get_required<float>("initial_radius", 0.1f))
  , m_alpha(params.get_optional<float>("alpha", 0.7f))
  , m_photon_count_per_pass(params.get_optional<size_t>("photons_per_pass", 100000))
  , m_photon_count_per_estimate(params.get_optional<size_t>("photons_per_estimate", 100))
{
}

}   // namespace renderer
