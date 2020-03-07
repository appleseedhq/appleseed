
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
#include "sppmpasscallback.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/rendering/ishadingresultframebufferfactory.h"
#include "renderer/kernel/shading/oslshadingsystem.h"
#include "renderer/kernel/texturing/oiiotexturesystem.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/hash/hash.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/memory/memory.h"
#include "foundation/string/string.h"
#include "foundation/utility/job/iabortswitch.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstdint>
#include <string>

using namespace foundation;

namespace renderer
{

//
// SPPMPassCallback class implementation.
//

SPPMPassCallback::SPPMPassCallback(
    const Scene&                        scene,
    const ForwardLightSampler&          light_sampler,
    const TraceContext&                 trace_context,
    TextureStore&                       texture_store,
    OIIOTextureSystem&                  oiio_texture_system,
    OSLShadingSystem&                   shading_system,
    IShadingResultFrameBufferFactory&   shading_result_framebuffer_factory,
    const SPPMParameters&               params)
  : m_params(params)
  , m_photon_tracer(
        scene,
        light_sampler,
        trace_context,
        texture_store,
        oiio_texture_system,
        shading_system,
        params)
  , m_shading_result_framebuffer_factory(shading_result_framebuffer_factory)
  , m_pass_number(0)
{
    // Compute lookup radii.
    const GAABB3 scene_bbox = scene.compute_bbox();
    const float scene_diameter = static_cast<float>(scene_bbox.diameter());
    m_initial_photon_lookup_radius = scene_diameter * (m_params.m_initial_photon_lookup_radius_percents / 100.0f);
    m_importon_lookup_radius = scene_diameter * (m_params.m_importon_lookup_radius_percents / 100.0f);

    // Start with the initial photon lookup radius.
    m_photon_lookup_radius = m_initial_photon_lookup_radius;
}

void SPPMPassCallback::release()
{
    delete this;
}

void SPPMPassCallback::on_pass_begin(
    const Frame&                        frame,
    JobQueue&                           job_queue,
    IAbortSwitch&                       abort_switch)
{
    m_stopwatch.start();

    if (m_params.m_enable_importons)
    {
        if (m_pass_number == 0)
            RENDERER_LOG_INFO("pass #" FMT_SIZE_T " is a pure importon tracing pass.", m_pass_number + 1);
        else RENDERER_LOG_INFO("pass #" FMT_SIZE_T " is a combined rendering + importon tracing pass.", m_pass_number + 1);

        // If importons are enabled, the first pass was a pure importon tracing pass and the framebuffers
        // are black but with a weight of 1. Clear the framebuffers before starting rendering for real.
        if (m_pass_number == 1)
            m_shading_result_framebuffer_factory.clear();

        // Reset working sets.
        for (std::unique_ptr<SPPMLightingEngineWorkingSet>& working_set : m_working_sets)
        {
            // Clear importon vector.
            clear_keep_memory(working_set->m_importons);

            // Create importon mask if necessary.
            if (!working_set->m_importon_mask)
            {
                const CanvasProperties& props = frame.image().properties();
                working_set->m_importon_mask.reset(new BitMask2(props.m_canvas_width, props.m_canvas_height));
            }

            // Clear importon mask.
            working_set->m_importon_mask->clear();
        }
    }

    // Don't trace photons in the first pass if importons are enabled.
    if (!m_params.m_enable_importons || m_pass_number > 0)
    {
        // Clear photons.
        m_photons.clear_keep_memory();

        // Create a new set of photons.
        const std::size_t effective_pass_number =
            m_params.m_enable_importons ? m_pass_number - 1 : m_pass_number;
        const std::uint32_t pass_hash = mix_uint32(frame.get_noise_seed(), static_cast<std::uint32_t>(effective_pass_number));
        m_photon_tracer.trace_photons(
            m_photons,
            m_params.m_enable_importons && m_pass_number > 0 ? m_importon_map.get() : nullptr,
            m_importon_lookup_radius,
            pass_hash,
            job_queue,
            abort_switch);

        // Stop there if rendering was aborted.
        if (abort_switch.is_aborted())
            return;

        // Build a new photon map.
        m_photon_map.reset(new SPPMPhotonMap(m_photons));

        if (m_initial_photon_lookup_radius > 0.0f)
        {
            RENDERER_LOG_INFO(
                "sppm lookup radius is %f (%s of initial radius).",
                m_photon_lookup_radius,
                pretty_percent(m_photon_lookup_radius, m_initial_photon_lookup_radius, 3).c_str());
        }
    }
}

void SPPMPassCallback::on_pass_end(
    const Frame&                        frame,
    JobQueue&                           job_queue,
    IAbortSwitch&                       abort_switch)
{
    // Don't prepare for the next pass on the last pass.
    if (m_pass_number < m_params.m_pass_count - 1)
    {
        // Don't shrink lookup radius after the first pass if importons are enabled.
        if (!m_params.m_enable_importons || m_pass_number > 0)
        {
            // Shrink the lookup radius for the next pass.
            const std::size_t effective_pass_number =
                m_params.m_enable_importons ? m_pass_number - 1 : m_pass_number;
            const float k = (effective_pass_number + m_params.m_alpha) / (effective_pass_number + 1);
            assert(k <= 1.0);
            m_photon_lookup_radius *= std::sqrt(k);
        }

        // Merge importon vectors and build importon map.
        if (m_params.m_enable_importons)
        {
            // Count the total number of created importons.
            std::size_t importon_count = 0;
            for (std::unique_ptr<SPPMLightingEngineWorkingSet>& working_set : m_working_sets)
                importon_count += working_set->m_importons.size();

            // Allocate the cumulated importon vector.
            SPPMImportonVector importons;
            importons.reserve(importon_count);

            // Copy importons into cumulated importon vector.
            for (std::unique_ptr<SPPMLightingEngineWorkingSet>& working_set : m_working_sets)
                importons.insert(std::end(importons), std::begin(working_set->m_importons), std::end(working_set->m_importons));

            assert(importons.size() == importon_count);

            RENDERER_LOG_DEBUG(
                "%s importon%s created (%s).",
                pretty_uint(importons.size()).c_str(),
                importons.size() > 1 ? "s" : "",
                pretty_size(importons.capacity() * sizeof(SPPMImportonVector::value_type)).c_str());

            // Build a new importon map.
            m_importon_map.reset(new SPPMImportonMap(importons));
        }
    }

    m_stopwatch.measure();

    RENDERER_LOG_INFO(
        "sppm pass %s completed in %s.",
        pretty_uint(m_pass_number + 1).c_str(),
        pretty_time(m_stopwatch.get_seconds()).c_str());

    ++m_pass_number;
}

SPPMLightingEngineWorkingSet& SPPMPassCallback::acquire_working_set()
{
    m_working_sets.emplace_back(new SPPMLightingEngineWorkingSet());
    return *m_working_sets.back();
}

}   // namespace renderer
