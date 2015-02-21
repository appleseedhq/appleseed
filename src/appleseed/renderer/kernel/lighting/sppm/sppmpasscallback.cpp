
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/math/hash.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// SPPMPassCallback class implementation.
//

SPPMPassCallback::SPPMPassCallback(
    const Scene&            scene,
    const LightSampler&     light_sampler,
    const TraceContext&     trace_context,
    TextureStore&           texture_store,
#ifdef APPLESEED_WITH_OIIO
    OIIO::TextureSystem&    oiio_texture_system,
#endif
#ifdef APPLESEED_WITH_OSL
    OSL::ShadingSystem&     shading_system,
#endif
    const SPPMParameters&   params)
  : m_params(params)
  , m_photon_tracer(
        scene,
        light_sampler,
        trace_context,
        texture_store,
#ifdef APPLESEED_WITH_OIIO
        oiio_texture_system,
#endif
#ifdef APPLESEED_WITH_OSL
        shading_system,
#endif
        params)
  , m_pass_number(0)
{
    // Compute the initial lookup radius.
    const GAABB3 scene_bbox = scene.compute_bbox();
    const float scene_diameter =
        scene_bbox.is_valid()
            ? static_cast<float>(scene_bbox.diameter())
            : 0.0f;
    const float diameter_factor = m_params.m_initial_radius_percents / 100.0f;
    m_initial_lookup_radius = m_lookup_radius = scene_diameter * diameter_factor;
}

void SPPMPassCallback::release()
{
    delete this;
}

void SPPMPassCallback::pre_render(
    const Frame&            frame,
    JobQueue&               job_queue,
    IAbortSwitch&           abort_switch)
{
    if (m_initial_lookup_radius > 0.0f)
    {
        RENDERER_LOG_INFO(
            "sppm lookup radius is %f (%s of initial radius).",
            m_lookup_radius,
            pretty_percent(m_lookup_radius, m_initial_lookup_radius, 3).c_str());
    }

    m_stopwatch.start();

    // Create a new set of photons.
    m_photons.clear_keep_memory();
    m_photon_tracer.trace_photons(
        m_photons,
        hash_uint32(m_pass_number),
        job_queue,
        abort_switch);

    // Stop there if rendering was aborted.
    if (abort_switch.is_aborted())
        return;

    // Build a new photon map.
    m_photon_map.reset(new SPPMPhotonMap(m_photons));
}

void SPPMPassCallback::post_render(
    const Frame&            frame,
    JobQueue&               job_queue,
    IAbortSwitch&           abort_switch)
{
    // Shrink the lookup radius for the next pass.
    const float k = (m_pass_number + m_params.m_alpha) / (m_pass_number + 1);
    assert(k <= 1.0);
    m_lookup_radius *= sqrt(k);

    m_stopwatch.measure();

    RENDERER_LOG_INFO(
        "sppm pass %s completed in %s.",
        pretty_uint(m_pass_number + 1).c_str(),
        pretty_time(m_stopwatch.get_seconds()).c_str());

    ++m_pass_number;
}

}   // namespace renderer
