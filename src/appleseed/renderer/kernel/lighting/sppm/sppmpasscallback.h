
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPASSCALLBACK_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPASSCALLBACK_H

// appleseed.renderer headers.
#include "renderer/kernel/lighting/sppm/sppmphoton.h"
#include "renderer/kernel/lighting/sppm/sppmphotonmap.h"
#include "renderer/kernel/lighting/sppm/sppmphotontracer.h"
#include "renderer/kernel/rendering/ipasscallback.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/timer.h"
#include "foundation/platform/types.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <cstddef>
#include <memory>

// Forward declarations.
namespace foundation    { class AbortSwitch; }
namespace foundation    { class JobQueue; }
namespace renderer      { class Frame; }
namespace renderer      { class LightSampler; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Scene; }
namespace renderer      { class SPPMPhotonData; }
namespace renderer      { class TextureStore; }
namespace renderer      { class TraceContext; }

namespace renderer
{

//
// This class is responsible for generating a new photon the backbone of the SPPM implementation.
//

class SPPMPassCallback
  : public IPassCallback
{
  public:
    // Constructor.
    SPPMPassCallback(
        const Scene&                scene,
        const LightSampler&         light_sampler,
        const TraceContext&         trace_context,
        TextureStore&               texture_store,
        const ParamArray&           params);

    // Delete this instance.
    virtual void release() OVERRIDE;

    // This method is called at the beginning of a pass.
    virtual void pre_render(
        const Frame&                frame,
        foundation::JobQueue&       job_queue,
        foundation::AbortSwitch&    abort_switch) OVERRIDE;

    // This method is called at the end of a pass.
    virtual void post_render(
        const Frame&                frame,
        foundation::JobQueue&       job_queue,
        foundation::AbortSwitch&    abort_switch) OVERRIDE;

    // Return the number of photons emitted for this pass.
    size_t get_emitted_photon_count() const;

    // Return the i'th photon.
    const SPPMPhotonData& get_photon_data(const size_t i) const;

    // Return the current photon map.
    const SPPMPhotonMap& get_photon_map() const;

    // Return the current lookup radius.
    float get_lookup_radius() const;

  private:
    struct Parameters
    {
        const float m_initial_radius_percents;      // initial lookup radius as a percentage of the scene diameter
        const float m_alpha;                        // radius shrinking control

        explicit Parameters(const ParamArray& params);
    };

    const Parameters                m_params;
    SPPMPhotonTracer                m_photon_tracer;
    foundation::uint32              m_pass_number;
    size_t                          m_emitted_photon_count;
    SPPMPhotonVector                m_photons;
    std::auto_ptr<SPPMPhotonMap>    m_photon_map;
    float                           m_lookup_radius;
    foundation::Stopwatch<foundation::DefaultWallclockTimer>
                                    m_stopwatch;
};


//
// SPPMPassCallback class implementation.
//

inline size_t SPPMPassCallback::get_emitted_photon_count() const
{
    return m_emitted_photon_count;
}

inline const SPPMPhotonData& SPPMPassCallback::get_photon_data(const size_t i) const
{
    return m_photons.m_data[i];
}

inline const SPPMPhotonMap& SPPMPassCallback::get_photon_map() const
{
    return *m_photon_map.get();
}

inline float SPPMPassCallback::get_lookup_radius() const
{
    return m_lookup_radius;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPASSCALLBACK_H
