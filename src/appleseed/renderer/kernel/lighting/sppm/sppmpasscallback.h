
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

#pragma once

// appleseed.renderer headers.
#include "renderer/kernel/lighting/sppm/sppmparameters.h"
#include "renderer/kernel/lighting/sppm/sppmphoton.h"
#include "renderer/kernel/lighting/sppm/sppmphotonmap.h"
#include "renderer/kernel/lighting/sppm/sppmphotontracer.h"
#include "renderer/kernel/rendering/ipasscallback.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/timers.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <cstddef>
#include <memory>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class JobQueue; }
namespace renderer      { class Frame; }
namespace renderer      { class ForwardLightSampler; }
namespace renderer      { class OIIOTextureSystem; }
namespace renderer      { class OSLShadingSystem; }
namespace renderer      { class Scene; }
namespace renderer      { class TextureStore; }
namespace renderer      { class TraceContext; }

namespace renderer
{

//
// This class is responsible for building a new photon map before a pass begins.
//

class SPPMPassCallback
  : public IPassCallback
{
  public:
    // Constructor.
    SPPMPassCallback(
        const Scene&                    scene,
        const ForwardLightSampler&      light_sampler,
        const TraceContext&             trace_context,
        TextureStore&                   texture_store,
        OIIOTextureSystem&              oiio_texture_system,
        OSLShadingSystem&               shading_system,
        const SPPMParameters&           params);

    // Delete this instance.
    void release() override;

    // This method is called at the beginning of a pass.
    void on_pass_begin(
        const Frame&                    frame,
        foundation::JobQueue&           job_queue,
        foundation::IAbortSwitch&       abort_switch) override;

    // This method is called at the end of a pass.
    void on_pass_end(
        const Frame&                    frame,
        foundation::JobQueue&           job_queue,
        foundation::IAbortSwitch&       abort_switch) override;

    // Return the i'th photon.
    const SPPMMonoPhoton& get_mono_photon(const size_t i) const;
    const SPPMPolyPhoton& get_poly_photon(const size_t i) const;

    // Return the current photon map.
    const SPPMPhotonMap& get_photon_map() const;

    // Return the current lookup radius.
    float get_lookup_radius() const;

  private:
    const SPPMParameters                m_params;
    SPPMPhotonTracer                    m_photon_tracer;
    size_t                              m_pass_number;
    SPPMPhotonVector                    m_photons;
    std::unique_ptr<SPPMPhotonMap>      m_photon_map;
    float                               m_initial_lookup_radius;
    float                               m_lookup_radius;
    foundation::Stopwatch<foundation::DefaultWallclockTimer>
                                        m_stopwatch;
};


//
// SPPMPassCallback class implementation.
//

inline const SPPMMonoPhoton& SPPMPassCallback::get_mono_photon(const size_t i) const
{
    return m_photons.m_mono_photons[i];
}

inline const SPPMPolyPhoton& SPPMPassCallback::get_poly_photon(const size_t i) const
{
    return m_photons.m_poly_photons[i];
}

inline const SPPMPhotonMap& SPPMPassCallback::get_photon_map() const
{
    return *m_photon_map.get();
}

inline float SPPMPassCallback::get_lookup_radius() const
{
    return m_lookup_radius;
}

}   // namespace renderer
