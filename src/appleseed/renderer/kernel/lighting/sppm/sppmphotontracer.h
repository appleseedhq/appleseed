
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPHOTONTRACER_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPHOTONTRACER_H

// appleseed.renderer headers.
#include "renderer/kernel/lighting/sppm/sppmparameters.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// OSL headers.
#ifdef APPLESEED_WITH_OSL
#include "foundation/platform/oslheaderguards.h"
BEGIN_OSL_INCLUDES
#include "OSL/oslexec.h"
END_OSL_INCLUDES
#endif

// OpenImageIO headers.
#ifdef APPLESEED_WITH_OIIO
#include "foundation/platform/oiioheaderguards.h"
BEGIN_OIIO_INCLUDES
#include "OpenImageIO/texture.h"
END_OIIO_INCLUDES
#endif

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class JobQueue; }
namespace renderer      { class LightSampler; }
namespace renderer      { class LightTargetArray; }
namespace renderer      { class Scene; }
namespace renderer      { class SPPMPhotonVector; }
namespace renderer      { class TextureStore; }
namespace renderer      { class TraceContext; }

namespace renderer
{

class SPPMPhotonTracer
  : public foundation::NonCopyable
{
  public:
    SPPMPhotonTracer(
        const Scene&                scene,
        const LightSampler&         light_sampler,
        const TraceContext&         trace_context,
        TextureStore&               texture_store,
#ifdef APPLESEED_WITH_OIIO
        OIIO::TextureSystem&        oiio_texture_system,
#endif
#ifdef APPLESEED_WITH_OSL
        OSL::ShadingSystem&         shading_system,
#endif
        const SPPMParameters&       params);

    void trace_photons(
        SPPMPhotonVector&           photons,
        const size_t                pass_hash,
        foundation::JobQueue&       job_queue,
        foundation::IAbortSwitch&   abort_switch);

  private:
    const SPPMParameters            m_params;
    const Scene&                    m_scene;
    const LightSampler&             m_light_sampler;
    const TraceContext&             m_trace_context;
    TextureStore&                   m_texture_store;
    size_t                          m_total_emitted_photon_count;
    size_t                          m_total_stored_photon_count;
#ifdef APPLESEED_WITH_OIIO
    OIIO::TextureSystem&            m_oiio_texture_system;
#endif
#ifdef APPLESEED_WITH_OSL
    OSL::ShadingSystem&             m_shading_system;
#endif

    void schedule_light_photon_tracing_jobs(
        const LightTargetArray&     photon_targets,
        SPPMPhotonVector&           photons,
        const size_t                pass_hash,
        foundation::JobQueue&       job_queue,
        size_t&                     job_count,
        size_t&                     emitted_photon_count,
        foundation::IAbortSwitch&   abort_switch);

    void schedule_environment_photon_tracing_jobs(
        const LightTargetArray&     photon_targets,
        SPPMPhotonVector&           photons,
        const size_t                pass_hash,
        foundation::JobQueue&       job_queue,
        size_t&                     job_count,
        size_t&                     emitted_photon_count,
        foundation::IAbortSwitch&   abort_switch);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPHOTONTRACER_H
