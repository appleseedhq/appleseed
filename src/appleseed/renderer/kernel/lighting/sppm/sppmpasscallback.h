
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/sppm/sppmphoton.h"
#include "renderer/kernel/rendering/ipasscallback.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <memory>

// Forward declarations.
namespace renderer  { class LightSample; }
namespace renderer  { class LightSampler; }
namespace renderer  { class PathVertex; }
namespace renderer  { class ShadingPoint; }
namespace renderer  { class SPPMPhotonMap; }
namespace renderer  { class TextureStore; }
namespace renderer  { class TraceContext; }

namespace renderer
{

class SPPMPassCallback
  : public IPassCallback
{
  public:
    // Constructor.
    SPPMPassCallback(
        const LightSampler&         light_sampler,
        const TraceContext&         trace_context,
        TextureStore&               texture_store,
        const ParamArray&           params);

    // Delete this instance.
    virtual void release() OVERRIDE;

    // This method is called at the beginning of a pass.
    virtual void pre_render() OVERRIDE;

    // Return the SPPM photon map stored in this object.
    const SPPMPhotonMap& get_photon_map() const;

  private:
    const ParamArray                m_params;
    const LightSampler&             m_light_sampler;
    TextureCache                    m_texture_cache;
    Intersector                     m_intersector;
    std::auto_ptr<SPPMPhotonMap>    m_photon_map;
    foundation::uint32              m_pass_number;

    void trace_photons(
        PhotonVector&               photons);

    void trace_photon(
        SamplingContext&            sampling_context,
        const float                 flux_multiplier,
        PhotonVector&               photons);

    void trace_emitting_triangle_photon(
        SamplingContext&            sampling_context,
        LightSample&                light_sample,
        const float                 flux_multiplier,
        PhotonVector&               photons);

    void trace_non_physical_light_photon(
        SamplingContext&            sampling_context,
        LightSample&                light_sample,
        const float                 flux_multiplier,
        PhotonVector&               photons);

    struct PathVisitor
    {
        const Spectrum  m_initial_alpha;    // initial particle flux (in W)
        PhotonVector&   m_photons;

        PathVisitor(
            const ParamArray&           params,
            const Spectrum&             initial_alpha,
            PhotonVector&               photons);

        bool accept_scattering_mode(
            const BSDF::Mode            prev_bsdf_mode,
            const BSDF::Mode            bsdf_mode) const;

        bool visit_vertex(const PathVertex& vertex);

        void visit_environment(
            const ShadingPoint&         shading_point,
            const foundation::Vector3d& outgoing,
            const BSDF::Mode            prev_bsdf_mode,
            const double                prev_bsdf_prob,
            const Spectrum&             throughput);
    };
};


//
// SPPMPassCallback class implementation.
//

inline const SPPMPhotonMap& SPPMPassCallback::get_photon_map() const
{
    return *m_photon_map.get();
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPASSCALLBACK_H
