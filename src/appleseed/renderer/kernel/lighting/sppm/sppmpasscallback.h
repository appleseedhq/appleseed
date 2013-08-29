
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
#include "renderer/kernel/lighting/sppm/sppmpixelstatistics.h"
#include "renderer/kernel/rendering/ipasscallback.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

// Forward declarations.
namespace renderer  { class Frame; }
namespace renderer  { class LightSampler; }
namespace renderer  { class Scene; }
namespace renderer  { class SPPMGatherPointVector; }
namespace renderer  { class SPPMPhotonMap; }
namespace renderer  { class SPPMPhotonVector; }
namespace renderer  { class TextureStore; }
namespace renderer  { class TraceContext; }

namespace renderer
{

//
// This class is the backbone of the SPPM implementation.
//

class SPPMPassCallback
  : public IPassCallback
{
  public:
    // Constructor.
    SPPMPassCallback(
        const Scene&                    scene,
        const LightSampler&             light_sampler,
        const TraceContext&             trace_context,
        TextureStore&                   texture_store,
        const ParamArray&               params);

    // Delete this instance.
    virtual void release() OVERRIDE;

    // This method is called at the beginning of a pass.
    virtual void pre_render(const Frame& frame) OVERRIDE;

    // Return radiance for a given pixel.
    void get_pixel_radiance(
        const foundation::Vector2i&     p,
        Spectrum&                       radiance) const;

  private:
    struct Parameters
    {
        float   m_initial_radius;
        float   m_alpha;
        size_t  m_photon_count_per_pass;
        size_t  m_photon_count_per_estimate;

        explicit Parameters(const ParamArray& params);
    };

    const Parameters                    m_params;
    const Scene&                        m_scene;
    const LightSampler&                 m_light_sampler;
    const TraceContext&                 m_trace_context;
    TextureStore&                       m_texture_store;
    foundation::uint32                  m_pass_number;
    foundation::AABB2i                  m_frame_bbox;
    size_t                              m_frame_width;
    std::vector<SPPMPixelStatistics>    m_pixel_statistics;
    size_t                              m_emitted_photon_count;

    void create_gather_points(
        SPPMGatherPointVector&          gather_points,
        const size_t                    pass_hash,
        const Frame&                    frame);

    void update_pixel_statistics(
        const SPPMGatherPointVector&    gather_points,
        const SPPMPhotonVector&         photons,
        const SPPMPhotonMap&            photon_map);
};


//
// SPPMPassCallback class implementation.
//

inline void SPPMPassCallback::get_pixel_radiance(
    const foundation::Vector2i&         p,
    Spectrum&                           radiance) const
{
    if (m_frame_bbox.contains(p))
        radiance = m_pixel_statistics[p.y * m_frame_width + p.x].m_radiance;
    else radiance.set(0.0f);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPASSCALLBACK_H
