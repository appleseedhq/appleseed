
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPARAMETERS_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPARAMETERS_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class ParamArray; }

namespace renderer
{

struct SPPMParameters
{
    enum PhotonType { Monochromatic, Polychromatic };
    enum Mode { RayTraced, SPPM, Off };

    const SamplingContext::Mode m_sampling_mode;
    const PhotonType            m_photon_type;

    const Mode                  m_dl_mode;                              // direct lighting mode
    const bool                  m_enable_ibl;                           // is image-based lighting enabled?
    const bool                  m_enable_caustics;                      // are caustics enabled?

    const size_t                m_light_photon_count;                   // number of photons emitted from the lights
    const size_t                m_env_photon_count;                     // number of photons emitted from the environment
    const size_t                m_photon_packet_size;                   // number of photons per tracing job

    const size_t                m_photon_tracing_max_path_length;       // maximum photon tracing path length, ~0 for unlimited
    const size_t                m_photon_tracing_rr_min_path_length;    // minimum photon tracing path length before Russian Roulette kicks in, ~0 for unlimited

    const size_t                m_path_tracing_max_path_length;         // maximum path tracing path length, ~0 for unlimited
    const size_t                m_path_tracing_rr_min_path_length;      // minimum path tracing path length before Russian Roulette kicks in, ~0 for unlimited

    const float                 m_transparency_threshold;
    const size_t                m_max_iterations;                       // maximum number of iteration during path tracing

    const float                 m_initial_radius_percents;              // initial lookup radius as a percentage of the scene diameter
    const float                 m_alpha;                                // radius shrinking control
    const size_t                m_max_photons_per_estimate;             // maximum number of photons per density estimation
    const float                 m_dl_light_sample_count;                // number of light samples used to estimate direct illumination in ray traced mode
    const float                 m_dl_low_light_threshold;               // light contribution threshold to disable shadow rays
    float                       m_rcp_dl_light_sample_count;

    const bool                  m_view_photons;                         // debug mode to visualize the photons
    const float                 m_view_photons_radius;                  // lookup radius when visualizing photons

    explicit SPPMParameters(const ParamArray& params);

    void print() const;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPARAMETERS_H
