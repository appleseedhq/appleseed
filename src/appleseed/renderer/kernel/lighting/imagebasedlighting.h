
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
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/math/dual.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class BSDF; }
namespace renderer  { class DirectShadingComponents; }
namespace renderer  { class EnvironmentEDF; }
namespace renderer  { class IMaterialSampler; }
namespace renderer  { class LightPathStream; }
namespace renderer  { class ShadingContext; }
namespace renderer  { class ShadingPoint; }

namespace renderer
{

//
// Compute image-based lighting at a given point in space.
//

// Compute outgoing radiance due to image-based lighting via combined BSDF and environment sampling.
void compute_ibl_combined_sampling(
    SamplingContext&                sampling_context,
    const ShadingContext&           shading_context,
    const EnvironmentEDF&           environment_edf,
    const foundation::Dual3d&       outgoing,               // world space outgoing direction, unit-length
    const IMaterialSampler&         material_sampler,
    const int                       env_sampling_modes,     // permitted scattering modes during environment sampling
    const size_t                    material_sample_count,  // number of samples in BSDF sampling
    const size_t                    env_sample_count,       // number of samples in environment sampling
    DirectShadingComponents&        radiance,
    Spectrum&                       unshaded_radiance,
    Spectrum&                       shaded_radiance,
    LightPathStream*                light_path_stream);

// Compute outgoing radiance due to image-based lighting via BSDF sampling only.
void compute_ibl_material_sampling(
    SamplingContext&                sampling_context,
    const ShadingContext&           shading_context,
    const EnvironmentEDF&           environment_edf,
    const foundation::Dual3d&       outgoing,               // world space outgoing direction, unit-length
    const IMaterialSampler&         material_sampler,
    const size_t                    material_sample_count,  // number of samples in BSDF sampling
    const size_t                    env_sample_count,       // number of samples in environment sampling
    DirectShadingComponents&        radiance);

// Compute outgoing radiance due to image-based lighting via environment sampling only.
void compute_ibl_environment_sampling(
    SamplingContext&                sampling_context,
    const ShadingContext&           shading_context,
    const EnvironmentEDF&           environment_edf,
    const foundation::Dual3d&       outgoing,               // world space outgoing direction, unit-length
    const IMaterialSampler&         material_sampler,
    const int                       env_sampling_modes,     // permitted scattering modes during environment sampling
    const size_t                    material_sample_count,
    const size_t                    env_sample_count,       // number of samples in environment sampling
    DirectShadingComponents&        radiance,
    Spectrum&                       unshaded_radiance,
    Spectrum&                       shaded_radiance,
    LightPathStream*                light_path_stream);

}   // namespace renderer
