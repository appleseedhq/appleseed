
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Stephen Agyemang, The appleseedhq Organization
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

// Standard headers.
#include <cstddef>
#include <string>

// Forward declarations.
namespace renderer
{
class ParamArray;
}

namespace renderer
{
enum class SpatialFilter
{
	Nearest,
	Stochastic,
	Box
};

enum class DirectionalFilter
{
	Nearest,
	Box
};

enum class IterationProgression
{
	Combine,
	Automatic
};

enum class BSDFSamplingFractionMode
{
	Learn,
	Fixed
};

enum class GuidedBounceMode
{
	Learn,
	StrictlyDiffuse,
	StrictlyGlossy,
	PreferDiffuse,
	PreferGlossy
};

enum class SaveMode
{
	None,
	All,
	Final
};

struct GPTParameters
{
	explicit GPTParameters(const ParamArray& params);

	void print() const;

	const size_t                      m_samples_per_pass;
	const IterationProgression        m_iteration_progression;
	const SpatialFilter               m_spatial_filter;
	const DirectionalFilter           m_directional_filter;
	const BSDFSamplingFractionMode    m_bsdf_sampling_fraction_mode;
	const GuidedBounceMode			  m_guided_bounce_mode;
	const SaveMode					  m_save_mode;
	const std::string				  m_save_path;
	const float                       m_fixed_bsdf_sampling_fraction;
	const float                       m_learning_rate;


	const bool                        m_enable_dl;                    // is direct lighting enabled?
	const bool                        m_enable_ibl;                   // is image-based lighting enabled?
	const bool                        m_enable_caustics;              // are caustics enabled?

	const size_t                      m_max_bounces;                  // maximum number of bounces, ~0 for unlimited
	const size_t                      m_max_guided_bounces;           // maximum number of path guided bounces, ~0 for unlimited
	const size_t                      m_max_diffuse_bounces;          // maximum number of diffuse bounces, ~0 for unlimited
	const size_t                      m_max_glossy_bounces;           // maximum number of glossy bounces, ~0 for unlimited
	const size_t                      m_max_specular_bounces;         // maximum number of specular bounces, ~0 for unlimited
	const size_t                      m_max_volume_bounces;           // maximum number of volume scattering events, ~0 for unlimited

	const bool                        m_clamp_roughness;

	const size_t                      m_rr_min_path_length;           // minimum path length before Russian Roulette kicks in, ~0 for unlimited
	const bool                        m_next_event_estimation;        // use next event estimation?

	const float                       m_dl_light_sample_count;        // number of light samples used to estimate direct illumination
	const float                       m_dl_low_light_threshold;       // light contribution threshold to disable shadow rays
	const float                       m_ibl_env_sample_count;         // number of environment samples used to estimate IBL
	float                             m_rcp_dl_light_sample_count;
	float                             m_rcp_ibl_env_sample_count;

	const bool                        m_has_max_ray_intensity;
	const float                       m_max_ray_intensity;

	const size_t                      m_distance_sample_count;        // number of distance samples for volume rendering
	const bool                        m_enable_equiangular_sampling;  // optimize for lights that are located outside volumes

	const bool                        m_record_light_paths;

	
	static size_t fixup_bounces(const int x)
	{
		return x == -1 ? ~size_t(0) : x;
	}

	static size_t fixup_path_length(const size_t x)
	{
		return x == 0 ? ~size_t(0) : x;
	}
};

} 	// namespace renderer
