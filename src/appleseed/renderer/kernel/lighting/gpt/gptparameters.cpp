
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

// Interface header.
#include "gptparameters.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/settingsparsing.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{
SpatialFilter get_spatial_filter(const ParamArray& params)
{
    const std::string name = params.get_required<std::string>("spatial_filter", "stochastic");

    if (name == "nearest")
    {
      return SpatialFilter::Nearest;
    }
    else if (name == "box")
    {
      return SpatialFilter::Box;
    }
    else if (name == "stochastic")
    {
      return SpatialFilter::Stochastic;
    }
    else
    {
      RENDERER_LOG_WARNING("Unknown parameter for spatial filter");
      return SpatialFilter::Stochastic;
    }
}

DirectionalFilter get_directional_filter(const ParamArray& params)
{
    const std::string name = params.get_required<std::string>("directional_filter", "box");

    if (name == "nearest")
    {
      return DirectionalFilter::Nearest;
    }
    else if (name == "box")
    {
      return DirectionalFilter::Box;
    }
    else
    {
      RENDERER_LOG_WARNING("Unknown parameter for directional filter");
      return DirectionalFilter::Box;
    }
}

IterationProgression get_iteration_progression(const ParamArray& params)
{
	const std::string name = params.get_required<std::string>("iteration_progression", "combine");

	if (name == "automatic")
	{
		return IterationProgression::Automatic;
	}
	else if (name == "combine")
	{
		return IterationProgression::Combine;
	}
	else
	{
		RENDERER_LOG_WARNING("Unknown parameter for iteration progression");
		return IterationProgression::Combine;
	}
}

BSDFSamplingFractionMode get_bsdf_sampling_fraction_mode(const ParamArray& params)
{
	const std::string name = params.get_required<std::string>("bsdf_sampling_fraction", "learn");

	if (name == "fixed")
	{
		return BSDFSamplingFractionMode::Fixed;
	}
	else if (name == "learn")
	{
		return BSDFSamplingFractionMode::Learn;
	}
	else
	{
		RENDERER_LOG_WARNING("Unknown parameter for bsdf sampling fraction mode");
		return BSDFSamplingFractionMode::Learn;
	}
}

GuidedBounceMode get_guided_bounce_mode(const ParamArray& params)
{
	const std::string name = params.get_required<std::string>("guided_bounce_mode", "learn");

	if (name == "learn")
	{
		return GuidedBounceMode::Learn;
	}
	else if (name == "strictly_diffuse")
	{
		return GuidedBounceMode::StrictlyDiffuse;
	}
	else if (name == "strictly_glossy")
	{
		return GuidedBounceMode::StrictlyGlossy;
	}
	else if (name == "prefer_diffuse")
	{
		return GuidedBounceMode::PreferDiffuse;
	}
	else if (name == "prefer_glossy")
	{
		return GuidedBounceMode::PreferGlossy;
	}
	else
	{
		RENDERER_LOG_WARNING("Unknown parameter for guided bounce mode");
		return GuidedBounceMode::Learn;
	}
}

SaveMode get_save_mode(const ParamArray& params)
{
	const std::string name = params.get_required<std::string>("save_tree_iterations", "none");

	if (name == "none")
	{
		return SaveMode::None;
	}
	else if (name == "all")
	{
		return SaveMode::All;
	}
	else if (name == "final")
	{
		return SaveMode::Final;
	}
	else
	{
		RENDERER_LOG_WARNING("Unknown parameter for save mode");
		return SaveMode::None;
	}
}

GPTParameters::GPTParameters(const ParamArray& params)
  : m_samples_per_pass(params.get_optional<int>("samples_per_pass", 4))
  , m_fixed_bsdf_sampling_fraction(params.get_optional<float>("fixed_bsdf_sampling_fraction_value", 0.5f))
  , m_learning_rate(params.get_optional<float>("learning_rate", 0.01f))
  , m_bsdf_sampling_fraction_mode(get_bsdf_sampling_fraction_mode(params))
  , m_guided_bounce_mode(get_guided_bounce_mode(params))
  , m_save_mode(get_save_mode(params))
  , m_save_path(params.get_optional<std::string>("file_path", "none"))
  , m_directional_filter(get_directional_filter(params))
  , m_spatial_filter(get_spatial_filter(params))
  , m_iteration_progression(get_iteration_progression(params))
  , m_enable_dl(params.get_optional<bool>("enable_dl", true))
  , m_enable_ibl(params.get_optional<bool>("enable_ibl", true))
  , m_enable_caustics(params.get_optional<bool>("enable_caustics", false))
  , m_max_bounces(fixup_bounces(params.get_optional<int>("max_bounces", 8)))
  , m_max_diffuse_bounces(fixup_bounces(params.get_optional<int>("max_diffuse_bounces", 3)))
  , m_max_guided_bounces(fixup_bounces(params.get_optional<int>("max_guided_bounces", 8)))
  , m_max_glossy_bounces(fixup_bounces(params.get_optional<int>("max_glossy_bounces", 8)))
  , m_max_specular_bounces(fixup_bounces(params.get_optional<int>("max_specular_bounces", 8)))
  , m_max_volume_bounces(fixup_bounces(params.get_optional<int>("max_volume_bounces", 8)))
  , m_clamp_roughness(params.get_optional<bool>("clamp_roughness", false))
  , m_rr_min_path_length(fixup_path_length(params.get_optional<size_t>("rr_min_path_length", 6)))
  , m_next_event_estimation(params.get_optional<bool>("next_event_estimation", true))
  , m_dl_light_sample_count(params.get_optional<float>("dl_light_samples", 1.0f))
  , m_dl_low_light_threshold(params.get_optional<float>("dl_low_light_threshold", 0.0f))
  , m_ibl_env_sample_count(params.get_optional<float>("ibl_env_samples", 1.0f))
  , m_has_max_ray_intensity(params.strings().exist("max_ray_intensity"))
  , m_max_ray_intensity(params.get_optional<float>("max_ray_intensity", 0.0f))
  , m_distance_sample_count(params.get_optional<size_t>("volume_distance_samples", 2))
  , m_enable_equiangular_sampling(!params.get_optional<bool>("optimize_for_lights_outside_volumes", false))
  , m_record_light_paths(params.get_optional<bool>("record_light_paths", false))
{
	// Precompute the reciprocal of the number of light samples.
	m_rcp_dl_light_sample_count =
		m_dl_light_sample_count > 0.0f && m_dl_light_sample_count < 1.0f
			? 1.0f / m_dl_light_sample_count
			: 0.0f;

	// Precompute the reciprocal of the number of environment samples.
	m_rcp_ibl_env_sample_count =
		m_ibl_env_sample_count > 0.0f && m_ibl_env_sample_count < 1.0f
			? 1.0f / m_ibl_env_sample_count
			: 0.0f;
}

void GPTParameters::print() const
{
	std::string bsdf_mode_string = "bsdf sampling mode            ";

	switch (m_bsdf_sampling_fraction_mode)
	{
	case BSDFSamplingFractionMode::Fixed:
		bsdf_mode_string += "Fixed\n";
		bsdf_mode_string += "  fixed bsdf sampling fraction  " + pretty_scalar(m_fixed_bsdf_sampling_fraction, 2);
		break;
	
	case BSDFSamplingFractionMode::Learn:
		bsdf_mode_string += "Learn\n";
		bsdf_mode_string += "  learning rate                 " + pretty_scalar(m_learning_rate, 2);
		break;

	default:
		break;
  	}

	std::string iteration_progression_string;

	switch (m_iteration_progression)
	{
	case IterationProgression::Automatic:
		iteration_progression_string = "Automatic";
		break;

	case IterationProgression::Combine:
		iteration_progression_string = "Combine";
		break;
	
	default:
		break;
	}

	std::string directional_filter_string;

	switch (m_directional_filter)
	{
	case DirectionalFilter::Nearest:
		directional_filter_string = "Nearest";
		break;

	case DirectionalFilter::Box:
		directional_filter_string = "Box";
		break;
	
	default:
		break;
	}

	std::string spatial_filter_string;

	switch (m_spatial_filter)
	{
	case SpatialFilter::Nearest:
		spatial_filter_string = "Nearest";
		break;

	case SpatialFilter::Box:
		spatial_filter_string = "Box";
		break;

	case SpatialFilter::Stochastic:
		spatial_filter_string = "Stochastic";
		break;
	
	default:
		break;
	}

	std::string bounce_mode_string;

	switch (m_guided_bounce_mode)
	{
	case GuidedBounceMode::Learn:
		bounce_mode_string = "Learned Distribution";
		break;

	case GuidedBounceMode::StrictlyDiffuse:
		bounce_mode_string = "Strictly Diffuse";
		break;

	case GuidedBounceMode::StrictlyGlossy:
		bounce_mode_string = "Strictly Glossy";
		break;

	case GuidedBounceMode::PreferDiffuse:
		bounce_mode_string = "Prefer Diffuse";
		break;

	case GuidedBounceMode::PreferGlossy:
		bounce_mode_string = "Prefer Glossy";
		break;
	
	default:
		break;
	}

	std::string save_mode_string;

	switch (m_save_mode)
	{
	case SaveMode::None:
		save_mode_string = "None";
		break;

	case SaveMode::All:
		save_mode_string = "All";
		break;

	case SaveMode::Final:
		save_mode_string = "Final";
		break;
	
	default:
		break;
	}

    RENDERER_LOG_INFO(
        "guided path tracer settings:\n"
        "  samples per pass              %s\n"
        "  iteration progression         %s\n"
        "  %s\n"
        "  spatial filter                %s\n"
        "  directional filter            %s\n"
        "  guided bounce mode            %s\n"
        "  save iterations	             %s\n"
        "  save path		             %s\n"
        "  direct lighting               %s\n"
        "  ibl                           %s\n"
        "  caustics                      %s\n"
        "  max bounces                   %s\n"
        "  max path guided bounces       %s\n"
        "  max diffuse bounces           %s\n"
        "  max glossy bounces            %s\n"
        "  max specular bounces          %s\n"
        "  max volume bounces            %s\n"
        "  russian roulette start bounce %s\n"
        "  next event estimation         %s\n"
        "  dl light samples              %s\n"
        "  dl light threshold            %s\n"
        "  ibl env samples               %s\n"
        "  max ray intensity             %s\n"
        "  volume distance samples       %s\n"
        "  equiangular sampling          %s\n"
        "  clamp roughness               %s",
        pretty_uint(m_samples_per_pass).c_str(),
        iteration_progression_string.c_str(),
        bsdf_mode_string.c_str(),
        spatial_filter_string.c_str(),
        directional_filter_string.c_str(),
        bounce_mode_string.c_str(),
        save_mode_string.c_str(),
        m_save_path.c_str(),
        m_enable_dl ? "on" : "off",
        m_enable_ibl ? "on" : "off",
        m_enable_caustics ? "on" : "off",
        m_max_bounces == ~size_t(0) ? "unlimited" : pretty_uint(m_max_bounces).c_str(),
        m_max_guided_bounces == ~size_t(0) ? "unlimited" : pretty_uint(m_max_guided_bounces).c_str(),
        m_max_diffuse_bounces == ~size_t(0) ? "unlimited" : pretty_uint(m_max_diffuse_bounces).c_str(),
        m_max_glossy_bounces == ~size_t(0) ? "unlimited" : pretty_uint(m_max_glossy_bounces).c_str(),
        m_max_specular_bounces == ~size_t(0) ? "unlimited" : pretty_uint(m_max_specular_bounces).c_str(),
        m_max_volume_bounces == ~size_t(0) ? "unlimited" : pretty_uint(m_max_volume_bounces).c_str(),
        m_rr_min_path_length == ~size_t(0) ? "unlimited" : pretty_uint(m_rr_min_path_length).c_str(),
        m_next_event_estimation ? "on" : "off",
        pretty_scalar(m_dl_light_sample_count).c_str(),
        pretty_scalar(m_dl_low_light_threshold, 3).c_str(),
        pretty_scalar(m_ibl_env_sample_count).c_str(),
        m_has_max_ray_intensity ? pretty_scalar(m_max_ray_intensity).c_str() : "unlimited",
        pretty_int(m_distance_sample_count).c_str(),
        m_enable_equiangular_sampling ? "on" : "off",
        m_clamp_roughness ? "on" : "off");
}

}   // namespace renderer
