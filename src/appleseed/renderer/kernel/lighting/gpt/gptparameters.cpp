
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

// Interface header.
#include "gptparameters.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/settingsparsing.h"

// appleseed.foundation headers.
#include "foundation/utility/string.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

GPTParameters::GPTParameters(const ParamArray& params)
    : m_spp_budget(50)
    , m_bsdf_sampling_fraction(0.0f)
    , m_samples_per_pass(4)
    , m_enable_dl(params.get_optional<bool>("enable_dl", true))
    , m_enable_ibl(params.get_optional<bool>("enable_ibl", true))
    , m_enable_caustics(params.get_optional<bool>("enable_caustics", false))
    , m_max_bounces(fixup_bounces(params.get_optional<int>("max_bounces", 8)))
    , m_max_diffuse_bounces(fixup_bounces(params.get_optional<int>("max_diffuse_bounces", 3)))
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
    RENDERER_LOG_INFO(
        "guided path tracer settings:\n"
        "  sample budget                 %s\n"
        "  bsdf sampling fraction        %s\n"
        "  samples per pass              %s\n"
        "  direct lighting               %s\n"
        "  ibl                           %s\n"
        "  caustics                      %s\n"
        "  max bounces                   %s\n"
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
        pretty_uint(m_spp_budget).c_str(),
        pretty_scalar(m_bsdf_sampling_fraction).c_str(),
        pretty_uint(m_samples_per_pass).c_str(),
        m_enable_dl ? "on" : "off",
        m_enable_ibl ? "on" : "off",
        m_enable_caustics ? "on" : "off",
        m_max_bounces == ~size_t(0) ? "unlimited" : pretty_uint(m_max_bounces).c_str(),
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
