
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "gpuptlightingengine.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/platform/types.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <limits>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // GPU Path Tracing lighting engine.
    //

    class GPUPTLightingEngine
      : public IGPULightingEngine
    {
      public:
        explicit GPUPTLightingEngine(const ParamArray& params)
          : m_params(params)
        {
        }

        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
            RENDERER_LOG_INFO(
                "unidirectional GPU path tracer settings:\n"
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
                m_params.m_enable_dl ? "on" : "off",
                m_params.m_enable_ibl ? "on" : "off",
                m_params.m_enable_caustics ? "on" : "off",
                m_params.m_max_bounces == ~size_t(0) ? "unlimited" : pretty_uint(m_params.m_max_bounces).c_str(),
                m_params.m_max_diffuse_bounces == ~size_t(0) ? "unlimited" : pretty_uint(m_params.m_max_diffuse_bounces).c_str(),
                m_params.m_max_glossy_bounces == ~size_t(0) ? "unlimited" : pretty_uint(m_params.m_max_glossy_bounces).c_str(),
                m_params.m_max_specular_bounces == ~size_t(0) ? "unlimited" : pretty_uint(m_params.m_max_specular_bounces).c_str(),
                m_params.m_max_volume_bounces == ~size_t(0) ? "unlimited" : pretty_uint(m_params.m_max_volume_bounces).c_str(),
                m_params.m_rr_min_path_length == ~size_t(0) ? "unlimited" : pretty_uint(m_params.m_rr_min_path_length).c_str(),
                m_params.m_next_event_estimation ? "on" : "off",
                pretty_scalar(m_params.m_dl_light_sample_count).c_str(),
                pretty_scalar(m_params.m_dl_low_light_threshold, 3).c_str(),
                pretty_scalar(m_params.m_ibl_env_sample_count).c_str(),
                m_params.m_has_max_ray_intensity ? pretty_scalar(m_params.m_max_ray_intensity).c_str() : "unlimited",
                pretty_int(m_params.m_distance_sample_count).c_str(),
                m_params.m_enable_equiangular_sampling ? "on" : "off",
                m_params.m_clamp_roughness ? "on" : "off");
        }

        StatisticsVector get_statistics() const override
        {
            Statistics stats;
            stats.insert("path count", m_path_count);
            stats.insert("path length", m_path_length);

            return StatisticsVector::make("path tracing statistics", stats);
        }

      private:
        struct Parameters
        {
            const bool      m_enable_dl;                    // is direct lighting enabled?
            const bool      m_enable_ibl;                   // is image-based lighting enabled?
            const bool      m_enable_caustics;              // are caustics enabled?

            const size_t    m_max_bounces;                  // maximum number of bounces, ~0 for unlimited
            const size_t    m_max_diffuse_bounces;          // maximum number of diffuse bounces, ~0 for unlimited
            const size_t    m_max_glossy_bounces;           // maximum number of glossy bounces, ~0 for unlimited
            const size_t    m_max_specular_bounces;         // maximum number of specular bounces, ~0 for unlimited
            const size_t    m_max_volume_bounces;           // maximum number of volume scattering events, ~0 for unlimited

            const bool      m_clamp_roughness;

            const size_t    m_rr_min_path_length;           // minimum path length before Russian Roulette kicks in, ~0 for unlimited
            const bool      m_next_event_estimation;        // use next event estimation?

            const float     m_dl_light_sample_count;        // number of light samples used to estimate direct illumination
            const float     m_dl_low_light_threshold;       // light contribution threshold to disable shadow rays
            const float     m_ibl_env_sample_count;         // number of environment samples used to estimate IBL
            float           m_rcp_dl_light_sample_count;
            float           m_rcp_ibl_env_sample_count;

            const bool      m_has_max_ray_intensity;
            const float     m_max_ray_intensity;

            const size_t    m_distance_sample_count;        // number of distance samples for volume rendering
            const bool      m_enable_equiangular_sampling;  // optimize for lights that are located outside volumes

            const bool      m_record_light_paths;

            explicit Parameters(const ParamArray& params)
              : m_enable_dl(params.get_optional<bool>("enable_dl", true))
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

            static size_t fixup_bounces(const int x)
            {
                return x == -1 ? ~size_t(0) : x;
            }

            static size_t fixup_path_length(const size_t x)
            {
                return x == 0 ? ~size_t(0) : x;
            }
        };

        const Parameters                m_params;

        uint64                          m_path_count;
        Population<uint64>              m_path_length;
    };
}


//
// GPUPTLightingEngineFactory class implementation.
//

GPUPTLightingEngineFactory::GPUPTLightingEngineFactory(const ParamArray& params)
  : m_params(params)
{
}

void GPUPTLightingEngineFactory::release()
{
    delete this;
}

IGPULightingEngine* GPUPTLightingEngineFactory::create()
{
    return new GPUPTLightingEngine(m_params);
}

Dictionary GPUPTLightingEngineFactory::get_params_metadata()
{
    Dictionary metadata;
    return metadata;
}

}   // namespace renderer
