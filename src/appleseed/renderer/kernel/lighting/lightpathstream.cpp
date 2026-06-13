
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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
#include "lightpathstream.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/lighttypes.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/memory/memory.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

LightPathStream::LightPathStream(const Project& project)
  : m_scene(*project.get_scene())   // at this time the scene's render data are not available
{
}

void LightPathStream::clear()
{
    clear_release_memory(m_events);
    clear_release_memory(m_hit_reflector_data);
    clear_release_memory(m_hit_emitter_data);
    clear_release_memory(m_sampled_emitter_data);
    clear_release_memory(m_sampled_env_data);

    clear_release_memory(m_paths);
    clear_release_memory(m_vertices);
}

void LightPathStream::begin_path(
    const PixelContext&     pixel_context,
    const Camera*           camera,
    const Vector3d&         camera_vertex_position)
{
    assert(m_events.empty());
    assert(m_hit_reflector_data.empty());
    assert(m_hit_emitter_data.empty());
    assert(m_sampled_emitter_data.empty());
    assert(m_sampled_env_data.empty());
    assert(camera != nullptr);

    m_scene_diameter = 1.2f * static_cast<float>(m_scene.get_render_data().m_safe_diameter);

    m_camera = camera;
    m_camera_vertex_position = Vector3f(camera_vertex_position);
    m_pixel_coords = pixel_context.get_pixel_coords();
    m_sample_position = Vector2f(pixel_context.get_sample_position());
}

void LightPathStream::hit_reflector(const PathVertex& vertex)
{
    // todo: properly handle this case.
    assert(m_hit_reflector_data.size() < 256);

    Event event;
    event.m_type = EventType::HitReflector;
    event.m_data_index = static_cast<std::uint8_t>(m_hit_reflector_data.size());
    m_events.push_back(event);

    HitReflectorData data;
    data.m_object_instance = &vertex.m_shading_point->get_object_instance();
    data.m_vertex_position = Vector3f(vertex.get_point());
    data.m_path_throughput = vertex.m_throughput.illuminance_to_rgb(g_std_lighting_conditions);
    data.m_crossing_interface = vertex.m_crossing_interface;
    data.m_scattering_type = vertex.m_prev_mode;
    m_hit_reflector_data.push_back(data);
}

void LightPathStream::hit_emitter(
    const PathVertex&       vertex,
    const Spectrum&         emitted_radiance)
{
    // todo: properly handle this case.
    assert(m_hit_emitter_data.size() < 256);

    Event event;
    event.m_type = EventType::HitEmitter;
    event.m_data_index = static_cast<std::uint8_t>(m_hit_emitter_data.size());
    m_events.push_back(event);

    HitEmitterData data;
    data.m_object_instance = &vertex.m_shading_point->get_object_instance();
    data.m_vertex_position = Vector3f(vertex.get_point());
    data.m_path_throughput = vertex.m_throughput.illuminance_to_rgb(g_std_lighting_conditions);
    data.m_emitted_radiance = emitted_radiance.illuminance_to_rgb(g_std_lighting_conditions);
    m_hit_emitter_data.push_back(data);
}

void LightPathStream::sampled_emitting_shape(
    const EmittingShape&    shape,
    const Vector3d&         emission_position,
    const Spectrum&         material_value,
    const Spectrum&         emitted_radiance)
{
    Event event;
    event.m_type = EventType::SampledEmitter;
    event.m_data_index = static_cast<std::uint8_t>(m_sampled_emitter_data.size());
    m_events.push_back(event);

    SampledEmitterData data;
    data.m_entity =
        shape.get_assembly_instance()->get_assembly().object_instances().get_by_index(
            shape.get_object_instance_index());
    data.m_vertex_position = Vector3f(emission_position);
    data.m_material_value = material_value.reflectance_to_rgb(g_std_lighting_conditions);
    data.m_emitted_radiance = emitted_radiance.illuminance_to_rgb(g_std_lighting_conditions);
    m_sampled_emitter_data.push_back(data);
}

void LightPathStream::sampled_non_physical_light(
    const Light&            light,
    const Vector3d&         emission_position,
    const Spectrum&         material_value,
    const Spectrum&         emitted_radiance)
{
    Event event;
    event.m_type = EventType::SampledEmitter;
    event.m_data_index = static_cast<std::uint8_t>(m_sampled_emitter_data.size());
    m_events.push_back(event);

    SampledEmitterData data;
    data.m_entity = &light;
    data.m_vertex_position = Vector3f(emission_position);
    data.m_material_value = material_value.reflectance_to_rgb(g_std_lighting_conditions);
    data.m_emitted_radiance = emitted_radiance.illuminance_to_rgb(g_std_lighting_conditions);
    m_sampled_emitter_data.push_back(data);
}

void LightPathStream::sampled_environment(
    const EnvironmentEDF&   environment_edf,
    const Vector3f&         emission_direction,
    const Spectrum&         material_value,
    const Spectrum&         emitted_radiance)
{
    Event event;
    event.m_type = EventType::SampledEnvironment;
    event.m_data_index = static_cast<std::uint8_t>(m_sampled_env_data.size());
    m_events.push_back(event);

    SampledEnvData data;
    data.m_environment_edf = &environment_edf;
    data.m_emission_direction = emission_direction;
    data.m_material_value = material_value.reflectance_to_rgb(g_std_lighting_conditions);
    data.m_emitted_radiance = emitted_radiance.illuminance_to_rgb(g_std_lighting_conditions);
    m_sampled_env_data.push_back(data);
}

void LightPathStream::sampled_volume(const bool is_homogeneous)
{
    Event event;
    event.m_type = EventType::SampledVolume;
    event.m_data_index = static_cast<std::uint8_t>(m_sampled_volume_data.size());
    m_events.push_back(event);

    SampledVolumeData data;
    data.m_is_homogeneous = is_homogeneous;
    m_sampled_volume_data.push_back(data);
}

void LightPathStream::terminate(const TerminateType& terminate_type)
{
    Event event;
    event.m_type = EventType::Terminate;
    event.m_data_index = static_cast<std::uint8_t>(m_cut_off_data.size());
    m_events.push_back(event);

    TerminateData data;
    data.type = terminate_type;
    m_cut_off_data.push_back(data);
}

void LightPathStream::hit_background()
{
    Event event;
    event.m_type = EventType::HitBackground;
    m_events.push_back(event);
}

void LightPathStream::end_path()
{
    // Ignore paths that fall outside of the supported range.
    if (m_pixel_coords.x >= 0 &&
        m_pixel_coords.y >= 0 &&
        m_pixel_coords.x < 65536 &&
        m_pixel_coords.y < 65536)
    {
        for (size_t i = 0, e = m_events.size(); i < e; ++i)
        {
            switch (m_events[i].m_type)
            {
              case EventType::HitReflector:
                break;

              case EventType::HitEmitter:
                create_path_from_hit_emitter(i);
                break;

              case EventType::SampledEmitter:
                create_path_from_sampled_emitter(i);
                break;

              case EventType::SampledEnvironment:
                create_path_from_sampled_environment(i);
                break;

              assert_otherwise;
            }
        }
    }

    clear_keep_memory(m_events);
    clear_keep_memory(m_hit_reflector_data);
    clear_keep_memory(m_hit_emitter_data);
    clear_keep_memory(m_sampled_emitter_data);
    clear_keep_memory(m_sampled_env_data);
}

std::vector<OIIO::ustring> LightPathStream::build_lpe_events()
{
    std::vector<OIIO::ustring> lpe_events;

    assert(m_camera != nullptr);
    lpe_events.emplace_back("C_");

    for (auto& event : m_events)
    {
        switch (event.m_type)
        {
          case EventType::HitReflector:
          {
            auto& reflector_data = m_hit_reflector_data[event.m_data_index];

            std::string event_type, scattering_type;
            // Determine event type.
            // Volumn is not considered now.
            if (reflector_data.m_crossing_interface)
                event_type = "T";
            else
                event_type = "R";

            // Determine scattering type.
            switch (reflector_data.m_scattering_type)
            {
              case ScatteringMode::Diffuse:
              {
                scattering_type = "D";
                break;
              }
              case ScatteringMode::Glossy:
              {
                scattering_type = "G";
                break;
              }
              case ScatteringMode::Specular:
              {
                scattering_type = "S";
                break;
              }

              // Ignore the "s" type for now.

              assert_otherwise;
            }

            lpe_events.emplace_back(OIIO::ustring(event_type + scattering_type, 0));
            break;
          }

          case EventType::HitEmitter:
          {
            lpe_events.emplace_back("O_");
            break;
          }

          case EventType::SampledEmitter:
          {
            lpe_events.emplace_back("O_");
            break;
          }

          case EventType::SampledEnvironment:
          {
            lpe_events.emplace_back("B_");
            break;
          }

          case EventType::SampledVolume:
          {
            lpe_events.emplace_back("V_");
            break;
          }

          case EventType::HitBackground:
          {
            lpe_events.emplace_back("B_");
            break;
          }

          case EventType::Terminate:
          {
            lpe_events.emplace_back("X_");
            break;
          }

          assert_otherwise;
        }
    }

    return lpe_events;
}

void LightPathStream::create_path_from_hit_emitter(const size_t emitter_event_index)
{
    const auto& hit_emitter_event = m_events[emitter_event_index];
    const auto& hit_emitter_data = m_hit_emitter_data[hit_emitter_event.m_data_index];

    // Create path.
    StoredPath stored_path;
    stored_path.m_pixel_coords = Vector2u16(m_pixel_coords);
    stored_path.m_sample_position = m_sample_position;
    stored_path.m_vertex_begin_index = static_cast<std::uint32_t>(m_vertices.size());

    // Emitter vertex.
    StoredPathVertex emitter_vertex;
    emitter_vertex.m_entity = hit_emitter_data.m_object_instance;
    emitter_vertex.m_position = hit_emitter_data.m_vertex_position;
    emitter_vertex.m_radiance = hit_emitter_data.m_emitted_radiance;
    m_vertices.push_back(emitter_vertex);

    Color3f current_radiance = hit_emitter_data.m_emitted_radiance;
    Color3f prev_throughput = hit_emitter_data.m_path_throughput;

    // Walk back the list of events and create path vertices.
    for (size_t i = emitter_event_index; i > 0; --i)
    {
        const auto event_index = i - 1;
        const auto& event = m_events[event_index];
        if (event.m_type == EventType::HitReflector ||
            event.m_type == EventType::HitEmitter)
        {
            const auto& event_data = get_reflector_data(event_index);

            // Reflector vertex.
            StoredPathVertex reflector_vertex;
            reflector_vertex.m_entity = event_data.m_object_instance;
            reflector_vertex.m_position = event_data.m_vertex_position;
            reflector_vertex.m_radiance = current_radiance;
            m_vertices.push_back(reflector_vertex);

            // Update current radiance.
            const auto& throughput = event_data.m_path_throughput;
            // Multiply by previous throughput to attenuate by the next hit
            current_radiance *= prev_throughput;
            // Divide by throughput before previous so that we isolate only throughput from the light source to current vertex,
            // since throughput is cumulative in reverse
            current_radiance /= throughput;
            prev_throughput = throughput;

            // If the hit is an emitter, add the radiance to current
            if (event.m_type == EventType::HitEmitter)
            {
                current_radiance += m_hit_emitter_data[event.m_data_index].m_emitted_radiance;
            }
        }
    }

    // Camera vertex.
    StoredPathVertex camera_vertex;
    camera_vertex.m_entity = m_camera;
    camera_vertex.m_position = m_camera_vertex_position;
    camera_vertex.m_radiance = current_radiance;
    m_vertices.push_back(camera_vertex);

    // Store path.
    stored_path.m_vertex_end_index = static_cast<std::uint32_t>(m_vertices.size());
    m_paths.push_back(stored_path);
}

void LightPathStream::create_path_from_sampled_emitter(const size_t emitter_event_index)
{
    // Find the last scattering event.
    assert(emitter_event_index > 0);
    size_t last_scattering_event_index = emitter_event_index - 1;
    while (m_events[last_scattering_event_index].m_type != EventType::HitReflector &&
           m_events[last_scattering_event_index].m_type != EventType::HitEmitter)
        --last_scattering_event_index;
    const HitReflectorData& last_reflector_data = get_reflector_data(last_scattering_event_index);

    const auto& sampled_emitter_event = m_events[emitter_event_index];
    const auto& sampled_emitter_data = m_sampled_emitter_data[sampled_emitter_event.m_data_index];

    // Create path.
    StoredPath stored_path;
    stored_path.m_pixel_coords = Vector2u16(m_pixel_coords);
    stored_path.m_sample_position = m_sample_position;
    stored_path.m_vertex_begin_index = static_cast<std::uint32_t>(m_vertices.size());

    // Emitter vertex.
    StoredPathVertex emitter_vertex;
    emitter_vertex.m_entity = sampled_emitter_data.m_entity;
    emitter_vertex.m_position = sampled_emitter_data.m_vertex_position;
    emitter_vertex.m_radiance = sampled_emitter_data.m_emitted_radiance;
    m_vertices.push_back(emitter_vertex);

    Color3f current_radiance = sampled_emitter_data.m_emitted_radiance;
    Color3f prev_throughput = sampled_emitter_data.m_material_value * last_reflector_data.m_path_throughput;

    // Walk back the list of events and create path vertices.
    for (size_t i = last_scattering_event_index + 1; i > 0; --i)
    {
        const auto event_index = i - 1;
        const auto& event = m_events[event_index];
        if (event.m_type == EventType::HitReflector ||
            event.m_type == EventType::HitEmitter)
        {
            const auto& event_data = get_reflector_data(event_index);

            // Reflector vertex.
            StoredPathVertex reflector_vertex;
            reflector_vertex.m_entity = event_data.m_object_instance;
            reflector_vertex.m_position = event_data.m_vertex_position;
            reflector_vertex.m_radiance = current_radiance;
            m_vertices.push_back(reflector_vertex);

            // Update current radiance.
            const auto& throughput = event_data.m_path_throughput;
            // Multiply by previous throughput to attenuate by the next hit
            current_radiance *= prev_throughput;
            // Divide by throughput before previous so that we isolate only throughput from the light source to current vertex,
            // since throughput is cumulative in reverse
            current_radiance /= throughput;
            prev_throughput = throughput;

            // If the hit is an emitter, add the radiance to current
            if (event.m_type == EventType::HitEmitter)
            {
                current_radiance += m_hit_emitter_data[event.m_data_index].m_emitted_radiance;
            }
        }
    }

    // Camera vertex.
    StoredPathVertex camera_vertex;
    camera_vertex.m_entity = m_camera;
    camera_vertex.m_position = m_camera_vertex_position;
    camera_vertex.m_radiance = current_radiance;
    m_vertices.push_back(camera_vertex);

    // Store path.
    stored_path.m_vertex_end_index = static_cast<std::uint32_t>(m_vertices.size());
    m_paths.push_back(stored_path);
}

void LightPathStream::create_path_from_sampled_environment(const size_t env_event_index)
{
    // Find the last scattering event.
    assert(env_event_index > 0);
    size_t last_scattering_event_index = env_event_index - 1;
    while (m_events[last_scattering_event_index].m_type != EventType::HitReflector &&
           m_events[last_scattering_event_index].m_type != EventType::HitEmitter)
        --last_scattering_event_index;
    const HitReflectorData& last_reflector_data = get_reflector_data(last_scattering_event_index);

    const auto& sampled_env_event = m_events[env_event_index];
    const auto& sampled_env_data = m_sampled_env_data[sampled_env_event.m_data_index];

    // Create path.
    StoredPath stored_path;
    stored_path.m_pixel_coords = Vector2u16(m_pixel_coords);
    stored_path.m_sample_position = m_sample_position;
    stored_path.m_vertex_begin_index = static_cast<std::uint32_t>(m_vertices.size());

    // Emitter vertex.
    StoredPathVertex emitter_vertex;
    emitter_vertex.m_entity = sampled_env_data.m_environment_edf;
    emitter_vertex.m_position = last_reflector_data.m_vertex_position + m_scene_diameter * sampled_env_data.m_emission_direction;
    emitter_vertex.m_radiance = sampled_env_data.m_emitted_radiance;
    m_vertices.push_back(emitter_vertex);

    Color3f current_radiance = sampled_env_data.m_emitted_radiance;
    Color3f prev_throughput = sampled_env_data.m_material_value * last_reflector_data.m_path_throughput;

    // Walk back the list of events and create path vertices.
    for (size_t i = last_scattering_event_index + 1; i > 0; --i)
    {
        const auto event_index = i - 1;
        const auto& event = m_events[event_index];
        if (event.m_type == EventType::HitReflector ||
            event.m_type == EventType::HitEmitter)
        {
            const auto& event_data = get_reflector_data(event_index);

            // Reflector vertex.
            StoredPathVertex reflector_vertex;
            reflector_vertex.m_entity = event_data.m_object_instance;
            reflector_vertex.m_position = event_data.m_vertex_position;
            reflector_vertex.m_radiance = current_radiance;
            m_vertices.push_back(reflector_vertex);

            // Update current radiance.
            const auto& throughput = event_data.m_path_throughput;
            // Multiply by previous throughput to attenuate by the next hit
            current_radiance *= prev_throughput;
            // Divide by throughput before previous so that we isolate only throughput from the light source to current vertex,
            // since throughput is cumulative in reverse
            current_radiance /= throughput;
            prev_throughput = throughput;

            // If the hit is an emitter, add the radiance to current
            if (event.m_type == EventType::HitEmitter)
            {
                current_radiance += m_hit_emitter_data[event.m_data_index].m_emitted_radiance;
            }
        }
    }

    // Camera vertex.
    StoredPathVertex camera_vertex;
    camera_vertex.m_entity = m_camera;
    camera_vertex.m_position = m_camera_vertex_position;
    camera_vertex.m_radiance = current_radiance;
    m_vertices.push_back(camera_vertex);

    // Store path.
    stored_path.m_vertex_end_index = static_cast<std::uint32_t>(m_vertices.size());
    m_paths.push_back(stored_path);
}

const LightPathStream::HitReflectorData& LightPathStream::get_reflector_data(const size_t event_index) const
{
    const auto& event = m_events[event_index];

    assert(
        event.m_type == EventType::HitReflector ||
        event.m_type == EventType::HitEmitter);

    return
        event.m_type == EventType::HitReflector
            ? m_hit_reflector_data[event.m_data_index]
            : m_hit_emitter_data[event.m_data_index];
}

}   // namespace renderer
