
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

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <vector>

// Forward declarations.
namespace renderer  { class Camera; }
namespace renderer  { class EmittingShape; }
namespace renderer  { class Entity; }
namespace renderer  { class EnvironmentEDF; }
namespace renderer  { class Light; }
namespace renderer  { class ObjectInstance; }
namespace renderer  { class PathVertex; }
namespace renderer  { class PixelContext; }
namespace renderer  { class Project; }
namespace renderer  { class Scene; }

namespace renderer
{

//
// This class allows a single thread to collect light paths in memory.
//

class LightPathStream
{
  public:
    void clear();

    void begin_path(
        const PixelContext&             pixel_context,
        const Camera*                   camera,
        const foundation::Vector3d&     camera_vertex_position,
        const foundation::Vector3d&     camera_vertex_normal);

    void hit_reflector(
        const PathVertex&               vertex);

    void hit_emitter(
        const PathVertex&               vertex,
        const Spectrum&                 emitted_radiance);

    void sampled_emitting_shape(
        const EmittingShape&            shape,
        const foundation::Vector3d&     emission_position,
        const foundation::Vector3d&     emission_normal,
        const Spectrum&                 material_value,
        const Spectrum&                 emitted_radiance);

    void sampled_non_physical_light(
        const Light&                    light,
        const foundation::Vector3d&     emission_position,
        const foundation::Vector3d&     emission_normal,
        const Spectrum&                 material_value,
        const Spectrum&                 emitted_radiance);

    void sampled_environment(
        const EnvironmentEDF&           environment_edf,
        const foundation::Vector3f&     emission_direction,
        const Spectrum&                 material_value,
        const Spectrum&                 emitted_radiance);

    void end_path();

  private:
    friend class LightPathRecorder;

    enum class EventType : std::uint8_t
    {
        HitReflector,
        HitEmitter,
        SampledEmitter,
        SampledEnvironment
    };

    struct Event
    {
        EventType                   m_type;
        std::uint8_t                m_data_index;               // index of this event's data in one of the data array
    };

    struct HitReflectorData
    {
        const ObjectInstance*       m_object_instance;          // object instance that was hit
        foundation::Vector3f        m_vertex_position;          // world space position of the hit point on the reflector
        foundation::Vector3f        m_surface_normal;           // world space normal of the surface of the reflector
        foundation::Color3f         m_path_throughput;          // cumulative path throughput up to but excluding this vertex, in reverse order (i.e. in the order from camera to light source)
    };

    struct HitEmitterData
      : public HitReflectorData                                 // an emitter is also a reflector
    {
        foundation::Color3f         m_emitted_radiance;         // emitted radiance in W.sr^-1.m^-2
    };

    struct SampledEmitterData
    {
        const Entity*               m_entity;                   // object instance or non-physical light that was sampled
        foundation::Vector3f        m_vertex_position;          // world space position of the emitting point on the emitter
        foundation::Vector3f        m_surface_normal;           // world space normal of the surface of the emitter
        foundation::Color3f         m_material_value;           // BSDF value at the previous vertex
        foundation::Color3f         m_emitted_radiance;         // emitted radiance in W.sr^-1.m^-2
    };

    struct SampledEnvData
    {
        const EnvironmentEDF*       m_environment_edf;          // environment EDF that was sampled
        foundation::Vector3f        m_emission_direction;       // world space emission direction pointing toward the environment
        foundation::Color3f         m_material_value;           // BSDF value at the previous vertex
        foundation::Color3f         m_emitted_radiance;         // emitted radiance in W.sr^-1.m^-2
    };

    typedef foundation::Vector<std::uint16_t, 2> Vector2u16;

    struct StoredPath
    {
        Vector2u16                  m_pixel_coords;
        foundation::Vector2f        m_sample_position;
        std::uint32_t               m_vertex_begin_index;       // index of the first vertex in m_vertices
        std::uint32_t               m_vertex_end_index;         // index of one vertex past the last one in m_vertices
    };

    struct StoredPathVertex
    {
        const Entity*               m_entity;                   // object instance or non-physical light
        foundation::Vector3f        m_position;                 // world space position of this vertex
        foundation::Color3f         m_radiance;                 // radiance arriving at this vertex, in W.sr^-1.m^-2
        foundation::Vector3f        m_surface_normal;           // world space normal of the surface at this vertex
    };

    // Scene.
    const Scene&                    m_scene;
    float                           m_scene_diameter;

    // Camera event (transient).
    const Camera*                   m_camera;
    foundation::Vector2i            m_pixel_coords;
    foundation::Vector2f            m_sample_position;
    foundation::Vector3f            m_camera_vertex_position;
    foundation::Vector3f            m_camera_vertex_normal;

    // Scattering events (transient).
    std::vector<Event>              m_events;
    std::vector<HitReflectorData>   m_hit_reflector_data;
    std::vector<HitEmitterData>     m_hit_emitter_data;
    std::vector<SampledEmitterData> m_sampled_emitter_data;
    std::vector<SampledEnvData>     m_sampled_env_data;

    // Final representation as paths and path vertices (persistent).
    std::vector<StoredPath>         m_paths;
    std::vector<StoredPathVertex>   m_vertices;

    // Constructor.
    explicit LightPathStream(const Project& project);

    void create_path_from_hit_emitter(const size_t emitter_event_index);
    void create_path_from_sampled_emitter(const size_t emitter_event_index);
    void create_path_from_sampled_environment(const size_t env_event_index);

    const HitReflectorData& get_reflector_data(const size_t event_index) const;
};

}   // namespace renderer
