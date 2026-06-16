
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
#include "diagnosticsurfaceshader.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovcomponents.h"
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/shading/ambientocclusion.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/kernel/shading/shadowcatcher.h"
#include "renderer/modeling/aov/screenspacevelocityaov.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/hash/hash.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/math/aabb.h"
#include "foundation/math/distance.h"
#include "foundation/math/minmax.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <string>

using namespace foundation;

namespace renderer
{

#undef SHADE_VECTORS_USING_3DSMAX_CONVENTIONS

namespace
{
    const char* Model = "diagnostic_surface_shader";
}

const KeyValuePair<const char*, DiagnosticSurfaceShader::ShadingMode>
    DiagnosticSurfaceShader::ShadingModeValues[] =
{
    { "coverage",                   Coverage },
    { "facing_ratio",               FacingRatio },
    { "albedo",                     Albedo },
    { "barycentric",                Barycentric },
    { "uv",                         UV },
    { "tangent",                    Tangent },
    { "bitangent",                  Bitangent },
    { "geometric_normal",           GeometricNormal },
    { "shading_normal",             ShadingNormal },
    { "original_shading_normal",    OriginalShadingNormal },
    { "world_space_position",       WorldSpacePosition },
    { "world_space_velocity",       WorldSpaceVelocity },
    { "screen_space_velocity",      ScreenSpaceVelocity },
    { "sides",                      Sides },
    { "depth",                      Depth },
    { "world_space_wireframe" ,     WorldSpaceWireframe },
    { "screen_space_wireframe" ,    ScreenSpaceWireframe },
    { "ambient_occlusion",          AmbientOcclusion },
    { "assemblies",                 Assemblies },
    { "assembly_instances",         AssemblyInstances },
    { "objects",                    Objects },
    { "object_instances",           ObjectInstances },
    { "primitives",                 Primitives },
    { "materials",                  Materials },
    { "ray_spread",                 RaySpread }
};

const KeyValuePair<const char*, const char*> DiagnosticSurfaceShader::ShadingModeNames[] =
{
    { "coverage",                   "Coverage" },
    { "facing_ratio",               "Facing Ratio" },
    { "albedo",                     "Albedo" },
    { "barycentric",                "Barycentric Coordinates" },
    { "uv",                         "UV Coordinates" },
    { "tangent",                    "Tangents" },
    { "bitangent",                  "Bitangents" },
    { "geometric_normal",           "Geometric Normals" },
    { "shading_normal",             "Shading Normals" },
    { "original_shading_normal",    "Original Shading Normals" },
    { "world_space_position",       "World-Space Position" },
    { "world_space_velocity",       "World-Space Velocity" },
    { "screen_space_velocity",      "Screen-Space Velocity" },
    { "sides",                      "Sides" },
    { "depth",                      "Depth" },
    { "world_space_wireframe",      "World-Space Wireframe" },
    { "screen_space_wireframe",     "Screen-Space Wireframe" },
    { "ambient_occlusion",          "Ambient Occlusion" },
    { "assemblies",                 "Assemblies" },
    { "assembly_instances",         "Assembly Instances" },
    { "objects",                    "Objects" },
    { "object_instances",           "Object Instances" },
    { "primitives",                 "Primitives" },
    { "materials",                  "Materials" },
    { "ray_spread",                 "Ray Spread" }
};

struct DiagnosticSurfaceShader::Impl
{
    ShadingMode m_shading_mode;
    double      m_ao_max_distance;
    size_t      m_ao_samples;
    Vector3d    m_scene_aabb_min;
    Vector3d    m_rcp_scene_aabb_extent;

    explicit Impl(const ParamArray& params)
    {
        // Retrieve shading mode.
        const std::string mode_string = params.get_required<std::string>("mode", "coverage");
        const KeyValuePair<const char*, ShadingMode>* mode_pair =
            lookup_kvpair_array(ShadingModeValues, ShadingModeCount, mode_string);
        if (mode_pair != nullptr)
            m_shading_mode = mode_pair->m_value;
        else
        {
            RENDERER_LOG_ERROR(
                "invalid shading mode \"%s\", using default value \"coverage\".",
                mode_string.c_str());
            m_shading_mode = Coverage;
        }

        // Retrieve ambient occlusion parameters.
        if (m_shading_mode == AmbientOcclusion)
        {
            const ParamArray& ao_params = params.child("ambient_occlusion");
            m_ao_max_distance = ao_params.get_optional<double>("max_distance", 1.0);
            m_ao_samples = ao_params.get_optional<size_t>("samples", 16);
        }
    }
};

DiagnosticSurfaceShader::DiagnosticSurfaceShader(
    const char*                 name,
    const ParamArray&           params)
  : SurfaceShader(name, params)
  , impl(new Impl(params))
{
}

DiagnosticSurfaceShader::~DiagnosticSurfaceShader()
{
    delete impl;
}

void DiagnosticSurfaceShader::release()
{
    delete this;
}

const char* DiagnosticSurfaceShader::get_model() const
{
    return Model;
}

bool DiagnosticSurfaceShader::on_render_begin(
    const Project&              project,
    const BaseGroup*            parent,
    OnRenderBeginRecorder&      recorder,
    IAbortSwitch*               abort_switch)
{
    if (!Entity::on_render_begin(project, parent, recorder, abort_switch))
        return false;

    const AABB3d scene_aabb(project.get_scene()->get_render_data().m_bbox);
    const Vector3d scene_extent = scene_aabb.extent();

    impl->m_scene_aabb_min = scene_aabb.min;
    impl->m_rcp_scene_aabb_extent[0] = scene_extent[0] != 0.0 ? 1.0 / scene_extent[0] : 0.0;
    impl->m_rcp_scene_aabb_extent[1] = scene_extent[1] != 0.0 ? 1.0 / scene_extent[1] : 0.0;
    impl->m_rcp_scene_aabb_extent[2] = scene_extent[2] != 0.0 ? 1.0 / scene_extent[2] : 0.0;

    return true;
}

namespace
{
    // Like foundation::wrap() defined in foundation/math/scalar.h but for [0,1] instead of [0,1).
    template <typename T>
    inline T wrap1(const T x)
    {
        if (x < T(0.0) || x > T(1.0))
        {
            const T y = std::fmod(x, T(1.0));
            return y < T(0.0) ? y + T(1.0) : y;
        }
        else return x;
    }

    // Compute a color from a given unit-length 3D vector.
    template <typename T>
    inline Color3f unit_vector3_to_color(const Vector<T, 3>& vec)
    {
        assert(is_normalized(vec));

#ifdef SHADE_VECTORS_USING_3DSMAX_CONVENTIONS
        return Color3f(
            saturate(( static_cast<float>(vec[0]) + 1.0f) * 0.5f),
            saturate((-static_cast<float>(vec[2]) + 1.0f) * 0.5f),
            saturate(( static_cast<float>(vec[1]) + 1.0f) * 0.5f));
#else
        return Color3f(
            saturate((static_cast<float>(vec[0]) + 1.0f) * 0.5f),
            saturate((static_cast<float>(vec[1]) + 1.0f) * 0.5f),
            saturate((static_cast<float>(vec[2]) + 1.0f) * 0.5f));
#endif
    }

    inline void set_shading_result(
        ShadingResult&  shading_result,
        const Color4f&  color)
    {
        shading_result.m_main = color;
    }

    inline void set_shading_result(
        ShadingResult&  shading_result,
        const Color3f&  color)
    {
        shading_result.m_main.rgb() = color;
        shading_result.m_main.a = 1.0f;
    }

    inline void set_shading_result(
        ShadingResult&  shading_result,
        const Spectrum& value)
    {
        shading_result.m_main.rgb() = value.illuminance_to_rgb(g_std_lighting_conditions);
        shading_result.m_main.a = 1.0f;
    }
}

void DiagnosticSurfaceShader::evaluate(
    SamplingContext&            sampling_context,
    const PixelContext&         pixel_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point,
    ShadingResult&              shading_result,
    ShadingComponents&          shading_components,
    AOVComponents&              aov_components,
    ShadowCatcher&              shadow_catcher) const
{
    switch (impl->m_shading_mode)
    {
      case Coverage:
        set_shading_result(shading_result, Color3f(1.0f));
        break;

      case FacingRatio:
        {
            const Vector3d& normal = shading_point.get_shading_normal();
            const Vector3d& view = shading_point.get_ray().m_dir;
            const double facing = std::abs(dot(normal, view));
            set_shading_result(
                shading_result,
                Color3f(static_cast<float>(facing)));
        }
        break;

      case Albedo:
        {
            shading_result.set_main_to_opaque_pink();

            const Material* material = shading_point.get_material();
            if (material != nullptr)
            {
                const Material::RenderData& material_data = material->get_render_data();

                // Execute the OSL shader if there is one.
                if (material_data.m_shader_group)
                {
                    shading_context.execute_osl_shading(
                        *material_data.m_shader_group,
                        shading_point);
                }

                if (material_data.m_bsdf != nullptr)
                {
                    const ShadingRay& ray = shading_point.get_ray();
                    const Dual3d outgoing(
                        -ray.m_dir,
                        -ray.m_rx_dir,
                        -ray.m_ry_dir);

                    BSDF::LocalGeometry local_geometry;
                    local_geometry.m_shading_point = &shading_point;
                    local_geometry.m_geometric_normal = Vector3f(shading_point.get_geometric_normal());
                    local_geometry.m_shading_basis = Basis3f(shading_point.get_shading_basis());

                    BSDFSample sample;
                    material_data.m_bsdf->sample(
                        sampling_context,
                        material_data.m_bsdf->evaluate_inputs(shading_context, shading_point),
                        false,
                        false,
                        local_geometry,
                        Dual3f(outgoing),
                        ScatteringMode::All,
                        sample);

                    set_shading_result(shading_result, sample.m_aov_components.m_albedo);
                }
            }
        }
        break;

      case Barycentric:
        {
            const Vector2f& bary = shading_point.get_bary();
            const float r = wrap1(static_cast<float>(bary[0]));
            const float g = wrap1(static_cast<float>(bary[1]));
            const float b = wrap1(1.0f - r - g);
            set_shading_result(shading_result, Color3f(r, g, b));
        }
        break;

      case UV:
        {
            const Vector2f& uv = shading_point.get_uv(0);
            const float r = wrap1(static_cast<float>(uv[0]));
            const float g = wrap1(static_cast<float>(uv[1]));
            set_shading_result(shading_result, Color3f(r, g, 0.0f));
        }
        break;

      case Tangent:
      case Bitangent:
      case ShadingNormal:
        {
            const Material* material = shading_point.get_material();
            if (material != nullptr)
            {
                const Material::RenderData& material_data = material->get_render_data();

                // Execute the OSL shader if there is one.
                if (material_data.m_shader_group)
                {
                    sampling_context.split_in_place(2, 1);
                    shading_context.execute_osl_bump(
                        *material_data.m_shader_group,
                        shading_point,
                        sampling_context.next2<Vector2f>());
                }
            }

            const Vector3d v =
                impl->m_shading_mode == ShadingNormal ? shading_point.get_shading_basis().get_normal() :
                impl->m_shading_mode == Tangent ? shading_point.get_shading_basis().get_tangent_u() :
                shading_point.get_shading_basis().get_tangent_v();

            set_shading_result(shading_result, unit_vector3_to_color(v));
        }
        break;

      case GeometricNormal:
        set_shading_result(
            shading_result,
            unit_vector3_to_color(shading_point.get_geometric_normal()));
        break;

      case OriginalShadingNormal:
        set_shading_result(
            shading_result,
            unit_vector3_to_color(shading_point.get_original_shading_normal()));
        break;

      case WorldSpacePosition:
        {
            const Vector3d d = shading_point.get_point() - impl->m_scene_aabb_min;
            const Vector3d p = saturate(d * impl->m_rcp_scene_aabb_extent);
            set_shading_result(
                shading_result,
                Color3f(Color3d(p.x, p.y, p.z)));
        }
        break;

      case WorldSpaceVelocity:
        {
            Vector3d v = shading_point.get_world_space_point_velocity();
            const double vn = norm(v);
            if (vn > 0.0)
            {
                v /= vn;
                set_shading_result(
                    shading_result,
                    Color3f(
                        static_cast<float>((v[0] + 1.0) * 0.5),
                        static_cast<float>((v[1] + 1.0) * 0.5),
                        static_cast<float>(vn)));
            }
            else set_shading_result(shading_result, Color3f(0.0f));
        }
        break;

      case ScreenSpaceVelocity:
        set_shading_result(
            shading_result,
            compute_screen_space_velocity_color(shading_point, 0.0));
        break;

      case Sides:
        set_shading_result(
            shading_result,
            shading_point.get_side() == ObjectInstance::FrontSide
                ? Color3f(0.0f, 0.0f, 1.0f)
                : Color3f(1.0f, 0.0f, 0.0f));
        break;

      case Depth:
        set_shading_result(
            shading_result,
            Color3f(static_cast<float>(shading_point.get_distance())));
        break;

      case WorldSpaceWireframe:
        {
            // Initialize the shading result to the background color.
            set_shading_result(shading_result, Color4f(0.0f, 0.0f, 0.8f, 0.5f));

            switch (shading_point.get_primitive_type())
            {
              case ShadingPoint::PrimitiveTriangle:
                {
                    // World space thickness of the wires.
                    const double SquareWireThickness = square(0.0015);

                    // Retrieve the world space intersection point.
                    const Vector3d& point = shading_point.get_point();

                    // Loop over the triangle edges.
                    for (size_t i = 0; i < 3; ++i)
                    {
                        // Retrieve the end points of this edge.
                        const size_t j = (i + 1) % 3;
                        const Vector3d& vi = shading_point.get_vertex(i);
                        const Vector3d& vj = shading_point.get_vertex(j);

                        // Compute the world space distance from the intersection point to the edge.
                        const double d = square_distance_point_segment(point, vi, vj);

                        // Shade with the wire's color if the hit point is close enough to the edge.
                        if (d < SquareWireThickness)
                        {
                            set_shading_result(shading_result, Color4f(1.0f));
                            break;
                        }
                    }
                }
                break;

              case ShadingPoint::PrimitiveProceduralSurface:
                // todo: implement.
                break;

              case ShadingPoint::PrimitiveCurve1:
              case ShadingPoint::PrimitiveCurve3:
                // todo: implement.
                break;

              assert_otherwise;
            }
        }
        break;

      case ScreenSpaceWireframe:
        {
            // Initialize the shading result to the background color.
            set_shading_result(shading_result, Color4f(0.0f, 0.0f, 0.8f, 0.5f));

            switch (shading_point.get_primitive_type())
            {
              case ShadingPoint::PrimitiveTriangle:
                {
                    // Film space thickness of the wires.
                    const double SquareWireThickness = square(0.00025);

                    // Retrieve the time, the scene and the camera.
                    const float time = shading_point.get_time().m_absolute;
                    const Scene& scene = shading_point.get_scene();
                    const Camera& camera = *scene.get_render_data().m_active_camera;

                    // Compute the film space coordinates of the intersection point.
                    Vector2d point_ndc;
                    camera.project_point(time, shading_point.get_point(), point_ndc);

                    // Loop over the triangle edges.
                    for (size_t i = 0; i < 3; ++i)
                    {
                        // Retrieve the end points of this edge.
                        const size_t j = (i + 1) % 3;
                        const Vector3d vi = shading_point.get_vertex(i);
                        const Vector3d vj = shading_point.get_vertex(j);

                        // Compute the film space coordinates of the edge's end points.
                        Vector2d vi_ndc, vj_ndc;
                        if (!camera.project_segment(time, vi, vj, vi_ndc, vj_ndc))
                            continue;

                        // Compute the film space distance from the intersection point to the edge.
                        const double d = square_distance_point_segment(point_ndc, vi_ndc, vj_ndc);

                        // Shade with the wire's color if the hit point is close enough to the edge.
                        if (d < SquareWireThickness)
                        {
                            set_shading_result(shading_result, Color4f(1.0f));
                            break;
                        }
                    }
                }
                break;

              case ShadingPoint::PrimitiveProceduralSurface:
                // todo: implement.
                break;

              case ShadingPoint::PrimitiveCurve1:
              case ShadingPoint::PrimitiveCurve3:
                // todo: implement.
                break;

              assert_otherwise;
            }
        }
        break;

      case AmbientOcclusion:
        {
            // Compute the occlusion.
            const double occlusion =
                compute_ambient_occlusion(
                    sampling_context,
                    sample_hemisphere_uniform<double>,
                    shading_context.get_intersector(),
                    shading_point,
                    impl->m_ao_max_distance,
                    impl->m_ao_samples);

            // Return a gray scale value proportional to the accessibility.
            const float accessibility = static_cast<float>(1.0 - occlusion);
            set_shading_result(shading_result, Color3f(accessibility));
        }
        break;

      case Assemblies:
        set_shading_result(
            shading_result,
            integer_to_color3<float>(shading_point.get_assembly().get_uid()));
        break;

      case AssemblyInstances:
        set_shading_result(
            shading_result,
            integer_to_color3<float>(shading_point.get_assembly_instance().get_uid()));
        break;

      case Objects:
        set_shading_result(
            shading_result,
            integer_to_color3<float>(shading_point.get_object().get_uid()));
        break;

      case ObjectInstances:
        set_shading_result(
            shading_result,
            integer_to_color3<float>(shading_point.get_object_instance().get_uid()));
        break;

      case Primitives:
        {
            const std::uint32_t h =
                mix_uint32(
                    static_cast<std::uint32_t>(shading_point.get_object_instance().get_uid()),
                    static_cast<std::uint32_t>(shading_point.get_primitive_index()));
            set_shading_result(shading_result, integer_to_color3<float>(h));
        }
        break;

      case Materials:
        {
            const Material* material = shading_point.get_material();
            if (material != nullptr)
                set_shading_result(shading_result, integer_to_color3<float>(material->get_uid()));
            else shading_result.set_main_to_opaque_pink();
        }
        break;

      case RaySpread:
        {
            const ShadingRay& ray = shading_point.get_ray();
            if (!ray.m_has_differentials)
                break;

            const Material* material = shading_point.get_material();
            if (material != nullptr)
            {
                const Material::RenderData& material_data = material->get_render_data();

                // Execute the OSL shader if there is one.
                if (material_data.m_shader_group)
                {
                    shading_context.execute_osl_shading(
                        *material_data.m_shader_group,
                        shading_point);
                }

                if (material_data.m_bsdf)
                {
                    const Dual3d outgoing(
                        -ray.m_dir,
                        -ray.m_rx_dir,
                        -ray.m_ry_dir);

                    BSDF::LocalGeometry local_geometry;
                    local_geometry.m_shading_point = &shading_point;
                    local_geometry.m_geometric_normal = Vector3f(shading_point.get_geometric_normal());
                    local_geometry.m_shading_basis = Basis3f(shading_point.get_shading_basis());

                    BSDFSample sample;
                    material_data.m_bsdf->sample(
                        sampling_context,
                        material_data.m_bsdf->evaluate_inputs(shading_context, shading_point),
                        false,
                        false,
                        local_geometry,
                        Dual3f(outgoing),
                        ScatteringMode::All,
                        sample);

                    if (!sample.m_incoming.has_derivatives())
                        break;

                    // The 3.0 factor is chosen so that ray spread from Lambertian BRDFs is approximately 1.
                    const double spread =
                        std::max(
                            norm(sample.m_incoming.get_dx() - sample.m_incoming.get_value()),
                            norm(sample.m_incoming.get_dy() - sample.m_incoming.get_value())) * 3.0;
                    set_shading_result(
                        shading_result,
                        Color3f(static_cast<float>(spread)));

                }
            }
        }
        break;

      assert_otherwise;
    }
}


//
// DiagnosticSurfaceShaderFactory class implementation.
//

void DiagnosticSurfaceShaderFactory::release()
{
    delete this;
}

const char* DiagnosticSurfaceShaderFactory::get_model() const
{
    return Model;
}

Dictionary DiagnosticSurfaceShaderFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Diagnostics");
}

DictionaryArray DiagnosticSurfaceShaderFactory::get_input_metadata() const
{
    Dictionary model_items;

    for (int i = 0; i < DiagnosticSurfaceShader::ShadingModeCount; ++i)
    {
        const char* shading_mode_value = DiagnosticSurfaceShader::ShadingModeNames[i].m_key;
        const char* shading_mode_name = DiagnosticSurfaceShader::ShadingModeNames[i].m_value;
        model_items.insert(shading_mode_name, shading_mode_value);
    }

    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "mode")
            .insert("label", "Mode")
            .insert("type", "enumeration")
            .insert("items", model_items)
            .insert("use", "required")
            .insert("default", "coverage")
            .insert("on_change", "rebuild_form"));

    return metadata;
}

auto_release_ptr<SurfaceShader> DiagnosticSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<SurfaceShader>(new DiagnosticSurfaceShader(name, params));
}

}   // namespace renderer
