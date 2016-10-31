
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/shading/ambientocclusion.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/distance.h"
#include "foundation/math/hash.h"
#include "foundation/math/minmax.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>

using namespace foundation;
using namespace std;

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
    { "color",                      Color },
    { "coverage",                   Coverage },
    { "barycentric",                Barycentric },
    { "uv",                         UV },
    { "tangent",                    Tangent },
    { "bitangent",                  Bitangent },
    { "geometric_normal",           GeometricNormal },
    { "shading_normal",             ShadingNormal },
    { "original_shading_normal",    OriginalShadingNormal },
    { "world_space_position",       WorldSpacePosition },
    { "sides",                      Sides },
    { "depth",                      Depth },
    { "screen_space_wireframe" ,    ScreenSpaceWireframe },
    { "world_space_wireframe" ,     WorldSpaceWireframe },
    { "ambient_occlusion",          AmbientOcclusion },
    { "assembly_instances",         AssemblyInstances },
    { "object_instances",           ObjectInstances },
    { "regions",                    Regions },
    { "primitives",                 Primitives },
    { "materials",                  Materials },
    { "ray_spread",                 RaySpread },
    { "facing_ratio",               FacingRatio }
};

const KeyValuePair<const char*, const char*> DiagnosticSurfaceShader::ShadingModeNames[] =
{
    { "color",                      "Color" },
    { "coverage",                   "Coverage" },
    { "barycentric",                "Barycentric Coordinates" },
    { "uv",                         "UV Coordinates" },
    { "tangent",                    "Tangents" },
    { "bitangent",                  "Bitangents" },
    { "geometric_normal",           "Geometric Normals" },
    { "shading_normal",             "Shading Normals" },
    { "original_shading_normal",    "Original Shading Normals" },
    { "world_space_position",       "World Space Position" },
    { "sides",                      "Sides" },
    { "depth",                      "Depth" },
    { "screen_space_wireframe",     "Screen-space Wireframe" },
    { "world_space_wireframe",      "World-space Wireframe" },
    { "ambient_occlusion",          "Ambient Occlusion" },
    { "assembly_instances",         "Assembly Instances" },
    { "object_instances",           "Object Instances" },
    { "regions",                    "Regions" },
    { "primitives",                 "Primitives" },
    { "materials",                  "Materials" },
    { "ray_spread",                 "Ray Spread" },
    { "facing_ratio",               "Facing Ratio" }

};

DiagnosticSurfaceShader::DiagnosticSurfaceShader(
    const char*             name,
    const ParamArray&       params)
  : SurfaceShader(name, params)
{
    extract_parameters();
}

void DiagnosticSurfaceShader::release()
{
    delete this;
}

const char* DiagnosticSurfaceShader::get_model() const
{
    return Model;
}

namespace
{
    // Like foundation::wrap() defined in foundation/math/scalar.h but for [0,1] instead of [0,1).
    template <typename T>
    inline T wrap1(const T x)
    {
        if (x < T(0.0) || x > T(1.0))
        {
            const T y = fmod(x, T(1.0));
            return y < T(0.0) ? y + T(1.0) : y;
        }
        else return x;
    }

    // Compute a color from a given 2D vector.
    template <typename T>
    inline Color3f vector2_to_color(const Vector<T, 2>& vec)
    {
        const float u = wrap1(static_cast<float>(vec[0]));
        const float v = wrap1(static_cast<float>(vec[1]));
        const float w = wrap1(1.0f - u - v);
        return Color3f(u, v, w);
    }

    // Compute a color from uv coordinates.
    template <typename T>
    inline Color3f uvs_to_color(const Vector<T, 2>& vec)
    {
        const float u = wrap1(static_cast<float>(vec[0]));
        const float v = wrap1(static_cast<float>(vec[1]));
        return Color3f(u, v, 0.0f);
    }

    // Compute a color from a given unit-length 3D vector.
    template <typename T>
    inline Color3f vector3_to_color(const Vector<T, 3>& vec)
    {
        assert(is_normalized(vec));

#ifdef SHADE_VECTORS_USING_3DSMAX_CONVENTIONS
        return Color3f(
            static_cast<float>(( vec[0] + T(1.0)) * T(0.5)),
            static_cast<float>((-vec[2] + T(1.0)) * T(0.5)),
            static_cast<float>(( vec[1] + T(1.0)) * T(0.5)));
#else
        return Color3f(
            static_cast<float>((vec[0] + T(1.0)) * T(0.5)),
            static_cast<float>((vec[1] + T(1.0)) * T(0.5)),
            static_cast<float>((vec[2] + T(1.0)) * T(0.5)));
#endif
    }

    // Compute a color from a given integer.
    template <typename T>
    inline Color3f integer_to_color(const T i)
    {
        const uint32 u = static_cast<uint32>(i);    // keep the low 32 bits

        const uint32 x = hash_uint32(u);
        const uint32 y = hash_uint32(u + 1);
        const uint32 z = hash_uint32(u + 2);

        return Color3f(
            static_cast<float>(x) * (1.0f / 4294967295.0f),
            static_cast<float>(y) * (1.0f / 4294967295.0f),
            static_cast<float>(z) * (1.0f / 4294967295.0f));
    }
}

void DiagnosticSurfaceShader::evaluate(
    SamplingContext&        sampling_context,
    const PixelContext&     pixel_context,
    const ShadingContext&   shading_context,
    const ShadingPoint&     shading_point,
    ShadingResult&          shading_result) const
{
    switch (m_shading_mode)
    {
      case Color:
        {
            shading_result.set_main_to_opaque_pink_linear_rgba();

            const Material* material = shading_point.get_material();
            if (material)
            {
                const Material::RenderData& material_data = material->get_render_data();

#ifdef APPLESEED_WITH_OSL
                // Execute the OSL shader if there is one.
                if (material_data.m_shader_group)
                {
                    shading_context.execute_osl_shading(
                        *material_data.m_shader_group,
                        shading_point);
                }
#endif

                if (material_data.m_bsdf)
                {
                    InputEvaluator input_evaluator(shading_context.get_texture_cache());
                    material_data.m_bsdf->evaluate_inputs(
                        shading_context,
                        input_evaluator,
                        shading_point);

                    const Vector3f direction = -normalize(Vector3f(shading_point.get_ray().m_dir));
                    material_data.m_bsdf->evaluate(
                        input_evaluator.data(),
                        false,
                        false,
                        Vector3f(shading_point.get_geometric_normal()),
                        Basis3f(shading_point.get_shading_basis()),
                        direction,
                        direction,
                        ScatteringMode::All,
                        shading_result.m_main.m_color);

                    shading_result.m_color_space = ColorSpaceSpectral;
                }
            }
        }
        break;

      case Coverage:
        shading_result.set_main_to_linear_rgb(Color3f(1.0f));
        break;

      case Barycentric:
        shading_result.set_main_to_linear_rgb(
            vector2_to_color(shading_point.get_bary()));
        break;

      case UV:
        shading_result.set_main_to_linear_rgb(
            uvs_to_color(shading_point.get_uv(0)));
        break;

      case Tangent:
      case Bitangent:
      case ShadingNormal:
        {
#ifdef APPLESEED_WITH_OSL
            const Material* material = shading_point.get_material();
            if (material)
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
#endif

            const Vector3d v =
                m_shading_mode == ShadingNormal ? shading_point.get_shading_basis().get_normal() :
                m_shading_mode == Tangent ? shading_point.get_shading_basis().get_tangent_u() :
                shading_point.get_shading_basis().get_tangent_v();

            shading_result.set_main_to_linear_rgb(vector3_to_color(v));
        }
        break;

      case GeometricNormal:
        shading_result.set_main_to_linear_rgb(
            vector3_to_color(shading_point.get_geometric_normal()));
        break;

      case OriginalShadingNormal:
        shading_result.set_main_to_linear_rgb(
            vector3_to_color(shading_point.get_original_shading_normal()));
        break;

      case WorldSpacePosition:
        {
            const Vector3d& p = shading_point.get_point();
            shading_result.set_main_to_linear_rgb(
                Color3f(Color3d(p.x, p.y, p.z)));
        }
        break;

      case Sides:
        shading_result.set_main_to_linear_rgb(
            shading_point.get_side() == ObjectInstance::FrontSide
                ? Color3f(0.0f, 0.0f, 1.0f)
                : Color3f(1.0f, 0.0f, 0.0f));
        break;

      case Depth:
        shading_result.set_main_to_linear_rgb(
            Color3f(static_cast<float>(shading_point.get_distance())));
        break;

      case ScreenSpaceWireframe:
        {
            // Initialize the shading result to the background color.
            shading_result.set_main_to_linear_rgba(Color4f(0.0f, 0.0f, 0.8f, 0.5f));

            if (shading_point.is_triangle_primitive())
            {
                // Film space thickness of the wires.
                const double SquareWireThickness = square(0.00025);

                // Retrieve the time, the scene and the camera.
                const float time = shading_point.get_time().m_absolute;
                const Scene& scene = shading_point.get_scene();
                const Camera& camera = *scene.get_active_camera();

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
                        shading_result.set_main_to_linear_rgba(Color4f(1.0f));
                        break;
                    }
                }
            }
            else
            {
                assert(shading_point.is_curve_primitive());

                // todo: implement.
            }
        }
        break;

      case WorldSpaceWireframe:
        {
            // Initialize the shading result to the background color.
            shading_result.set_main_to_linear_rgba(Color4f(0.0f, 0.0f, 0.8f, 0.5f));

            if (shading_point.is_triangle_primitive())
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
                        shading_result.set_main_to_linear_rgba(Color4f(1.0f));
                        break;
                    }
                }
            }
            else
            {
                assert(shading_point.is_curve_primitive());

                // todo: implement.
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
                    m_ao_max_distance,
                    m_ao_samples);

            // Return a gray scale value proportional to the accessibility.
            const float accessibility = static_cast<float>(1.0 - occlusion);
            shading_result.set_main_to_linear_rgb(Color3f(accessibility));
        }
        break;

      case AssemblyInstances:
        shading_result.set_main_to_linear_rgb(
            integer_to_color(shading_point.get_assembly_instance().get_uid()));
        break;

      case ObjectInstances:
        shading_result.set_main_to_linear_rgb(
            integer_to_color(shading_point.get_object_instance().get_uid()));
        break;

      case Regions:
        {
            const uint32 h =
                mix_uint32(
                    static_cast<uint32>(shading_point.get_object_instance().get_uid()),
                    static_cast<uint32>(shading_point.get_region_index()));
            shading_result.set_main_to_linear_rgb(integer_to_color(h));
        }
        break;

      case Primitives:
        {
            const uint32 h =
                mix_uint32(
                    static_cast<uint32>(shading_point.get_object_instance().get_uid()),
                    static_cast<uint32>(shading_point.get_region_index()),
                    static_cast<uint32>(shading_point.get_primitive_index()));
            shading_result.set_main_to_linear_rgb(integer_to_color(h));
        }
        break;

      case Materials:
        {
            const Material* material = shading_point.get_material();
            if (material)
                shading_result.set_main_to_linear_rgb(integer_to_color(material->get_uid()));
            else shading_result.set_main_to_opaque_pink_linear_rgba();
        }
        break;

      case RaySpread:
        {
            const ShadingRay& ray = shading_point.get_ray();
            if (!ray.m_has_differentials)
                break;

            const Material* material = shading_point.get_material();
            if (material)
            {
                const Material::RenderData& material_data = material->get_render_data();

#ifdef APPLESEED_WITH_OSL
                // Execute the OSL shader if there is one.
                if (material_data.m_shader_group)
                {
                    shading_context.execute_osl_shading(
                        *material_data.m_shader_group,
                        shading_point);
                }
#endif

                if (material_data.m_bsdf)
                {
                    const Dual3d outgoing(
                        -ray.m_dir,
                        ray.m_dir - ray.m_rx.m_dir,
                        ray.m_dir - ray.m_ry.m_dir);

                    InputEvaluator input_evaluator(shading_context.get_texture_cache());
                    material_data.m_bsdf->evaluate_inputs(
                        shading_context,
                        input_evaluator,
                        shading_point);
                    const void* bsdf_data = input_evaluator.data();

                    BSDFSample sample(shading_point, outgoing);
                    material_data.m_bsdf->sample(
                        sampling_context,
                        bsdf_data,
                        false,
                        false,
                        sample);

                    if (!sample.m_incoming.has_derivatives())
                        break;

                    // The 3.0 factor is chosen so that ray spread from Lambertian BRDFs is approximately 1.
                    const double spread =
                        max(
                            norm(sample.m_incoming.get_dx()),
                            norm(sample.m_incoming.get_dy())) * 3.0;

                    shading_result.set_main_to_linear_rgb(
                        Color3f(static_cast<float>(spread)));
                }
            }
        }
        break;

      case FacingRatio:
        {
            const Vector3d& normal = shading_point.get_shading_normal();
            const Vector3d& view   = shading_point.get_ray().m_dir;

            const double facing = abs(dot(normal, view));

            shading_result.set_main_to_linear_rgb(
                Color3f(static_cast<float>(facing)));
        }
        break;

      default:
        assert(false);
        shading_result.set_main_to_transparent_black_linear_rgba();
        break;
    }
}

void DiagnosticSurfaceShader::extract_parameters()
{
    // Retrieve shading mode.
    const string mode_string = m_params.get_required<string>("mode", "coverage");
    const KeyValuePair<const char*, ShadingMode>* mode_pair =
        lookup_kvpair_array(ShadingModeValues, ShadingModeCount, mode_string);
    if (mode_pair)
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
        const ParamArray& ao_params = m_params.child("ambient_occlusion");
        m_ao_max_distance = ao_params.get_optional<double>("max_distance", 1.0);
        m_ao_samples = ao_params.get_optional<size_t>("samples", 16);
    }
}


//
// DiagnosticSurfaceShaderFactory class implementation.
//

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

auto_release_ptr<SurfaceShader> DiagnosticSurfaceShaderFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<SurfaceShader>(new DiagnosticSurfaceShader(name, params));
}

}   // namespace renderer
