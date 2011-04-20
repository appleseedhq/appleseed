
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/distance.h"
#include "foundation/math/frustum.h"
#include "foundation/math/hash.h"
#include "foundation/math/minmax.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/containers/dictionaryarray.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    // Utility function to compute a color from a given normal vector.
    inline void normal_to_color(const Vector3d& n, Spectrum& output)
    {
        assert(abs(n[0]) <= 1.0);
        assert(abs(n[1]) <= 1.0);
        assert(abs(n[2]) <= 1.0);

        output[0] = static_cast<float>((n[0] + 1.0) * 0.5);
        output[1] = static_cast<float>((n[1] + 1.0) * 0.5);
        output[2] = static_cast<float>((n[2] + 1.0) * 0.5);
    }

    // Utility function to compute a color from a given integer value.
    template <typename T>
    inline void integer_to_color(const T i, Spectrum& output)
    {
        const uint32 u = static_cast<uint32>(i);    // keep the low 32 bits

        const uint32 x = hashint32(u);
        const uint32 y = hashint32(u + 1);
        const uint32 z = hashint32(u + 2);

        output[0] = static_cast<float>(x) * (1.0f / 4294967295.0f);
        output[1] = static_cast<float>(y) * (1.0f / 4294967295.0f);
        output[2] = static_cast<float>(z) * (1.0f / 4294967295.0f);
    }
}


//
// Diagnostic surface shader.
//

const char* Model = "diagnostic_surface_shader";

struct DiagnosticSurfaceShader::Impl
{
    ShadingMode                 m_shading_mode;
    double                      m_ao_max_distance;
    size_t                      m_ao_samples;
};

const KeyValuePair<const char*, DiagnosticSurfaceShader::ShadingMode>
    DiagnosticSurfaceShader::ShadingModeValues[] =
{
    { "coverage",               Coverage },
    { "barycentric",            Barycentric },
    { "uv",                     UV },
    { "geometric_normal",       GeometricNormal },
    { "shading_normal",         ShadingNormal },
    { "assembly_instances",     AssemblyInstances },
    { "object_instances",       ObjectInstances },
    { "regions",                Regions },
    { "triangles",              Triangles },
    { "materials",              Materials  },
    { "ambient_occlusion",      AmbientOcclusion },
    { "wireframe" ,             Wireframe }
};

const KeyValuePair<const char*, const char*> DiagnosticSurfaceShader::ShadingModeNames[] =
{
    { "coverage",               "Coverage" },
    { "barycentric",            "Barycentric Coordinates" },
    { "uv",                     "UV Coordinates" },
    { "geometric_normal",       "Geometric Normals" },
    { "shading_normal",         "Shading Normals" },
    { "assembly_instances",     "Assembly Instances" },
    { "object_instances",       "Object Instances" },
    { "regions",                "Regions" },
    { "triangles",              "Triangles" },
    { "materials",              "Materials" },
    { "ambient_occlusion",      "Ambient Occlusion" },
    { "wireframe" ,             "Wireframe" }
};

DiagnosticSurfaceShader::DiagnosticSurfaceShader(
    const char*             name,
    const ParamArray&       params)
  : SurfaceShader(params)
  , impl(new Impl())
{
    set_name(name);
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

void DiagnosticSurfaceShader::evaluate(
    SamplingContext&        sampling_context,
    const ShadingContext&   shading_context,
    const ShadingPoint&     shading_point,
    ShadingResult&          shading_result) const
{
    // Set color space to linear RGB.
    shading_result.m_color_space = ColorSpaceLinearRGB;

    switch (impl->m_shading_mode)
    {
      // Shade according to pixel coverage
      case Coverage:
        shading_result.m_color[0] = 1.0f;
        shading_result.m_color[1] = 1.0f;
        shading_result.m_color[2] = 1.0f;
        shading_result.m_alpha = Alpha(1.0);
        break;

      // Shade according to barycentric coordinates.
      case Barycentric:
        {
            const Vector2d& bary = shading_point.get_bary();
            const double w = 1.0 - bary[0] - bary[1];
            shading_result.m_color[0] = static_cast<float>(w);
            shading_result.m_color[1] = static_cast<float>(bary[0]);
            shading_result.m_color[2] = static_cast<float>(bary[1]);
            shading_result.m_alpha = Alpha(1.0);
        }
        break;

      // Shade according to UV coordinates from UV set #0.
      case UV:
        {
            const Vector2d& uv0 = shading_point.get_uv(0);
            const double w = 1.0 - uv0[0] - uv0[1];
            shading_result.m_color[0] = static_cast<float>(w);
            shading_result.m_color[1] = static_cast<float>(uv0[0]);
            shading_result.m_color[2] = static_cast<float>(uv0[1]);
            shading_result.m_alpha = Alpha(1.0);
        }
        break;

      // Shade according to the geometric normal.
      case GeometricNormal:
        normal_to_color(
            shading_point.get_geometric_normal(),
            shading_result.m_color);
        shading_result.m_alpha = Alpha(1.0);
        break;

      // Shade according to the shading normal.
      case ShadingNormal:
        normal_to_color(
            shading_point.get_shading_normal(),
            shading_result.m_color);
        shading_result.m_alpha = Alpha(1.0);
        break;

      // Assign an unique color to each assembly instance.
      case AssemblyInstances:
        integer_to_color(
            shading_point.get_assembly_instance_uid(),
            shading_result.m_color);
        shading_result.m_alpha = Alpha(1.0);
        break;

      // Assign an unique color to each object instance.
      case ObjectInstances:
        {
            const uint32 h = mix32(
                static_cast<uint32>(shading_point.get_assembly_instance_uid()),
                static_cast<uint32>(shading_point.get_object_instance_index()));
            integer_to_color(h, shading_result.m_color);
            shading_result.m_alpha = Alpha(1.0);
        }
        break;

      // Assign an unique color to each region.
      case Regions:
        {
            const uint32 h = mix32(
                static_cast<uint32>(shading_point.get_assembly_instance_uid()),
                static_cast<uint32>(shading_point.get_object_instance_index()),
                static_cast<uint32>(shading_point.get_region_index()));
            integer_to_color(h, shading_result.m_color);
            shading_result.m_alpha = Alpha(1.0);
        }
        break;

      // Assign an unique color to each triangle.
      case Triangles:
        {
            const uint32 h = mix32(
                static_cast<uint32>(shading_point.get_assembly_instance_uid()),
                static_cast<uint32>(shading_point.get_object_instance_index()),
                static_cast<uint32>(shading_point.get_region_index()),
                static_cast<uint32>(shading_point.get_triangle_index()));
            integer_to_color(h, shading_result.m_color);
            shading_result.m_alpha = Alpha(1.0);
        }
        break;

      // Assign an unique color to each material.
      case Materials:
        {
            const ObjectInstance& object_instance = shading_point.get_object_instance();
            const MaterialIndexArray& material_indices = object_instance.get_material_indices();
            const size_t pa_index = shading_point.get_primitive_attribute_index();
            if (pa_index < material_indices.size())
            {
                const size_t material_index = material_indices[pa_index];
                const uint32 h = mix32(
                    static_cast<uint32>(shading_point.get_assembly_instance_uid()),
                    static_cast<uint32>(material_index));
                integer_to_color(h, shading_result.m_color);
            }
            else
            {
                shading_result.m_color[0] = 1.0f;
                shading_result.m_color[1] = 0.0f;
                shading_result.m_color[2] = 1.0f;
            }
            shading_result.m_alpha = Alpha(1.0);
        }
        break;

      // Ambient occlusion.
      case AmbientOcclusion:
        {
            // Compute the occlusion.
            const double occlusion =
                compute_ambient_occlusion(
                    sampling_context,
                    shading_context.get_intersector(),
                    shading_point.get_point(),
                    shading_point.get_geometric_normal(),
                    shading_point.get_shading_basis(),
                    impl->m_ao_max_distance,
                    impl->m_ao_samples,
                    &shading_point);

            // Return a gray scale value proportional to the accessibility.
            const float accessibility = static_cast<float>(1.0 - occlusion);
            shading_result.m_color[0] = accessibility;
            shading_result.m_color[1] = accessibility;
            shading_result.m_color[2] = accessibility;
            shading_result.m_alpha = Alpha(1.0);
        }
        break;

      // Wireframe.
      case Wireframe:
        {
            // Film space thickness of the wires.
            const double SquareWireThickness = square(0.0005);

            // Initialize the shading result to the background color.
            shading_result.m_color[0] = 0.0f;
            shading_result.m_color[1] = 0.0f;
            shading_result.m_color[2] = 0.8f;
            shading_result.m_alpha = Alpha(0.5);

            // Retrieve the camera.
            const Scene& scene = shading_point.get_scene();
            const Camera& camera = *scene.get_camera();
            const Transformd& camera_transform = camera.get_transform();
            const Pyramid3d& view_pyramid = camera.get_view_pyramid();

            // Compute the film space coordinates of the intersection point.
            const Vector3d& point = shading_point.get_point();
            const Vector3d point_cs = camera_transform.transform_point_to_local(point);
            const Vector2d point_fs = camera.project(point_cs);

            // Compute the camera space coordinates of the triangle vertices.
            Vector3d v_cs[3];
            v_cs[0] = camera_transform.transform_point_to_local(shading_point.get_vertex(0));
            v_cs[1] = camera_transform.transform_point_to_local(shading_point.get_vertex(1));
            v_cs[2] = camera_transform.transform_point_to_local(shading_point.get_vertex(2));

            // Loop over the triangle edges.
            for (size_t i = 0; i < 3; ++i)
            {
                // Compute the end points of this edge.
                const size_t j = (i + 1) % 3;
                Vector3d vi_cs = v_cs[i];
                Vector3d vj_cs = v_cs[j];

                // Clip the edge against the view pyramid.
                if (!view_pyramid.clip(vi_cs, vj_cs))
                    continue;

                // Transform the edge to film space.
                const Vector2d vi_fs = camera.project(vi_cs);
                const Vector2d vj_fs = camera.project(vj_cs);

                // Compute the film space distance from the intersection point to the edge.
                const double d = square_distance_point_segment(point_fs, vi_fs, vj_fs);

                if (d < SquareWireThickness)
                {
                    shading_result.m_color[0] = 1.0f;
                    shading_result.m_color[1] = 1.0f;
                    shading_result.m_color[2] = 1.0f;
                    shading_result.m_alpha = Alpha(1.0);
                    break;
                }
            }
        }
        break;

      // Invalid shader.
      default:
        assert(false);
        shading_result.m_color[0] = 1.0f;
        shading_result.m_color[1] = 0.0f;
        shading_result.m_color[2] = 1.0f;
        shading_result.m_alpha = Alpha(1.0);
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
    {
        impl->m_shading_mode = mode_pair->m_value;
    }
    else
    {
        RENDERER_LOG_ERROR(
            "invalid shading mode \"%s\", using default value \"coverage\"",
            mode_string.c_str());
        impl->m_shading_mode = Coverage;
    }

    // Retrieve ambient occlusion parameters.
    if (impl->m_shading_mode == AmbientOcclusion)
    {
        const ParamArray& ao_params = m_params.child("ambient_occlusion");
        impl->m_ao_max_distance = ao_params.get_required<double>("max_distance", 1.0);
        impl->m_ao_samples = ao_params.get_required<size_t>("samples", 16);
    }
}


//
// DiagnosticSurfaceShaderFactory class implementation.
//

const char* DiagnosticSurfaceShaderFactory::get_model() const
{
    return Model;
}

const char* DiagnosticSurfaceShaderFactory::get_human_readable_model() const
{
    return "Diagnostics";
}

DictionaryArray DiagnosticSurfaceShaderFactory::get_widget_definitions() const
{
    DictionaryArray definitions;
    return definitions;
}

auto_release_ptr<SurfaceShader> DiagnosticSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<SurfaceShader>(
            new DiagnosticSurfaceShader(name, params));
}

}   // namespace renderer
