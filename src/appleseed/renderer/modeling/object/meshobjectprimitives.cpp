
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Ramon Blanquer, The appleseedhq Organization
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
#include "meshobjectprimitives.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/meshobjectoperations.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <algorithm>
#include <cmath>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    class ParametricSurfaceBase
    {
      public:

        const Transformf& transform() const
        {
            return Transformf::identity();
        }
    };

    class ParametricGrid
      : public ParametricSurfaceBase
    {
      public:
        ParametricGrid(const float width, const float height)
          : m_width(width)
          , m_height(height)
        {
        }

        GVector3 evaluate(const float u, const float v) const
        {
            return GVector3(
                m_width * (u - 0.5f),
                0.0f,
                m_height * (v - 0.5f));
        }

        GVector3 evaluate_du(const float u, const float v) const
        {
            return GVector3(1.0f, 0.0f, 0.0f);
        }

        GVector3 evaluate_normal(const float u, const float v) const
        {
            return GVector3(0.0f, 1.0f, 0.0f);
        }

      private:
        const float m_width;
        const float m_height;
    };

    class ParametricDisk
      : public ParametricSurfaceBase
    {
      public:
        explicit ParametricDisk(const float radius)
          : m_radius(radius)
        {
        }

        GVector3 evaluate(const float u, const float v) const
        {
            const float r = u * 2.0f * m_radius;
            const float theta = static_cast<float>(TwoPi) * v;
            return GVector3(r * cos(theta), 0.0f, r * sin(theta));
        }

        GVector3 evaluate_du(const float u, const float v) const
        {
            GVector3 dpdu = evaluate(u, v);
            const float dpdu_norm = norm(dpdu);

            // Return an arbitrary tangent at center of the disk.
            if (dpdu_norm == 0.0f)
                return GVector3(-1.0f, 0.0f, 0.0f);

            return dpdu / dpdu_norm;
        }

        GVector3 evaluate_normal(const float u, const float v) const
        {
            return GVector3(0.0f, 1.0f, 0.0f);
        }

      private:
        const float m_radius;
    };

    class ParametricSphere
      : public ParametricSurfaceBase
    {
      public:
        ParametricSphere(const float radius, const size_t resolution_u)
          : m_radius(radius)
          , m_h(1.0f / resolution_u * 4.0f)
          , m_transform(Matrix4f::make_rotation_x(static_cast<float>(HalfPi)))
        {
        }

        const Transformf& transform() const
        {
            return m_transform;
        }

        GVector3 evaluate(const float u, const float v) const
        {
            const float theta = static_cast<float>(TwoPi) * (1.0f - u);
            const float phi = static_cast<float>(Pi) * v;
            const float sin_phi = sin(phi);

            return GVector3(
                m_radius * cos(theta) * sin_phi,
                m_radius * sin(theta) * sin_phi,
                m_radius * cos(phi));
        }

        GVector3 evaluate_du(const float u, const float v) const
        {
            const GVector3 p = evaluate(u, v);

            // Compute u tangent using forward differencing.
            GVector3 dpdu = evaluate(u + m_h, v) - p;
            const float dpdu_norm = norm(dpdu);

            // Return an arbitrary tangent at the poles.
            if (dpdu_norm == 0.0f)
                return GVector3(-1.0f, 0.0f, 0.0f);

            return dpdu / dpdu_norm;
        }

        GVector3 evaluate_normal(const float u, const float v) const
        {
            return normalize(evaluate(u, v));
        }

      private:
        const float         m_radius;
        const float         m_h;
        const Transformf    m_transform;
    };

    class ParametricTorus
      : public ParametricSurfaceBase
    {
      public:
        ParametricTorus(
            const float major_radius,
            const float minor_radius,
            const size_t resolution_u,
            const size_t resolution_v)
          : m_major_radius(major_radius)
          , m_minor_radius(minor_radius)
          , m_hu(1.0f / (resolution_u * 4.0f))
          , m_hv(1.0f / (resolution_v * 4.0f))
          , m_transform(Matrix4f::make_rotation_x(static_cast<float>(HalfPi)))
        {
        }

        const Transformf& transform() const
        {
            return m_transform;
        }

        GVector3 evaluate(const float u, const float v) const
        {
            const float theta = static_cast<float>(TwoPi) * (1.0f - u);
            const float phi = static_cast<float>(TwoPi) * (1.0f - v);
            const float cos_phi = cos(phi);

            return GVector3(
                (m_major_radius + m_minor_radius * cos_phi) * cos(theta),
                (m_major_radius + m_minor_radius * cos_phi) * sin(theta),
                m_minor_radius * sin(phi));
        }

        GVector3 evaluate_du(const float u, const float v) const
        {
            // Compute u tangent using forward differencing.
            const GVector3 p = evaluate(u, v);
            GVector3 dpdu = evaluate(u + m_hu, v) - p;

            return normalize(dpdu);
        }

        GVector3 evaluate_normal(const float u, const float v) const
        {
            // Compute tangents using forward differencing.
            const GVector3 p = evaluate(u, v);
            GVector3 dpdu = evaluate(u + m_hu, v) - p;
            GVector3 dpdv = evaluate(u, v + m_hv) - p;

            // Compute normal.
            return normalize(cross(dpdu, dpdv));
        }

      private:
        const float         m_major_radius;
        const float         m_minor_radius;
        const float         m_hu, m_hv;
        const Transformf    m_transform;
    };

    template <typename ParametricSurface>
    void create_vertices(
        MeshObject&         mesh,
        ParametricSurface   surface,
        const size_t        resolution_u,
        const size_t        resolution_v)
    {
        const size_t num_points = resolution_u * resolution_v;

        mesh.reserve_vertices(num_points);
        mesh.reserve_vertex_normals(num_points);
        mesh.reserve_vertex_tangents(num_points);
        mesh.reserve_tex_coords(num_points);

        const Transformf& transform = surface.transform();

        for (size_t j = 0; j < resolution_v; ++j)
        {
            const float v = fit<size_t, float>(j, 0, resolution_v - 1, 0.0f, 1.0f);

            for (size_t i = 0; i < resolution_u; ++i)
            {
                const float u = fit<size_t, float>(i, 0, resolution_u - 1, 0.0f, 1.0f);

                mesh.push_tex_coords(GVector2(u, v));

                const GVector3 p = surface.evaluate(u, v);
                mesh.push_vertex(transform.point_to_parent(p));

                GVector3 dpdu = surface.evaluate_du(u, v);
                mesh.push_vertex_tangent(transform.vector_to_parent(dpdu));

                const GVector3 n = surface.evaluate_normal(u, v);
                mesh.push_vertex_normal(transform.normal_to_parent(n));
            }
        }
    }

    size_t convert_to_index(const size_t resolution_u, const size_t i, const size_t j)
    {
        return resolution_u * j + i;
    }

    void create_triangles(
        MeshObject&     mesh,
        const size_t    resolution_u,
        const size_t    resolution_v)
    {
        mesh.reserve_triangles(2 * (resolution_u - 1) * (resolution_v - 1));
        for (size_t j = 0, je = resolution_v - 1; j < je; ++j)
        {
            for (size_t i = 0, ie = resolution_u - 1; i < ie; ++i)
            {
                const size_t v0 = convert_to_index(resolution_u, i    , j);
                const size_t v1 = convert_to_index(resolution_u, i + 1, j);
                const size_t v2 = convert_to_index(resolution_u, i + 1, j + 1);
                const size_t v3 = convert_to_index(resolution_u, i    , j + 1);

                mesh.push_triangle(
                    Triangle(v3, v1, v0, v3, v1, v0, v3, v1, v0, 0));
                mesh.push_triangle(
                    Triangle(v3, v2, v1, v3, v2, v1, v3, v2, v1, 0));
            }
        }
    }

    template <typename ParametricSurface>
    void create_primitive(
        MeshObject&                 mesh,
        const ParametricSurface&    surface,
        const size_t                resolution_u,
        const size_t                resolution_v)
    {
        create_vertices(mesh, surface, resolution_u, resolution_v);
        create_triangles(mesh, resolution_u, resolution_v);
        mesh.push_material_slot("default");
    }
}

auto_release_ptr<MeshObject> create_primitive_mesh(const char* name, const ParamArray& params)
{
    auto_release_ptr<MeshObject> mesh = MeshObjectFactory::create(name, params);
    const size_t resolution_u  = params.get_optional<size_t>("resolution_u", 32);
    const size_t resolution_v = params.get_optional<size_t>("resolution_v", 32);

    if (resolution_u < 2 || resolution_v < 2)
    {
        RENDERER_LOG_ERROR("resolution must be greater than one.");
        return auto_release_ptr<MeshObject>();
    }

    const char* primitive_type = params.get("primitive");

    if (strcmp(primitive_type, "sphere") == 0)
    {
        const float radius = params.get_optional<float>("radius", 1.0);
        if (radius <= 0.0)
        {
            RENDERER_LOG_ERROR("radius must be greater than zero.");
            return auto_release_ptr<MeshObject>();
        }
        const ParametricSphere sphere(radius, resolution_u);
        create_primitive(*mesh, sphere, resolution_u, resolution_v);
    }
    else if (strcmp(primitive_type, "disk") == 0)
    {
        const float radius = params.get_optional<float>("disk", 1.0);
        if (radius <= 0.0)
        {
            RENDERER_LOG_ERROR("radius must be greater than zero.");
            return auto_release_ptr<MeshObject>();
        }
        const ParametricDisk disk(radius);
        create_primitive(*mesh, disk, resolution_u, resolution_v);
    }
    else if (strcmp(primitive_type, "grid") == 0)
    {
        const float width = params.get_optional<float>("width", 1.0);
        const float height = params.get_optional<float>("height", 1.0);
        if (width <= 0.0 || height <= 0.0)
        {
            RENDERER_LOG_ERROR("width and height must be greater than zero.");
            return auto_release_ptr<MeshObject>();
        }
        const ParametricGrid grid(width, height);
        create_primitive(*mesh, grid, resolution_u, resolution_v);
    }
    else if (strcmp(primitive_type, "torus") == 0)
    {
        const float major_radius = params.get_optional<float>("major_radius", 1.0f);
        const float minor_radius = params.get_optional<float>("minor_radius", 0.2f);
        if (major_radius <= 0.0f || major_radius <= 0.0f)
        {
            RENDERER_LOG_ERROR("torus radii must be greater than zero.");
            return auto_release_ptr<MeshObject>();
        }
        const ParametricTorus torus(major_radius, minor_radius, resolution_u, resolution_v);
        create_primitive(*mesh, torus, resolution_u, resolution_v);
    }
    else
    {
        RENDERER_LOG_ERROR("unknown primitive type.");
        return auto_release_ptr<MeshObject>();
    }

    return mesh;
}

}   // namespace renderer
