
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
#include "foundation/math/matrix.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cmath>
#include <cstddef>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Grid.
    //

    class ParametricGrid
    {
      public:
        ParametricGrid(const float width, const float height)
          : m_width(width)
          , m_height(height)
          , m_transform(Matrix4f::make_rotation_x(-HalfPi<float>()))
        {
        }

        const Transformf& transform() const
        {
            return m_transform;
        }

        GVector3 evaluate(const float s, const float t) const
        {
            //
            // Before transform:
            //
            // -width/2     +width/2
            //
            //           Y
            //           ^     u=1
            //           |     v=1
            //    +------+------+    +height/2
            //    |      |      |
            //    |      |      |
            //    |      o------+--> X
            //    |             |
            //    |             |
            //    +-------------+    -height/2
            //   u=0
            //   v=0
            //
            // After transform:
            //
            // -width/2     +width/2
            //
            //                 u=1
            //                 v=1
            //    +-------------+    +height/2
            //    |             |
            //    |             |
            //    |      o------+--> X
            //    |      |      |
            //    |      |      |
            //    +------+------+    -height/2
            //   u=0     |
            //   v=0     v
            //           Z
            //

            return GVector3(
                (s - 0.5f) * m_width,
                (t - 0.5f) * m_height,
                0.0f);
        }

        GVector2 evaluate_tex_coords(const float s, const float t) const
        {
            return GVector2(s, t);
        }

        GVector3 evaluate_tangent(const float s, const float t) const
        {
            return GVector3(1.0f, 0.0f, 0.0f);
        }

        GVector3 evaluate_normal(const float s, const float t) const
        {
            return GVector3(0.0f, 0.0f, 1.0f);
        }

      private:
        const float         m_width;
        const float         m_height;
        const Transformf    m_transform;
    };


    //
    // Disk.
    //

    class ParametricDisk
    {
      public:
        explicit ParametricDisk(const float radius)
          : m_radius(radius)
          , m_transform(Matrix4f::make_rotation_x(-HalfPi<float>()))
        {
        }

        const Transformf& transform() const
        {
            return m_transform;
        }

        GVector3 evaluate(const float s, const float t) const
        {
            //
            // Before transform:
            //
            //        Y
            //        ^
            //        |
            //      * | *
            //   *    |    *
            //  *     o-----+--> X
            //  *           *
            //   *         *
            //      *   *
            //
            // After transform:
            //
            //      *   *
            //   *         *
            //  *     o-----+--> X
            //  *     |     *
            //   *    |    *
            //      * | *
            //        |
            //        v
            //        Z
            //

            const float r = m_radius * s;
            const float theta = TwoPi<float>() * t;
            return GVector3(r * cos(theta), r * sin(theta), 0.0f);
        }

        GVector2 evaluate_tex_coords(const float s, const float t) const
        {
            const GVector3 p = evaluate(s, t);
            return GVector2(
                fit(p.x, -m_radius, m_radius, 0.0f, 1.0f),
                fit(p.y, -m_radius, m_radius, 0.0f, 1.0f));
        }

        GVector3 evaluate_tangent(const float s, const float t) const
        {
            return GVector3(1.0f, 0.0f, 0.0f);
        }

        GVector3 evaluate_normal(const float s, const float t) const
        {
            return GVector3(0.0f, 0.0f, 1.0f);
        }

      private:
        const float         m_radius;
        const Transformf    m_transform;
    };


    //
    // Sphere.
    //

    class ParametricSphere
    {
      public:
        ParametricSphere(const float radius, const size_t resolution_u)
          : m_radius(radius)
          , m_h(1.0f / (resolution_u * 4.0f))
          , m_transform(Matrix4f::make_rotation_x(Pi<float>()))
        {
        }

        const Transformf& transform() const
        {
            return m_transform;
        }

        GVector3 evaluate(const float s, const float t) const
        {
            const float theta = Pi<float>() * t;
            const float phi = TwoPi<float>() * s;
            return m_radius * GVector3::make_unit_vector(theta, phi);
        }

        GVector2 evaluate_tex_coords(const float s, const float t) const
        {
            return GVector2(s, t);
        }

        GVector3 evaluate_tangent(const float s, const float t) const
        {
            // Compute U tangent using forward differencing.
            return safe_normalize(
                evaluate(s + m_h, t) - evaluate(s, t),
                GVector3(1.0f, 0.0f, 0.0f));
        }

        GVector3 evaluate_normal(const float s, const float t) const
        {
            return normalize(evaluate(s, t));
        }

      private:
        const float         m_radius;
        const float         m_h;
        const Transformf    m_transform;
    };


    //
    // Torus.
    //

    class ParametricTorus
    {
      public:
        ParametricTorus(
            const float     major_radius,
            const float     minor_radius,
            const size_t    resolution_u,
            const size_t    resolution_v)
          : m_circle_radius(0.5f * (major_radius + minor_radius))
          , m_tube_radius(0.5f * (major_radius - minor_radius))
          , m_hs(1.0f / (resolution_u * 4.0f))
          , m_ht(1.0f / (resolution_v * 4.0f))
          , m_transform(Matrix4f::make_rotation_x(-HalfPi<float>()))
        {
        }

        const Transformf& transform() const
        {
            return m_transform;
        }

        GVector3 evaluate(const float s, const float t) const
        {
            // Reference: http://mathworld.wolfram.com/Torus.html

            const float theta = TwoPi<float>() * s;
            const float phi = TwoPi<float>() * t;
            const float cos_phi = cos(phi);
            const float r = m_circle_radius + m_tube_radius * cos_phi;

            return GVector3(
                r * cos(theta),
                r * sin(theta),
                m_tube_radius * sin(phi));
        }

        GVector2 evaluate_tex_coords(const float s, const float t) const
        {
            return GVector2(s, t);
        }

        GVector3 evaluate_tangent(const float s, const float t) const
        {
            // Compute U tangent using forward differencing.
            const GVector3 p = evaluate(s, t);
            const GVector3 dpdu = evaluate(s + m_hs, t) - p;
            return normalize(dpdu);
        }

        GVector3 evaluate_normal(const float s, const float t) const
        {
            // Compute tangents using forward differencing.
            const GVector3 p = evaluate(s, t);
            const GVector3 dpds = evaluate(s + m_hs, t) - p;
            const GVector3 dpdt = evaluate(s, t + m_ht) - p;

            // Compute normal.
            return normalize(cross(dpds, dpdt));
        }

      private:
        const float         m_circle_radius;
        const float         m_tube_radius;
        const float         m_hs, m_ht;
        const Transformf    m_transform;
    };


    //
    // Mesh generation.
    //

    template <typename ParametricSurface>
    void create_vertices(
        MeshObject&                 mesh,
        const ParametricSurface&    surface,
        const size_t                resolution_u,
        const size_t                resolution_v)
    {
        const size_t vertex_count = (resolution_u + 1) * (resolution_v + 1);

        mesh.reserve_vertices(vertex_count);
        mesh.reserve_vertex_normals(vertex_count);
        mesh.reserve_vertex_tangents(vertex_count);
        mesh.reserve_tex_coords(vertex_count);

        const Transformf& transform = surface.transform();

        for (size_t j = 0; j <= resolution_v; ++j)
        {
            const float t = fit<size_t, float>(j, 0, resolution_v, 0.0f, 1.0f);

            for (size_t i = 0; i <= resolution_u; ++i)
            {
                const float s = fit<size_t, float>(i, 0, resolution_u, 0.0f, 1.0f);

                mesh.push_vertex(transform.point_to_parent(surface.evaluate(s, t)));
                mesh.push_vertex_normal(transform.normal_to_parent(surface.evaluate_normal(s, t)));
                mesh.push_vertex_tangent(transform.vector_to_parent(surface.evaluate_tangent(s, t)));
                mesh.push_tex_coords(surface.evaluate_tex_coords(s, t));
            }
        }
    }

    size_t convert_to_index(const size_t resolution_u, const size_t i, const size_t j)
    {
        return (resolution_u + 1) * j + i;
    }

    void create_triangles(
        MeshObject&                 mesh,
        const size_t                resolution_u,
        const size_t                resolution_v)
    {
        mesh.reserve_triangles(2 * (resolution_u - 1) * (resolution_v - 1));

        for (size_t j = 0; j < resolution_v; ++j)
        {
            for (size_t i = 0; i < resolution_u; ++i)
            {
                const size_t v0 = convert_to_index(resolution_u, i    , j);
                const size_t v1 = convert_to_index(resolution_u, i + 1, j);
                const size_t v2 = convert_to_index(resolution_u, i + 1, j + 1);
                const size_t v3 = convert_to_index(resolution_u, i    , j + 1);

                mesh.push_triangle(Triangle(v3, v1, v0, v3, v1, v0, v3, v1, v0, 0));
                mesh.push_triangle(Triangle(v3, v2, v1, v3, v2, v1, v3, v2, v1, 0));
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


//
// Factory function.
//

auto_release_ptr<MeshObject> create_primitive_mesh(const char* name, const ParamArray& params)
{
    auto_release_ptr<MeshObject> mesh = MeshObjectFactory::create(name, params);
    const size_t resolution_u  = params.get_optional<size_t>("resolution_u", 32);
    const size_t resolution_v = params.get_optional<size_t>("resolution_v", 32);

    if (resolution_u < 1 || resolution_v < 1)
    {
        RENDERER_LOG_ERROR("resolution must be greater than zero.");
        return auto_release_ptr<MeshObject>();
    }

    const char* primitive_type = params.get("primitive");

    if (strcmp(primitive_type, "sphere") == 0)
    {
        const float radius = params.get_optional<float>("radius", 1.0f);
        if (radius <= 0.0f)
        {
            RENDERER_LOG_ERROR("radius must be greater than zero.");
            return auto_release_ptr<MeshObject>();
        }
        const ParametricSphere sphere(radius, resolution_u);
        create_primitive(*mesh, sphere, resolution_u, resolution_v);
    }
    else if (strcmp(primitive_type, "disk") == 0)
    {
        const float radius = params.get_optional<float>("radius", 1.0f);
        if (radius <= 0.0f)
        {
            RENDERER_LOG_ERROR("radius must be greater than zero.");
            return auto_release_ptr<MeshObject>();
        }
        const ParametricDisk disk(radius);
        create_primitive(*mesh, disk, resolution_u, resolution_v);
    }
    else if (strcmp(primitive_type, "grid") == 0)
    {
        const float width = params.get_optional<float>("width", 1.0f);
        const float height = params.get_optional<float>("height", 1.0f);
        if (width <= 0.0f || height <= 0.0f)
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
        if (major_radius <= 0.0f || minor_radius <= 0.0f)
        {
            RENDERER_LOG_ERROR("torus radii must be greater than zero.");
            return auto_release_ptr<MeshObject>();
        }
        const ParametricTorus torus(major_radius, minor_radius, resolution_u, resolution_v);
        create_primitive(*mesh, torus, resolution_u, resolution_v);
    }
    else
    {
        RENDERER_LOG_ERROR("unknown primitive type: %s", primitive_type);
        return auto_release_ptr<MeshObject>();
    }

    return mesh;
}

}   // namespace renderer
