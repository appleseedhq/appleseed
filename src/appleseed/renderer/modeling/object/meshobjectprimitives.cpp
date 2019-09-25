
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Ramon Blanquer, The appleseedhq Organization
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
        static std::unique_ptr<ParametricGrid> create(const size_t res_u, const size_t res_v, const ParamArray& params)
        {
            const float width = params.get_optional<float>("width", 1.0f);
            const float height = params.get_optional<float>("height", 1.0f);

            if (width <= 0.0f || height <= 0.0f)
            {
                RENDERER_LOG_ERROR("width and height must be greater than zero.");
                return std::unique_ptr<ParametricGrid>();
            }

            return std::unique_ptr<ParametricGrid>(new ParametricGrid(width, height));
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

        ParametricGrid(const float width, const float height)
          : m_width(width)
          , m_height(height)
          , m_transform(Matrix4f::make_rotation_x(-HalfPi<float>()))
        {
        }
    };


    //
    // Disk.
    //

    class ParametricDisk
    {
      public:
        static std::unique_ptr<ParametricDisk> create(const size_t res_u, const size_t res_v, const ParamArray& params)
        {
            const float radius = params.get_optional<float>("radius", 1.0f);

            if (radius <= 0.0f)
            {
                RENDERER_LOG_ERROR("radius must be greater than zero.");
                return std::unique_ptr<ParametricDisk>();
            }

            return std::unique_ptr<ParametricDisk>(new ParametricDisk(radius));
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
            return GVector3(r * std::cos(theta), r * std::sin(theta), 0.0f);
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

        explicit ParametricDisk(const float radius)
          : m_radius(radius)
          , m_transform(Matrix4f::make_rotation_x(-HalfPi<float>()))
        {
        }
    };


    //
    // Sphere.
    //

    class ParametricSphere
    {
      public:
        static std::unique_ptr<ParametricSphere> create(const size_t res_u, const size_t res_v, const ParamArray& params)
        {
            const float radius = params.get_optional<float>("radius", 1.0f);

            if (radius <= 0.0f)
            {
                RENDERER_LOG_ERROR("radius must be greater than zero.");
                return std::unique_ptr<ParametricSphere>();
            }

            return std::unique_ptr<ParametricSphere>(new ParametricSphere(res_u, radius));
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

        ParametricSphere(const size_t res_u, const float radius)
          : m_radius(radius)
          , m_h(1.0f / (res_u * 4.0f))
          , m_transform(Matrix4f::make_rotation_x(Pi<float>()))
        {
        }
    };


    //
    // Torus.
    //

    class ParametricTorus
    {
      public:
        static std::unique_ptr<ParametricTorus> create(const size_t res_u, const size_t res_v, const ParamArray& params)
        {
            const float major_radius = params.get_optional<float>("major_radius", 1.0f);
            const float minor_radius = params.get_optional<float>("minor_radius", 0.2f);

            if (major_radius <= 0.0f || minor_radius <= 0.0f)
            {
                RENDERER_LOG_ERROR("torus radii must be greater than zero.");
                return std::unique_ptr<ParametricTorus>();
            }

            return std::unique_ptr<ParametricTorus>(new ParametricTorus(res_u, res_v, major_radius, minor_radius));
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
            const float cos_phi = std::cos(phi);
            const float r = m_circle_radius + m_tube_radius * cos_phi;

            return GVector3(
                r * std::cos(theta),
                r * std::sin(theta),
                m_tube_radius * std::sin(phi));
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

        ParametricTorus(
            const size_t    res_u,
            const size_t    res_v,
            const float     major_radius,
            const float     minor_radius)
          : m_circle_radius(0.5f * (major_radius + minor_radius))
          , m_tube_radius(0.5f * (major_radius - minor_radius))
          , m_hs(1.0f / (res_u * 4.0f))
          , m_ht(1.0f / (res_v * 4.0f))
          , m_transform(Matrix4f::make_rotation_x(-HalfPi<float>()))
        {
        }
    };


    //
    // Parametric surface generation.
    //

    template <typename ParametricSurface>
    void create_vertices(
        MeshObject&                 mesh,
        const ParametricSurface&    surface,
        const size_t                res_u,
        const size_t                res_v)
    {
        const size_t vertex_count = (res_u + 1) * (res_v + 1);

        mesh.reserve_vertices(vertex_count);
        mesh.reserve_vertex_normals(vertex_count);
        mesh.reserve_vertex_tangents(vertex_count);
        mesh.reserve_tex_coords(vertex_count);

        const Transformf& transform = surface.transform();

        for (size_t j = 0; j <= res_v; ++j)
        {
            const float t = fit<size_t, float>(j, 0, res_v, 0.0f, 1.0f);

            for (size_t i = 0; i <= res_u; ++i)
            {
                const float s = fit<size_t, float>(i, 0, res_u, 0.0f, 1.0f);

                mesh.push_vertex(transform.point_to_parent(surface.evaluate(s, t)));
                mesh.push_vertex_normal(transform.normal_to_parent(surface.evaluate_normal(s, t)));
                mesh.push_vertex_tangent(transform.vector_to_parent(surface.evaluate_tangent(s, t)));
                mesh.push_tex_coords(surface.evaluate_tex_coords(s, t));
            }
        }
    }

    size_t convert_to_index(const size_t res_u, const size_t i, const size_t j)
    {
        return (res_u + 1) * j + i;
    }

    void create_triangles(
        MeshObject&                 mesh,
        const size_t                res_u,
        const size_t                res_v)
    {
        mesh.reserve_triangles(2 * res_u * res_v);

        for (size_t j = 0; j < res_v; ++j)
        {
            for (size_t i = 0; i < res_u; ++i)
            {
                const size_t v0 = convert_to_index(res_u, i    , j);
                const size_t v1 = convert_to_index(res_u, i + 1, j);
                const size_t v2 = convert_to_index(res_u, i + 1, j + 1);
                const size_t v3 = convert_to_index(res_u, i    , j + 1);

                mesh.push_triangle(Triangle(v3, v1, v0, v3, v1, v0, v3, v1, v0, 0));
                mesh.push_triangle(Triangle(v3, v2, v1, v3, v2, v1, v3, v2, v1, 0));
            }
        }
    }

    template <typename ParametricSurface>
    auto_release_ptr<MeshObject> create_parametric_surface(const char* name, const ParamArray& params)
    {
        const size_t res_u  = params.get_optional<size_t>("resolution_u", 32);
        const size_t res_v = params.get_optional<size_t>("resolution_v", 32);

        if (res_u < 1 || res_v < 1)
        {
            RENDERER_LOG_ERROR("resolution must be greater than zero.");
            return auto_release_ptr<MeshObject>();
        }

        std::unique_ptr<ParametricSurface> surface = ParametricSurface::create(res_u, res_v, params);

        if (surface.get() == nullptr)
            return auto_release_ptr<MeshObject>();

        auto_release_ptr<MeshObject> mesh(MeshObjectFactory().create(name, params));

        create_vertices(mesh.ref(), *surface.get(), res_u, res_v);
        create_triangles(mesh.ref(), res_u, res_v);
        mesh->push_material_slot("default");

        return mesh;
    }


    //
    // Cube.
    //

    auto_release_ptr<MeshObject> create_cube(const char* name, const ParamArray& params)
    {
        auto_release_ptr<MeshObject> mesh(MeshObjectFactory().create(name, params));

        mesh->reserve_vertices(8);
        mesh->push_vertex(GVector3(-1.0f, -1.0f,  1.0f));
        mesh->push_vertex(GVector3( 1.0f, -1.0f,  1.0f));
        mesh->push_vertex(GVector3(-1.0f,  1.0f,  1.0f));
        mesh->push_vertex(GVector3( 1.0f,  1.0f,  1.0f));
        mesh->push_vertex(GVector3(-1.0f,  1.0f, -1.0f));
        mesh->push_vertex(GVector3( 1.0f,  1.0f, -1.0f));
        mesh->push_vertex(GVector3(-1.0f, -1.0f, -1.0f));
        mesh->push_vertex(GVector3( 1.0f, -1.0f, -1.0f));

        mesh->reserve_vertex_normals(6);
        mesh->push_vertex_normal(GVector3( 0.0f,  0.0f,  1.0f));
        mesh->push_vertex_normal(GVector3( 0.0f,  1.0f,  0.0f));
        mesh->push_vertex_normal(GVector3( 0.0f,  0.0f, -1.0f));
        mesh->push_vertex_normal(GVector3( 0.0f, -1.0f,  0.0f));
        mesh->push_vertex_normal(GVector3( 1.0f,  0.0f,  0.0f));
        mesh->push_vertex_normal(GVector3(-1.0f,  0.0f,  0.0f));

        mesh->reserve_tex_coords(14);
        mesh->push_tex_coords(GVector2(0.375f, 0.000f));
        mesh->push_tex_coords(GVector2(0.625f, 0.000f));
        mesh->push_tex_coords(GVector2(0.375f, 0.250f));
        mesh->push_tex_coords(GVector2(0.625f, 0.250f));
        mesh->push_tex_coords(GVector2(0.375f, 0.500f));
        mesh->push_tex_coords(GVector2(0.625f, 0.500f));
        mesh->push_tex_coords(GVector2(0.375f, 0.750f));
        mesh->push_tex_coords(GVector2(0.625f, 0.750f));
        mesh->push_tex_coords(GVector2(0.375f, 1.000f));
        mesh->push_tex_coords(GVector2(0.625f, 1.000f));
        mesh->push_tex_coords(GVector2(0.875f, 0.000f));
        mesh->push_tex_coords(GVector2(0.875f, 0.250f));
        mesh->push_tex_coords(GVector2(0.125f, 0.000f));
        mesh->push_tex_coords(GVector2(0.125f, 0.250f));

        mesh->reserve_triangles(12);
        mesh->push_triangle(Triangle(0, 1, 2,   0, 0, 0,    0, 1,   2,   0));
        mesh->push_triangle(Triangle(2, 1, 3,   0, 0, 0,    2, 1,   3,   0));
        mesh->push_triangle(Triangle(2, 3, 4,   1, 1, 1,    2, 3,   4,   0));
        mesh->push_triangle(Triangle(4, 3, 5,   1, 1, 1,    4, 3,   5,   0));
        mesh->push_triangle(Triangle(4, 5, 6,   2, 2, 2,    4, 5,   6,   0));
        mesh->push_triangle(Triangle(6, 5, 7,   2, 2, 2,    6, 5,   7,   0));
        mesh->push_triangle(Triangle(6, 7, 0,   3, 3, 3,    6, 7,   8,   0));
        mesh->push_triangle(Triangle(0, 7, 1,   3, 3, 3,    8, 7,   9,   0));
        mesh->push_triangle(Triangle(1, 7, 3,   4, 4, 4,    1, 10,  3,   0));
        mesh->push_triangle(Triangle(3, 7, 5,   4, 4, 4,    3, 10, 11,   0));
        mesh->push_triangle(Triangle(6, 0, 4,   5, 5, 5,   12,  0, 13,   0));
        mesh->push_triangle(Triangle(4, 0, 2,   5, 5, 5,   13,  0,  2,   0));

        mesh->push_material_slot("default");

        return mesh;
    }
}


//
// Factory function.
//

auto_release_ptr<MeshObject> create_primitive_mesh(const char* name, const ParamArray& params)
{
    const char* primitive_type = params.get("primitive");

    // Parametric surfaces.

    if (strcmp(primitive_type, "grid") == 0)
        return create_parametric_surface<ParametricGrid>(name, params);

    if (strcmp(primitive_type, "disk") == 0)
        return create_parametric_surface<ParametricDisk>(name, params);

    if (strcmp(primitive_type, "sphere") == 0)
        return create_parametric_surface<ParametricSphere>(name, params);

    if (strcmp(primitive_type, "torus") == 0)
        return create_parametric_surface<ParametricTorus>(name, params);

    // Other, non-parametric primitives.

    if (strcmp(primitive_type, "cube") == 0)
        return create_cube(name, params);

    RENDERER_LOG_ERROR("unknown primitive type: %s", primitive_type);
    return auto_release_ptr<MeshObject>();
}

}   // namespace renderer
