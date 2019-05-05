
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "lighttypes.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/lightsample.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/distance.h"
#include "foundation/math/fp.h"
#include "foundation/math/intersection/rayparallelogram.h"
#include "foundation/math/intersection/raytrianglemt.h"
#include "foundation/math/sampling/mappings.h"

using namespace foundation;

namespace renderer
{

//
// EmittingShape class implementation.
//
// References:
//
//   [1] Monte Carlo Techniques for Direct Lighting Calculations.
//       http://www.cs.virginia.edu/~jdl/bib/globillum/mis/shirley96.pdf
//
//   [2] Stratified Sampling of Spherical Triangles.
//       https://www.graphics.cornell.edu/pubs/1995/Arv95c.pdf
//
//   [3] An Area-Preserving Parametrization for Spherical Rectangles.
//       https://www.arnoldrenderer.com/research/egsr2013_spherical_rectangle.pdf
//

namespace
{
    template <typename Shape>
    double signed_plane_distance(const Shape& shape, const Vector3d& p)
    {
        return dot(p, shape.m_geometric_normal) + shape.m_plane_dist;
    }
}

EmittingShape EmittingShape::create_triangle_shape(
    const AssemblyInstance*     assembly_instance,
    const size_t                object_instance_index,
    const size_t                primitive_index,
    const Material*             material,
    const double                area,
    const Vector3d&             v0,
    const Vector3d&             v1,
    const Vector3d&             v2,
    const Vector3d&             n0,
    const Vector3d&             n1,
    const Vector3d&             n2,
    const Vector3d&             geometric_normal)
{
    EmittingShape shape(
        TriangleShape,
        assembly_instance,
        object_instance_index,
        primitive_index,
        material);

    shape.m_geom.m_triangle.m_v0 = v0;
    shape.m_geom.m_triangle.m_v1 = v1;
    shape.m_geom.m_triangle.m_v2 = v2;
    shape.m_geom.m_triangle.m_n0 = n0;
    shape.m_geom.m_triangle.m_n1 = n1;
    shape.m_geom.m_triangle.m_n2 = n2;
    shape.m_geom.m_triangle.m_geometric_normal = geometric_normal;
    shape.m_geom.m_triangle.m_plane_dist = -dot(v0, geometric_normal);

    shape.m_bbox.invalidate();
    shape.m_bbox.insert(v0);
    shape.m_bbox.insert(v1);
    shape.m_bbox.insert(v2);

    shape.m_centroid = (v0 + v1 + v2) * (1.0 / 3.0);

    shape.m_area = static_cast<float>(area);

    if (shape.m_area != 0.0f)
        shape.m_rcp_area = 1.0f / shape.m_area;
    else
        shape.m_rcp_area = FP<float>().snan();


    return shape;
}

EmittingShape EmittingShape::create_rectangle_shape(
    const AssemblyInstance*     assembly_instance,
    const size_t                object_instance_index,
    const Material*             material,
    const double                area,
    const Vector3d&             o,
    const Vector3d&             x,
    const Vector3d&             y,
    const Vector3d&             n)
{
    EmittingShape shape(
        RectangleShape,
        assembly_instance,
        object_instance_index,
        0,
        material);

    shape.m_geom.m_rectangle.m_origin = o;
    shape.m_geom.m_rectangle.m_x = x;
    shape.m_geom.m_rectangle.m_y = y;
    shape.m_geom.m_rectangle.m_width = norm(x);
    shape.m_geom.m_rectangle.m_height = norm(y);
    shape.m_geom.m_rectangle.m_geometric_normal = n;
    shape.m_geom.m_rectangle.m_plane_dist = -dot(o, n);

    shape.m_area = static_cast<float>(area);

    if (shape.m_area != 0.0f)
        shape.m_rcp_area = 1.0f / shape.m_area;
    else
        shape.m_rcp_area = FP<float>().snan();

    return shape;
}

EmittingShape EmittingShape::create_sphere_shape(
    const AssemblyInstance*     assembly_instance,
    const size_t                object_instance_index,
    const Material*             material,
    const double                area,
    const Vector3d&             center,
    const double                radius)
{
    EmittingShape shape(
        SphereShape,
        assembly_instance,
        object_instance_index,
        0,
        material);

    shape.m_geom.m_sphere.m_center = center;
    shape.m_geom.m_sphere.m_radius = radius;

    shape.m_area = static_cast<float>(area);

    if (shape.m_area != 0.0f)
        shape.m_rcp_area = 1.0f / shape.m_area;
    else
        shape.m_rcp_area = FP<float>().snan();

    return shape;
}

EmittingShape EmittingShape::create_disk_shape(
    const AssemblyInstance*     assembly_instance,
    const size_t                object_instance_index,
    const Material*             material,
    const double                area,
    const Vector3d&             c,
    const double                r,
    const Vector3d&             n,
    const Vector3d&             x,
    const Vector3d&             y)
{
    EmittingShape shape(
        DiskShape,
        assembly_instance,
        object_instance_index,
        0,
        material);

    shape.m_geom.m_disk.m_center = c;
    shape.m_geom.m_disk.m_radius = r;
    shape.m_geom.m_disk.m_geometric_normal = n;
    shape.m_geom.m_disk.m_x = x;
    shape.m_geom.m_disk.m_y = y;

    shape.m_area = static_cast<float>(area);

    if (shape.m_area != 0.0f)
        shape.m_rcp_area = 1.0f / shape.m_area;
    else
        shape.m_rcp_area = FP<float>().snan();

    return shape;
}

EmittingShape::EmittingShape(
    const ShapeType         shape_type,
    const AssemblyInstance* assembly_instance,
    const size_t            object_instance_index,
    const size_t            primitive_index,
    const Material*         material)
{
    m_assembly_instance_and_type.set(
        assembly_instance,
        static_cast<foundation::uint16>(shape_type));

    m_object_instance_index = object_instance_index;
    m_primitive_index = primitive_index;
    m_material = material;
    m_shape_prob = 0.0f;
    m_average_flux = 1.0f;
}

void EmittingShape::sample_uniform(
    const Vector2f&         s,
    const float             shape_prob,
    LightSample&            light_sample) const
{
    // Store a pointer to the emitting shape.
    light_sample.m_shape = this;

    const auto shape_type = get_shape_type();

    if (shape_type == TriangleShape)
    {
        // Uniformly sample the surface of the shape.
        const Vector3d bary = sample_triangle_uniform(Vector2d(s));

        // Set the parametric coordinates.
        light_sample.m_param_coords[0] = static_cast<float>(bary[0]);
        light_sample.m_param_coords[1] = static_cast<float>(bary[1]);

        // Compute the world space position of the sample.
        light_sample.m_point =
              bary[0] * m_geom.m_triangle.m_v0
            + bary[1] * m_geom.m_triangle.m_v1
            + bary[2] * m_geom.m_triangle.m_v2;

        // Compute the world space shading normal at the position of the sample.
        light_sample.m_shading_normal =
              bary[0] * m_geom.m_triangle.m_n0
            + bary[1] * m_geom.m_triangle.m_n1
            + bary[2] * m_geom.m_triangle.m_n2;
        light_sample.m_shading_normal = normalize(light_sample.m_shading_normal);

        // Set the world space geometric normal.
        light_sample.m_geometric_normal = m_geom.m_triangle.m_geometric_normal;
    }
    else if (shape_type == RectangleShape)
    {
        // Set the parametric coordinates.
        light_sample.m_param_coords = s;

        light_sample.m_point =
            m_geom.m_rectangle.m_origin +
            static_cast<double>(s[0]) * m_geom.m_rectangle.m_x +
            static_cast<double>(s[1]) * m_geom.m_rectangle.m_y;

        // Set the world space shading and geometric normals.
        light_sample.m_shading_normal = m_geom.m_rectangle.m_geometric_normal;
        light_sample.m_geometric_normal = m_geom.m_rectangle.m_geometric_normal;
    }
    else if (shape_type == SphereShape)
    {
        // Set the parametric coordinates.
        light_sample.m_param_coords = s;

        Vector3d n(sample_sphere_uniform(s));

        // Set the world space shading and geometric normals.
        light_sample.m_shading_normal = n;
        light_sample.m_geometric_normal = n;

        // Compute the world space position of the sample.
        light_sample.m_point = m_geom.m_sphere.m_center + n * m_geom.m_sphere.m_radius;
    }
    else if (shape_type == DiskShape)
    {
        const Vector2f param_coords = sample_disk_uniform(s);

        // Compute the world space position of the sample.
        Vector3d p =
            m_geom.m_disk.m_center +
            static_cast<double>(param_coords[0]) * m_geom.m_disk.m_x +
            static_cast<double>(param_coords[1]) * m_geom.m_disk.m_y;

        light_sample.m_point = p;

        // Set the parametric coordinates.
        light_sample.m_param_coords = param_coords;

        // Set the world space shading and geometric normals.
        light_sample.m_shading_normal = m_geom.m_disk.m_geometric_normal;
        light_sample.m_geometric_normal = m_geom.m_disk.m_geometric_normal;
    }
    else
    {
        assert(false && "Unknown emitter shape type");
    }

    // Compute the probability density of this sample.
    light_sample.m_probability = shape_prob * get_rcp_area();
}

float EmittingShape::evaluate_pdf_uniform() const
{
    return get_shape_prob() * get_rcp_area();
}

void EmittingShape::make_shading_point(
    ShadingPoint&           shading_point,
    const Vector3d&         point,
    const Vector3d&         direction,
    const Vector2f&         param_coords,
    const Intersector&      intersector) const
{
    const ShadingRay ray(
        point,
        direction,
        0.0,
        0.0,
        ShadingRay::Time(),
        VisibilityFlags::CameraRay, 0);

    const auto shape_type = get_shape_type();

    if (shape_type == TriangleShape)
    {
        intersector.make_triangle_shading_point(
            shading_point,
            ray,
            param_coords,
            get_assembly_instance(),
            get_assembly_instance()->transform_sequence().get_earliest_transform(),
            get_object_instance_index(),
            get_primitive_index(),
            m_shape_support_plane);
    }
    else if (shape_type == RectangleShape)
    {
        const Vector3d p =
            m_geom.m_rectangle.m_origin +
            static_cast<double>(param_coords[0]) * m_geom.m_rectangle.m_x +
            static_cast<double>(param_coords[1]) * m_geom.m_rectangle.m_y;

        intersector.make_procedural_surface_shading_point(
            shading_point,
            ray,
            param_coords,
            get_assembly_instance(),
            get_assembly_instance()->transform_sequence().get_earliest_transform(),
            get_object_instance_index(),
            get_primitive_index(),
            p,
            m_geom.m_rectangle.m_geometric_normal,
            m_geom.m_rectangle.m_x,
            cross(m_geom.m_rectangle.m_x, m_geom.m_rectangle.m_geometric_normal));
    }
    else if (shape_type == SphereShape)
    {
        const double theta = static_cast<double>(param_coords[0]);
        const double phi = static_cast<double>(param_coords[1]);

        const Vector3d n = Vector3d::make_unit_vector(theta, phi);
        const Vector3d p = m_geom.m_sphere.m_center + m_geom.m_sphere.m_radius * n;

        const Vector3d dpdu(-TwoPi<double>() * n.y, TwoPi<double>() + n.x, 0.0);
        const Vector3d dpdv = cross(dpdu, n);

        intersector.make_procedural_surface_shading_point(
            shading_point,
            ray,
            param_coords,
            get_assembly_instance(),
            get_assembly_instance()->transform_sequence().get_earliest_transform(),
            get_object_instance_index(),
            get_primitive_index(),
            p,
            n,
            dpdu,
            dpdv);
    }
    else if (shape_type == DiskShape)
    {
        const Vector3d p =
            m_geom.m_disk.m_center +
            static_cast<double>(param_coords[0]) * m_geom.m_disk.m_x +
            static_cast<double>(param_coords[1]) * m_geom.m_disk.m_y;

        intersector.make_procedural_surface_shading_point(
            shading_point,
            ray,
            param_coords,
            get_assembly_instance(),
            get_assembly_instance()->transform_sequence().get_earliest_transform(),
            get_object_instance_index(),
            get_primitive_index(),
            p,
            m_geom.m_disk.m_geometric_normal,
            m_geom.m_disk.m_x,
            cross(m_geom.m_disk.m_x, m_geom.m_disk.m_geometric_normal));
    }
    else
    {
        assert(false && "Unknown emitter shape type");
    }
}

void EmittingShape::estimate_flux()
{
    // todo:
    /*
    if (constant EDF)
        return EDF->radiance();

    // Varying EDF or OSL emission case.
    for i = 0..N:
    {
        s = random2d()
        make_shading_point(shading_point, p, d, s, intersector);
        radiance += eval EDF or ShaderGroup
    }

    radiance /= N;
    return radiance;
    */

    m_average_flux = 1.0f;
    m_max_flux = 1.0f;
}

}   // namespace renderer
