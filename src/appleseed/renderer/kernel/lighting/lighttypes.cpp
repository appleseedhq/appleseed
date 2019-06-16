
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
#include "foundation/math/intersection/raysphere.h"
#include "foundation/math/intersection/raytrianglemt.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/sampling/sphericalcapsampler.h"

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
    const size_t                primitive_index,
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
        primitive_index,
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
    const size_t                primitive_index,
    const Material*             material,
    const double                area,
    const Vector3d&             center,
    const double                radius)
{
    EmittingShape shape(
        SphereShape,
        assembly_instance,
        object_instance_index,
        primitive_index,
        material);

    shape.m_geom.m_sphere.m_center = center;
    shape.m_geom.m_sphere.m_radius = radius;

    // todo: compute area here
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
    const size_t                primitive_index,
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
        primitive_index,
        material);

    shape.m_geom.m_disk.m_center = c;
    shape.m_geom.m_disk.m_radius = r;
    shape.m_geom.m_disk.m_geometric_normal = n;
    shape.m_geom.m_disk.m_x = x;
    shape.m_geom.m_disk.m_y = y;

    assert(c.x == 0.0);
    assert(c.y == 0.5);
    assert(c.z == 0.0);

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
        // fixme: polar coordinates
        light_sample.m_param_coords = s;

        const Vector3d n(sample_sphere_uniform(s));

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
    light_sample.m_probability = shape_prob * evaluate_pdf_uniform();
}

// todo: make this inline
float EmittingShape::evaluate_pdf_uniform() const
{
    return get_rcp_area();
}

namespace {
    struct ProjectedSphericalCap {
        /*! These three vectors define an orthonormal, positively oriented frame
            in which the spherical cap has its center at y-coordinate zero.
            \note Available in cases 1, 2, 3.*/
        Vector3d tangent;
        Vector3d bitangent;
        Vector3d normal;
        /*! The normalized vector towards the center of the spherical cap.
            \note Available in cases 1, 2 and 3 but only used by
                    computeProjectedSphericalCapSampleDensity().*/
        Vector3d normalizedCenter;
        /*! The boundary of the spherical cap is a circle. This vector stores half
            the extent of its axis-aligned bounding box along x and y. By
            convention, the x-extent is negative if and only if the center of the
            spherical cap is below the horizon.
            \note Available in cases 1, 2 and 3.*/
        Vector2d circleHalfExtent;
        /*! The reciprocal of what would go into circleHalfExtent.z if it existed.
            \note Available in case 3.*/
        double invCircleHalfExtentZ;
        /*! The x-coordinate of the center of the circle bounding the spherical
            cap. The y-coordinate is zero due to the choice of the local
            coordinate-frame.
            \note Available in cases 1 and 2.*/
        double circleCenterX;
        /*! Minus the z-coordinate of the center of the circle bounding the
            spherical cap, divided by half its extent along z.
            \note Available in case 3. Then this is positive.*/
        double centerOverHalfExtentZ;
        /*! The distance between the origin and the plane of the circle bounding
            the spherical cap.
            \note Available in cases 1, 2 and 3 but only used by
                    computeProjectedSphericalCapSampleDensity().*/
        double planeOriginDistance;
        /*! The plane of the circle bounding the spherical cap intersects the
            x-axis at this coordinate. It is <1.0f iff the cap is partially below
            the horizon.
            \note Available in cases 1, 2 and 3.*/
        double diskCutX;
        /*! 1.0f-diskCutX*diskCutX. This is the square of the y-coordinate of the
            location where the plane intersects the horizon.
            \note Available in case 3.*/
        double squaredDiskCutY;
        /*! The factor by which sample points need to be scaled along the z-axis to
            warp them into the spherical cap.
            \note Available in case 3.*/
        double scaleZ;
        /*! In cases 1 and 2, this is the constant sampled density, i.e. the
            reciprocal of the projected solid angle. In case 3, it is a constant
            factor that is pulled out of the computation of the probability density
            function. It is exactly 0.0f if the spherical cap is entirely below
            the horizon.
            \note Available in cases 1, 2 and 3.*/
        double densityFactor;
        /*! The threshold for the random number at which we need to transition from
            sampling of the cut ellipse to sampling of the cut disk. In other
            words, it is the ratio of these two areas.
            \note Available in case 2.*/
        double cutEllipseAreaRatio;
        /*! The factor needed to turn a random number into a normalized area (i.e.
            from 0 to pi) for the cut ellipse.
            \note Available in case 2.*/
        double randomToCutEllipseAreaFactor;
        /*! The factor needed to turn a random number into the area to the right of
            the point on the cut disk that is sampled.
            \note Available in cases 2 and 3.*/
        double randomToCutDiskAreaFactor;
        /*! The projected solid angle of the spherical cap. Exactly 0.0f if it is
            entirely below the horizon. Uncomment the appropriate lines in
            prepareProjectedSphericalCapSampling() if you need this. It only incurs
            an additional cost in case 3.
            \note Available in cases 1, 2 and 3 if uncommented.*/
            //double projectedSolidAngle;
    };

    inline double rsqrt(double x) {
        return 1.0 / sqrt(x);
    }

    inline double mad(double a, double b, double c) {
        return a * b + c;
    }
    /*! An element-wise multiply-add operation a*b+c.*/
    inline Vector3d mad(double a, const Vector3d& rB, const Vector3d& rC) {
        return a * rB + rC;
    }


    /*! Returns the signed area enclosed by the unit disk between x=0 and
        x=MaximalX.*/
    inline double getCutDiskArea(double maximalX) {
        return mad(sqrt(mad(-maximalX, maximalX, 1.0)), maximalX, asin(maximalX));
    }


    /*! Implements the inverse of getCutDiskArea().
       \note This implementation factors the inverse function into a part with a
             singularity and a quintic polynomial. The worst-case error is around
             4e-6.*/
    inline double getAreaDiskCut(double area) {
        double absArea = min(0.5*Pi<double>(), abs(area));
        double polynomial = mad(mad(mad(mad(mad(absArea, -0.0079908617, 0.0238255409), absArea, -0.0283903598), absArea, 0.0198450184), absArea, -0.0574433620), absArea, 0.7400712465);
        double result = 1.0 - polynomial * pow(0.5*Pi<double>() - absArea, 2.0 / 3.0);
        return (area < 0.0) ? (-result) : result;
    }


    /*!	Prepares all intermediate values to sample a spherical cap proportional to
        projected solid angle. The surface normal and sphere center are given in
        the same space, the sphere center is relative to the surface point for
        which samples are taken. The surface normal has to be normalized.
        \note For directional lights sphereCenter should be a normalized direction
                vector towards the light source and sphereRadius should be
                sin(0.5f*alpha) where alpha is the angle spanned by the light
                source.*/
    inline void prepareProjectedSphericalCapSampling(ProjectedSphericalCap& cap, Vector3d surfaceNormal, Vector3d sphereCenter, double sphereRadius) {
        double invCenterDistance = rsqrt(dot(sphereCenter, sphereCenter));
        // Construct a tangent frame where the normal is aligned with the positive 
        // z-axis and the projection of the sphere center onto the surface tangent 
        // plane is aligned with the positive x-axis.
        cap.normal = surfaceNormal;
        cap.normalizedCenter = invCenterDistance * sphereCenter;
        double normalizedCenterZ = dot(cap.normal, cap.normalizedCenter);
        cap.tangent = mad(-normalizedCenterZ, cap.normal, cap.normalizedCenter);
        double invTangentLength = rsqrt(dot(cap.tangent, cap.tangent));
        cap.tangent *= invTangentLength;
        double normalizedCenterX = dot(cap.tangent, cap.normalizedCenter);
        cap.bitangent = cross(cap.normal, cap.tangent);
        // Compute the radius of the circle that bounds the spherical cap. It 
        // agrees with half the diameter of the ellipse, which is also the extent 
        // along y.
        cap.circleHalfExtent.y = sphereRadius * invCenterDistance;
        // Compute the width of the ellipse (extent along x). Negative if the 
        // sphere center is below the horizon.
        cap.circleHalfExtent.x = cap.circleHalfExtent.y*normalizedCenterZ;
        // Compute the location of the center of the ellipse along the x-axis
        cap.planeOriginDistance = sqrt(mad(-cap.circleHalfExtent.y, cap.circleHalfExtent.y, 1.0f));
        cap.circleCenterX = cap.planeOriginDistance*normalizedCenterX;
        // Compute where the plane of the circle bounding the spherical cap 
        // intersects the x-axis
        cap.diskCutX = cap.planeOriginDistance*invTangentLength;
        // Case 1: The spherical cap is entirely above the horizon or maybe
        // case 4: The spherical cap is entirely below the horizon and thus empty
        if (cap.diskCutX >= 1.0) {
            // The projected solid angle is an ellipse
            double projectedSolidAngle = Pi<double>() * cap.circleHalfExtent.x*cap.circleHalfExtent.y;
            //cap.projectedSolidAngle=projectedSolidAngle;
            cap.densityFactor = max(0.0, 1.0 / projectedSolidAngle);
        }
        // Cases 2 and 3, the spherical cap intersects the horizon
        else {
            // The area of the cut disk is needed for case 2 and 3
            double cutDiskArea = 0.5*Pi<double>() - getCutDiskArea(cap.diskCutX);
            // Case 3, the spherical cap intersects the horizon but its center is 
            // below the horizon
            if (cap.circleHalfExtent.x <= 0.0) {
                double circleCenterZ = normalizedCenterZ * cap.planeOriginDistance;
                double circleHalfExtentZ = cap.circleHalfExtent.y*normalizedCenterX;
                double circleMaxZ = circleCenterZ + circleHalfExtentZ;
                cap.invCircleHalfExtentZ = 1.0 / circleHalfExtentZ;
                cap.centerOverHalfExtentZ = -circleCenterZ * cap.invCircleHalfExtentZ;
                cap.squaredDiskCutY = saturate(mad(-cap.diskCutX, cap.diskCutX, 1.0f));
                cap.scaleZ = circleMaxZ * rsqrt(cap.squaredDiskCutY);
                cap.densityFactor = cap.squaredDiskCutY / (circleMaxZ*circleMaxZ*cutDiskArea);
                cap.randomToCutDiskAreaFactor = cutDiskArea;
                // Optionally compute the projected solid angle. We do not need it for 
                // sampling but it may be useful in some contexts.
                //double ellipseCutRatioX=-saturate(-(cap.diskCutX-cap.circleCenterX)/cap.circleHalfExtent.x);
                //double normalizedEllipseArea=0.5f*M_PI_FLOAT+getCutDiskArea(ellipseCutRatioX);
                //cap.projectedSolidAngle=cap.circleHalfExtent.x*cap.circleHalfExtent.y*normalizedEllipseArea+cutDiskArea;
            }
            // Case 2, the spherical cap intersects the horizon but its center is 
            // above the horizon
            else {
                // Compute the area of the cut ellipse and the total projected solid 
                // angle
                double ellipseCutRatioX = saturate((cap.diskCutX - cap.circleCenterX) / cap.circleHalfExtent.x);
                double normalizedEllipseArea = 0.5*Pi<double>() + getCutDiskArea(ellipseCutRatioX);
                double cutEllipseArea = cap.circleHalfExtent.x*cap.circleHalfExtent.y*normalizedEllipseArea;
                double projectedSolidAngle = cutEllipseArea + cutDiskArea;
                //cap.projectedSolidAngle=projectedSolidAngle;
                cap.densityFactor = 1.0f / projectedSolidAngle;
                // Prepare the decision which cut disk will be sampled
                cap.cutEllipseAreaRatio = cutEllipseArea * cap.densityFactor;
                cap.randomToCutEllipseAreaFactor = normalizedEllipseArea / cap.cutEllipseAreaRatio;
                cap.randomToCutDiskAreaFactor = cutDiskArea / (1.0f - cap.cutEllipseAreaRatio);
            }
        }
        // For points inside the light source, we treat the projected spherical cap 
        // as empty
        cap.densityFactor = (cap.circleHalfExtent.y >= 1.0f) ? 0.0f : cap.densityFactor;
        //cap.projectedSolidAngle=(cap.circleHalfExtent.y>=1.0f)?0.0f:cap.projectedSolidAngle;
    }


    /*! \return true iff the given projected spherical cap is empty.*/
    inline bool isProjectedSphericalCapEmpty(const ProjectedSphericalCap& cap) {
        return cap.densityFactor <= 0.0f;
    }


    /*! Given the output of prepareProjectedSphericalCapSampling(), this function
        maps given random numbers in the range from 0 to 1 to a normalized
        direction vector providing a sample of the spherical cap in the original
        space (used for arguments of prepareSphericalCapSampling()). If the input
        random numbers are independent and uniform, the distribution of the output
        random variables has the probability density function outDensity with
        respect to the planar Lebesgue measure. The density is constant (i.e.
        independent of the random numbers) unless the sphere center is below the
        horizon. Otherwise the ratio between maximum and minimum is bounded by
        sqrt(2).*/
    inline Vector3d sampleProjectedSphericalCap(double& outDensity, const ProjectedSphericalCap& cap, Vector2d randomNumbers) {
        double area;
        bool sampleEllipse;
        // If the sphere center is below the horizon, we want to sample the cut 
        // unit disk
        if (cap.circleHalfExtent.x <= 0.0) {
            area = mad(randomNumbers.x, -cap.randomToCutDiskAreaFactor, cap.randomToCutDiskAreaFactor);
        }
        // If the sphere center is above the horizon but the sphere intersects it, 
        // we need to decide whether we want to sample the cut unit disk or the cut 
        // ellipse
        else if (cap.diskCutX < 1.0f) {
            sampleEllipse = (randomNumbers.x < cap.cutEllipseAreaRatio);
            area = sampleEllipse ?
                (randomNumbers.x*cap.randomToCutEllipseAreaFactor) :
                mad(randomNumbers.x, -cap.randomToCutDiskAreaFactor, cap.randomToCutDiskAreaFactor);
        }
        // If the sphere is entirely above the horizon, we sample the ellipse
        else {
            area = Pi<double>() * randomNumbers.x;
        }
        // Sample the cut disk
        Vector3d disk;
        disk.x = getAreaDiskCut(area - 0.5*Pi<double>());
        disk.y = sqrt(mad(-disk.x, disk.x, 1.0))*mad(randomNumbers.y, 2.0, -1.0);
        // If the sphere center is below the horizon, we need to warp the samples 
        // further and compute the density
        Vector3d local;
        outDensity = cap.densityFactor;
        if (cap.circleHalfExtent.x <= 0.0) {
            disk.x = -disk.x;
            disk.z = sqrt(saturate(mad(-disk.x, disk.x, mad(-disk.y, disk.y, 1.0))));
            // Scale down along Z to get the appropriate maximal Z
            local.z = disk.z*cap.scaleZ;
            // Scale down along Y to account for the different shape of the cut disk
            double zQuotient = mad(local.z, cap.invCircleHalfExtentZ, cap.centerOverHalfExtentZ);
            double scaleY = cap.circleHalfExtent.y*sqrt(max(0.0, mad(-zQuotient, zQuotient, 1.0) / mad(-disk.z, disk.z, cap.squaredDiskCutY)));
            local.y = disk.y*scaleY;
            // Turn it into a normalized vector to get X
            local.x = sqrt(saturate(mad(-local.y, local.y, mad(-local.z, local.z, 1.0))));
            // Compute the proper density
            outDensity *= local.x / (disk.x*scaleY);
        }
        // If the sphere center is above the horizon but the sphere intersects it, 
        // we may be sampling the cut disk
        else if (cap.diskCutX < 1.0 && !sampleEllipse) {
            local.x = -disk.x;
            local.y = disk.y;
            local.z = sqrt(saturate(mad(-local.x, local.x, mad(-local.y, local.y, 1.0))));
        }
        // Otherwise we are sampling the ellipse (either the cut ellipse or the 
        // entire ellipse, it does not make a difference here)
        else {
            local.x = mad(disk.x, cap.circleHalfExtent.x, cap.circleCenterX);
            local.y = cap.circleHalfExtent.y*disk.y;
            local.z = sqrt(saturate(mad(-local.x, local.x, mad(-local.y, local.y, 1.0))));
        }
        // Go back to the original coordinate frame
        return local.x*cap.tangent + local.y*cap.bitangent + local.z*cap.normal;
    }

    inline double computeProjectedSphericalCapSampleDensity(Vector3d sampledDirection, const ProjectedSphericalCap& cap) {
        // Early out if the sample is not in the spherical cap
        if (dot(cap.normalizedCenter, sampledDirection) < cap.planeOriginDistance) {
            return 0.0;
        }
        // Early out if the sample is in the lower hemisphere
        Vector3d local;
        local.z = dot(cap.normal, sampledDirection);
        if (local.z < 0.0) {
            return 0.0;
        }
        // If the sphere center is below the horizon, things are a little 
        // complicated
        if (cap.circleHalfExtent.x <= 0.0f) {
            local.x = dot(cap.tangent, sampledDirection);
            local.y = dot(cap.bitangent, sampledDirection);
            // Reconstruct the scaling along the y-axis
            Vector3d disk;
            disk.z = local.z / cap.scaleZ;
            double zQuotient = mad(local.z, cap.invCircleHalfExtentZ, cap.centerOverHalfExtentZ);
            double scaleY = cap.circleHalfExtent.y*sqrt(max(0.0, mad(-zQuotient, zQuotient, 1.0) / mad(-disk.z, disk.z, cap.squaredDiskCutY)));
            // Get the whole point in the disk that would have been sampled to 
            // produce this sample
            disk.y = local.y / scaleY;
            disk.x = sqrt(saturate(mad(-disk.y, disk.y, mad(-disk.z, disk.z, 1.0f))));
            // Compute the density
            return cap.densityFactor*local.x / (disk.x*scaleY);
        }
        else {
            // Otherwise the density is constant and has already been computed
            return cap.densityFactor;
        }
    }


}

// #define SPHERE_PROJECTED_SOLID_ANGLE
#define FORCE_UNIFORM

bool EmittingShape::sample_solid_angle(
    const ShadingPoint&         shading_point,
    const foundation::Vector2f& s,
    const float                 shape_prob,
    LightSample&                light_sample) const
{
#ifdef FORCE_UNIFORM
    sample_uniform(s, shape_prob, light_sample);
    return true;
#endif

    // Store a pointer to the emitting shape.
    light_sample.m_shape = this;

    const auto shape_type = get_shape_type();

    if (shape_type == TriangleShape)
    {
        assert(false);
        sample_uniform(s, shape_prob, light_sample);
        return true;
    }
    else if (shape_type == RectangleShape)
    {
        assert(false);
        sample_uniform(s, shape_prob, light_sample);
        return true;
    }
    else if (shape_type == SphereShape)
    {
#ifdef SPHERE_PROJECTED_SOLID_ANGLE
        const Vector3d& surface_point = shading_point.get_point();
        const Vector3d& surface_normal = shading_point.get_shading_normal();

        ProjectedSphericalCap cap;
        prepareProjectedSphericalCapSampling(
            cap,
            surface_normal,
            m_geom.m_sphere.m_center - surface_point,
            m_geom.m_sphere.m_radius);
        assert(!isProjectedSphericalCapEmpty(cap));

        double densityOverCosine;
        Vector3d sampledDirection = -sampleProjectedSphericalCap(densityOverCosine, cap, Vector2d(s));
        assert(is_normalized(sampledDirection));
        const Vector3d p = m_geom.m_sphere.m_center + m_geom.m_sphere.m_radius * sampledDirection;
        assert(feq(sqrt(square_distance(p, m_geom.m_sphere.m_center)), m_geom.m_sphere.m_radius, 0.01));
        const double u = atan2(-sampledDirection.z, sampledDirection.x) * RcpTwoPi<double>();
        const double v = 1.0 - (acos(sampledDirection.y) * RcpPi<double>());
        light_sample.m_param_coords =
            Vector2f(
                static_cast<float>(u),
                static_cast<float>(v));

        light_sample.m_geometric_normal = sampledDirection;
        light_sample.m_shading_normal = sampledDirection;
        assert(shape_prob == 1.0);
        light_sample.m_probability = shape_prob * static_cast<float>(densityOverCosine * square_distance(p, surface_point));

        return true;
#else
        // todo: rename o to surface_point.
        const Vector3d& o = shading_point.get_point();

        const SphericalCapSampler<double> sampler(
            o,
            m_geom.m_sphere.m_center,
            m_geom.m_sphere.m_radius
        );

        const Vector3d n = sampler.sample(Vector2d(s));
        assert(is_normalized(n));
        const Vector3d p = m_geom.m_sphere.m_center + m_geom.m_sphere.m_radius * n;
        assert(feq(sqrt(square_distance(p, m_geom.m_sphere.m_center)), m_geom.m_sphere.m_radius, 0.01));

        //light_sample.m_point = o + d * t;
        light_sample.m_point = p;

        const double u = atan2(-n.z, n.x) * RcpTwoPi<double>();
        const double v = 1.0 - (acos(n.y) * RcpPi<double>());
        light_sample.m_param_coords =
            Vector2f(
                static_cast<float>(u),
                static_cast<float>(v));

        light_sample.m_geometric_normal = n;
        light_sample.m_shading_normal = n;
        assert(shape_prob == 1.0);
        light_sample.m_probability =
            shape_prob * static_cast<float>(sampler.get_pdf() / square_distance(p, o));
        // light solid angle divided by the solid angle of the whole hemisphere

        return true;
#endif // SPHERE_PROJECTED_SOLID_ANGLE
    }
    else if (shape_type == DiskShape)
    {
        assert(false);
        sample_uniform(s, shape_prob, light_sample);
        return true;
    }
    else
    {
        assert(false && "Unknown emitter shape type");
    }

    return false;
}

// todo: rename to evaluate_pdf.
float EmittingShape::evaluate_pdf_solid_angle(
    const ShadingPoint&         light_shading_point,
    const ShadingPoint&         surface_shading_point) const
{
#ifdef  FORCE_UNIFORM
    return evaluate_pdf_uniform();
#endif //  FORCE_UNIFORM

    const auto shape_type = get_shape_type();

    if (shape_type == TriangleShape)
    {
        assert(false);
        return evaluate_pdf_uniform();
    }
    else if (shape_type == RectangleShape)
    {
        assert(false);
        return evaluate_pdf_uniform();
    }
    else if (shape_type == SphereShape)
    {
#ifdef SPHERE_PROJECTED_SOLID_ANGLE
        const Vector3d& surface_point = surface_shading_point.get_point();
        const Vector3d& surface_normal = surface_shading_point.get_shading_normal();

        ProjectedSphericalCap cap;
        prepareProjectedSphericalCapSampling(
            cap,
            surface_normal,
            m_geom.m_sphere.m_center - surface_point,
            m_geom.m_sphere.m_radius);

        const Vector3d dir = normalize(light_shading_point.get_point() - surface_point);
        const double pdf = computeProjectedSphericalCapSampleDensity(dir, cap);
        return static_cast<float>(pdf);
#else
        // todo: rename o.
        const Vector3d& o = surface_shading_point.get_point();

        const SphericalCapSampler<double> sampler(
            o,
            m_geom.m_sphere.m_center,
            m_geom.m_sphere.m_radius
        );

        //const float cos_on = static_cast<float>(dot(surface_shading_point.get_shading_normal(), normalize(m_geom.m_sphere.m_center - o)));
        //const float cos_on = static_cast<float>(dot(normalize(o - m_geom.m_sphere.m_center), light_shading_point.get_shading_normal()));
        const float cos_on = static_cast<float>(abs(dot(surface_shading_point.get_shading_normal(), normalize(light_shading_point.get_point() - o))));
        assert(cos_on > 0.0f);

        return static_cast<float>(sampler.get_pdf() / square_distance(light_shading_point.get_point(), o));
#endif // SPHERE_PROJECTED_SOLID_ANGLE
    }
    else if (shape_type == DiskShape)
    {
        assert(false);
        return evaluate_pdf_uniform();
    }
    else
    {
        assert(false && "Unknown emitter shape type");
        return -1.0f;
    }
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

        const Vector3d dpdu(-TwoPi<double>() * n.y, TwoPi<double>() * n.x, 0.0);
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
