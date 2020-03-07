
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Francois Beaune, The appleseedhq Organization
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

// interface header.
#include "sphereobject.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/refining.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/distance.h"
#include "foundation/math/intersection/raysphere.h"
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/searchpaths.h"

using namespace foundation;

namespace renderer
{

//
// SphereObject class implementation.
//

namespace
{
    const char* Model = "sphere_object";
}

SphereObject::SphereObject(
    const char*            name,
    const ParamArray&      params)
  : ProceduralObject(name, params)
{
}

void SphereObject::release()
{
    delete this;
}

const char* SphereObject::get_model() const
{
    return Model;
}

GAABB3 SphereObject::compute_local_bbox() const
{
    return GAABB3(GVector3(-1.0), GVector3(1.0));
}

size_t SphereObject::get_material_slot_count() const
{
    return 1;
}

const char* SphereObject::get_material_slot(const size_t index) const
{
    return "default";
}

Vector3d SphereObject::get_center() const
{
    return Vector3d(0.0, 0.0, 0.0);
}

double SphereObject::get_radius() const
{
    return 1.0;
}

void SphereObject::intersect(
    const ShadingRay&      ray,
    IntersectionResult&    result) const
{
    result.m_hit = intersect_sphere(
        ray,
        Vector3d(0.0, 0.0, 0.0),
        1.0,
        result.m_distance);

    if (result.m_hit)
    {
        const Vector3d n = ray.point_at(result.m_distance);
        result.m_geometric_normal = n;
        result.m_shading_normal = n;

        const Vector3f p(n);
        result.m_uv[0] = std::atan2(-p.z, p.x) * RcpTwoPi<float>();
        result.m_uv[1] = 1.0f - (std::acos(p.y) * RcpPi<float>());

        result.m_material_slot = 0;
    }
}

bool SphereObject::intersect(const ShadingRay& ray) const
{
    return intersect_sphere(
        ray,
        Vector3d(0.0, 0.0, 0.0),
        1.0);
}

namespace
{
    template <typename T>
    inline T intersect_sphere_always(const Ray<T, 3>& ray)
    {
        const T a = dot(ray.m_dir, ray.m_dir);
        assert(a > T(0.0));

        const Vector<T, 3> v = -ray.m_org;
        const T b = dot(ray.m_dir, v);

        const T d = square(b) - a * (dot(v, v) - T(1.0));
        assert(d >= T(0.0));

        const T sqrt_d = std::sqrt(d);
        const T t1 = (b - sqrt_d) / a;
        const T t2 = (b + sqrt_d) / a;

        return std::abs(t1) < std::abs(t2) ? t1 : t2;
    }
}

void SphereObject::refine_and_offset(
    const Ray3d&        obj_inst_ray,
    Vector3d&           obj_inst_front_point,
    Vector3d&           obj_inst_back_point,
    Vector3d&           obj_inst_geo_normal) const
{
    // Handle refining for the ray origin point.
    const auto intersection_handling = [](const Vector3d& p, const Vector3d& dir)
    {
        const Ray3d ray(p, dir);
        return intersect_sphere_always(ray);
    };

    // Refine the location of the intersection point.
    const Vector3d refined_intersection_point =
        refine(
            obj_inst_ray.m_org,
            obj_inst_ray.m_dir,
            intersection_handling);

    // Compute the geometric normal to the hit in object instance space.
    obj_inst_geo_normal = refined_intersection_point;
    obj_inst_geo_normal = faceforward(obj_inst_geo_normal, obj_inst_ray.m_dir);

    adaptive_offset(
        refined_intersection_point,
        obj_inst_geo_normal,
        obj_inst_front_point,
        obj_inst_back_point,
        intersection_handling);
}


//
// SphereObjectFactory class implementation.
//

void SphereObjectFactory::release()
{
    delete this;
}

const char* SphereObjectFactory::get_model() const
{
    return Model;
}

Dictionary SphereObjectFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Sphere Object");
}

DictionaryArray SphereObjectFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "radius")
            .insert("label", "Radius")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "10.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<Object> SphereObjectFactory::create(
    const char*            name,
    const ParamArray&      params) const
{
    return auto_release_ptr<Object>(new SphereObject(name, params));
}

bool SphereObjectFactory::create(
    const char*            name,
    const ParamArray&      params,
    const SearchPaths&     search_paths,
    const bool             omit_loading_assets,
    ObjectArray&           objects) const
{
    objects.push_back(create(name, params).release());
    return true;
}

}   // namespace renderer
