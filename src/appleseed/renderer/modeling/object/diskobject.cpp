
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Kevin Masson, The appleseedhq Organization
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
#include "diskobject.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/refining.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/intersection/raydisk.h"
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
// DiskObject class implementation.
//

namespace
{
    const char* Model = "disk_object";
}

struct DiskObject::Impl
{
    double      m_radius;
};

DiskObject::DiskObject(
    const char*            name,
    const ParamArray&      params)
  : ProceduralObject(name, params)
  , impl(new Impl())
{
}

void DiskObject::release()
{
    delete this;
}

const char* DiskObject::get_model() const
{
    return Model;
}

bool DiskObject::on_frame_begin(
    const Project&         project,
    const BaseGroup*       parent,
    OnFrameBeginRecorder&  recorder,
    IAbortSwitch*          abort_switch)
{
    if (!ProceduralObject::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    impl->m_radius = get_uncached_radius();
    return true;
}

GAABB3 DiskObject::compute_local_bbox() const
{
    const auto r = static_cast<GScalar>(get_uncached_radius());
    const GAABB3 bbox(
        GVector3(-r, GScalar(0), -r),
        GVector3(r, GScalar(0), r));
    return bbox;
}

size_t DiskObject::get_material_slot_count() const
{
    return 1;
}

const char* DiskObject::get_material_slot(const size_t index) const
{
    return "default";
}

double DiskObject::get_uncached_radius() const
{
    return m_params.get_optional<double>("radius", 1.0);
}

Vector3d DiskObject::get_uncached_center() const
{
    return Vector3d(0.0, 0.0, 0.0);
}

void DiskObject::get_axes(
    Vector3d&              x,
    Vector3d&              y,
    Vector3d&              n) const
{
    const double radius = get_uncached_radius();

    x = Vector3d(radius, 0.0, 0.0);
    y = Vector3d(0.0, 0.0, radius);
    n = Vector3d(0.0, 1.0, 0.0);
}

void DiskObject::intersect(
    const ShadingRay&      ray,
    IntersectionResult&    result) const
{
    double u, v;

    result.m_hit = intersect_disk(
        ray,
        impl->m_radius,
        result.m_distance,
        u,
        v);

    if (result.m_hit)
    {
        const Vector3d n(0.0, 1.0, 0.0);
        result.m_geometric_normal = n;
        result.m_shading_normal = n;

        result.m_uv[0] = static_cast<float>(u);
        result.m_uv[1] = static_cast<float>(v);

        result.m_material_slot = 0;
    }
}

bool DiskObject::intersect(const ShadingRay& ray) const
{
    return intersect_disk(
        ray,
        impl->m_radius);
}

void DiskObject::refine_and_offset(
    const Ray3d&        obj_inst_ray,
    Vector3d&           obj_inst_front_point,
    Vector3d&           obj_inst_back_point,
    Vector3d&           obj_inst_geo_normal) const
{
    const auto intersection_handling = [](const Vector3d& p, const Vector3d& dir)
    {
        assert(is_normalized(dir));
        return -p.y / dir.y;
    };

    const Vector3d refined_intersection_point =
        refine(
            obj_inst_ray.m_org,
            obj_inst_ray.m_dir,
            intersection_handling);

    obj_inst_geo_normal =
        obj_inst_ray.m_dir.y < 0.0
            ? Vector3d(0.0, 1.0, 0.0)
            : Vector3d(0.0, -1.0, 0.0);

    adaptive_offset(
        refined_intersection_point,
        obj_inst_geo_normal,
        obj_inst_front_point,
        obj_inst_back_point,
        intersection_handling);
}


//
// DiskObjectFactory class implementation.
//

void DiskObjectFactory::release()
{
    delete this;
}

const char* DiskObjectFactory::get_model() const
{
    return Model;
}

Dictionary DiskObjectFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Disk Object");
}

DictionaryArray DiskObjectFactory::get_input_metadata() const
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

auto_release_ptr<Object> DiskObjectFactory::create(
    const char*            name,
    const ParamArray&      params) const
{
    return auto_release_ptr<Object>(new DiskObject(name, params));
}

bool DiskObjectFactory::create(
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
