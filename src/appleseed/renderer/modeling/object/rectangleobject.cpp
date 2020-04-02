
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

// interface header.
#include "rectangleobject.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/refining.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/intersection/rayparallelogram.h"
#include "foundation/math/intersection/rayplane.h"
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/searchpaths.h"

using namespace foundation;

namespace renderer
{

//
// RectangleObject class implementation.
//

namespace
{
    const char* Model = "rectangle_object";
}

struct RectangleObject::Impl
{
    Vector3d    m_corner;
    Vector3d    m_normal;
    Vector3d    m_x;
    Vector3d    m_y;
    bool        m_skip_intersection;
};

RectangleObject::RectangleObject(
    const char*            name,
    const ParamArray&      params)
  : ProceduralObject(name, params)
  , impl(new Impl())
{
}

void RectangleObject::release()
{
    delete this;
}

const char* RectangleObject::get_model() const
{
    return Model;
}

bool RectangleObject::on_frame_begin(
    const Project&         project,
    const BaseGroup*       parent,
    OnFrameBeginRecorder&  recorder,
    IAbortSwitch*          abort_switch)
{
    if (!ProceduralObject::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    const double width  = get_uncached_width();
    const double height = get_uncached_height();

    const double half_width = width * 0.5;
    const double half_height = height * 0.5;

    impl->m_x = Vector3d(width, 0.0, 0.0);
    impl->m_y = Vector3d(0.0, 0.0, -height);

    impl->m_corner = Vector3d(-half_width, 0.0, half_height);
    impl->m_normal = Vector3d(0.0, 1.0, 0.0);

    impl->m_skip_intersection = (half_width == 0.0f || half_height == 0.0f);

    return true;
}

GAABB3 RectangleObject::compute_local_bbox() const
{
    const float width  = static_cast<float>(get_uncached_width());
    const float height = static_cast<float>(get_uncached_height());

    const GVector3 pmax(width * 0.5f, 0.0f, height * 0.5f);
    const GVector3 pmin(-pmax.x, 0.0f, -pmax.z);

    GAABB3 bbox(pmin, pmax);
    return bbox;
}

size_t RectangleObject::get_material_slot_count() const
{
    return 1;
}

const char* RectangleObject::get_material_slot(const size_t index) const
{
    return "default";
}

double RectangleObject::get_uncached_width() const
{
    return m_params.get_optional<double>("width", 1.0);
}

double RectangleObject::get_uncached_height() const
{
    return m_params.get_optional<double>("height", 1.0);
}

void RectangleObject::get_origin_and_axes(
    Vector3d&               origin,
    Vector3d&               x,
    Vector3d&               y,
    Vector3d&               n) const
{
    const double width = get_uncached_width();
    const double height = get_uncached_height();

    origin = Vector3d(-width * 0.5, 0.0, height * 0.5);
    x = Vector3d(width, 0.0, 0.0);
    y = Vector3d(0.0, 0.0, -height);
    n = Vector3d(0.0, 1.0, 0.0);
}

void RectangleObject::intersect(
    const ShadingRay&      ray,
    IntersectionResult&    result) const
{
    if APPLESEED_UNLIKELY(impl->m_skip_intersection)
    {
        result.m_hit = false;
        return;
    }

    double u, v;

    result.m_hit = intersect_parallelogram(
        ray,
        impl->m_corner,
        impl->m_x,
        impl->m_y,
        impl->m_normal,
        result.m_distance,
        u,
        v);

    if (result.m_hit)
    {
        result.m_geometric_normal = impl->m_normal;
        result.m_shading_normal = impl->m_normal;

        result.m_uv[0] = static_cast<float>(u);
        result.m_uv[1] = static_cast<float>(v);
        result.m_material_slot = 0;
    }
}

bool RectangleObject::intersect(const ShadingRay& ray) const
{
    if APPLESEED_UNLIKELY(impl->m_skip_intersection)
        return false;

    return intersect_parallelogram(
        ray,
        impl->m_corner,
        impl->m_x,
        impl->m_y,
        impl->m_normal);
}

void RectangleObject::refine_and_offset(
    const Ray3d&        obj_inst_ray,
    Vector3d&           obj_inst_front_point,
    Vector3d&           obj_inst_back_point,
    Vector3d&           obj_inst_geo_normal) const
{
    const auto intersection_handling = [this](const Vector3d& p, const Vector3d& dir)
    {
        const Ray3d ray(p, dir);
        return foundation::intersect(ray, impl->m_corner, impl->m_normal);
    };

    const Vector3d refined_intersection_point =
        refine(
            obj_inst_ray.m_org,
            obj_inst_ray.m_dir,
            intersection_handling);

    obj_inst_geo_normal = faceforward(impl->m_normal, obj_inst_ray.m_dir);

    adaptive_offset(
        refined_intersection_point,
        obj_inst_geo_normal,
        obj_inst_front_point,
        obj_inst_back_point,
        intersection_handling);
}

//
// RectangleObjectFactory class implementation.
//

void RectangleObjectFactory::release()
{
    delete this;
}

const char* RectangleObjectFactory::get_model() const
{
    return Model;
}

Dictionary RectangleObjectFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Rectangle Object");
}

DictionaryArray RectangleObjectFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "width")
            .insert("label", "Width")
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

    metadata.push_back(
        Dictionary()
            .insert("name", "height")
            .insert("label", "Height")
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

auto_release_ptr<Object> RectangleObjectFactory::create(
    const char*            name,
    const ParamArray&      params) const
{
    return auto_release_ptr<Object>(new RectangleObject(name, params));
}

bool RectangleObjectFactory::create(
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
