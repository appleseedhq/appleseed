
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
#include "rectobject.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/math/intersection/rayparallelogram.h"
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/string.h"

using namespace foundation;

namespace renderer
{

//
// RectObject class implementation.
//

namespace
{
    const char* Model = "rect_object";
}

RectObject::RectObject(
    const char*            name,
    const ParamArray&      params)
  : ProceduralObject(name, params)
{
}

void RectObject::release()
{
    delete this;
}

const char* RectObject::get_model() const
{
    return Model;
}

bool RectObject::on_frame_begin(
    const Project&         project,
    const BaseGroup*       parent,
    OnFrameBeginRecorder&  recorder,
    IAbortSwitch*          abort_switch)
{
    if (!ProceduralObject::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    const double width  = get_uncached_width();
    const double height = get_uncached_height();

    m_half_width = width * 0.5;
    m_half_height = height * 0.5;

    if (width != 0.0)
        m_rcp_width = 1.0 / width;

    if (height != 0.0)
        m_rcp_height = 1.0 / height;

    return true;
}

GAABB3 RectObject::compute_local_bbox() const
{
    const float width  = static_cast<float>(get_uncached_width());
    const float height = static_cast<float>(get_uncached_height());

    const GVector3 pmax(width * 0.5f, 0.0f, height * 0.5f);
    const GVector3 pmin(-pmax.x, 0.0f, -pmax.z);

    GAABB3 bbox(pmin, pmax);
    return bbox;
}

size_t RectObject::get_material_slot_count() const
{
    return 1;
}

const char* RectObject::get_material_slot(const size_t index) const
{
    return "default";
}

double RectObject::get_uncached_width() const
{
    return m_params.get_optional<double>("width", 1.0);
}

double RectObject::get_uncached_height() const
{
    return m_params.get_optional<double>("height", 1.0);
}

void RectObject::intersect(
    const ShadingRay&      ray,
    IntersectionResult&    result) const
{
    if (m_half_width == 0.0 || m_half_height == 0.0)
    {
        result.m_hit = false;
        return;
    }

    if (ray.m_dir.y == 0.0)
    {
        result.m_hit = false;
        return;
    }

    const double t = -ray.m_org.y / ray.m_dir.y;

    if (t < ray.m_tmin || t >= ray.m_tmax)
    {
        result.m_hit = false;
        return;
    }

    const double u =
        ((ray.m_org.x + t * ray.m_dir.x) + m_half_width) * m_rcp_width;

    if (u < 0.0 || u > 1.0)
    {
        result.m_hit = false;
        return;
    }

    const double v =
        ((ray.m_org.z + t * ray.m_dir.z) + m_half_height) * m_rcp_height;

    if (v < 0.0 || v > 1.0)
    {
        result.m_hit = false;
        return;
    }

    result.m_hit = true;
    result.m_distance = t;

    const Vector3d n(0.0, 1.0, 0.0);
    result.m_geometric_normal = n;
    result.m_shading_normal = n;

    result.m_uv[0] = static_cast<float>(u);
    result.m_uv[1] = 1.0f - static_cast<float>(v);
    result.m_material_slot = 0;
}

bool RectObject::intersect(const ShadingRay& ray) const
{
    if (m_half_width == 0.0 || m_half_height == 0.0)
        return false;

    if (ray.m_dir.y == 0.0)
        return false;

    const double t = -ray.m_org.y / ray.m_dir.y;

    if (t < ray.m_tmin || t >= ray.m_tmax)
        return false;

    const double u =
        ((ray.m_org.x + t * ray.m_dir.x) + m_half_width) * m_rcp_width;

    if (u < 0.0 || u > 1.0)
        return false;

    const double v =
        ((ray.m_org.z + t * ray.m_dir.z) + m_half_height) * m_rcp_height;

    if (v < 0.0 || v > 1.0)
        return false;

    return true;
}


//
// RectObjectFactory class implementation.
//

void RectObjectFactory::release()
{
    delete this;
}

const char* RectObjectFactory::get_model() const
{
    return Model;
}

Dictionary RectObjectFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Rect Object");
}

DictionaryArray RectObjectFactory::get_input_metadata() const
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

auto_release_ptr<Object> RectObjectFactory::create(
    const char*            name,
    const ParamArray&      params) const
{
    return auto_release_ptr<Object>(new RectObject(name, params));
}

bool RectObjectFactory::create(
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
