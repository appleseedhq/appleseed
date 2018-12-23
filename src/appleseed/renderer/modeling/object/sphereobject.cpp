
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Mayank Dhiman, The appleseedhq Organization
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
#include "sphereobject.h"

// appleseed.renderer headers.
#include "renderer/modeling/object/curveobject.h"
#include "renderer/modeling/object/proceduralobject.h"

// todo: fix.
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/utility/api/specializedapiarrays.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// SphereObject Class Implementation
//

namespace
{
    const char* Model = "sphere_object";
}

struct SphereObject::Impl
{
    vector<string>              m_material_slots;
    Impl()
    {
    }    
};

SphereObject::SphereObject(
    const char*                 name,
    const ParamArray&           params)
    : ProceduralObject(name, params)
    , impl(new Impl())
{
}    

SphereObject::~SphereObject()
{
    delete impl;
}

void SphereObject::release()
{
    delete this;
}

const char* SphereObject::get_model() const
{
    return Model;
}

bool SphereObject::on_frame_begin(
    const Project&         project,
    const BaseGroup*       parent,
    OnFrameBeginRecorder&  recorder,
    foundation::IAbortSwitch*          abort_switch)
{
    if (!ProceduralObject::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    m_radius = get_uncached_radius();
    m_rcp_radius = 1.0 / m_radius;

    return true;
}

GAABB3 SphereObject::compute_local_bbox() const
{
    const auto r = static_cast<GScalar>(get_uncached_radius());
    return GAABB3(GVector3(-r), GVector3(r));
}

size_t SphereObject::get_material_slot_count() const 
{
    return 1;
}

const char* SphereObject::get_material_slot(const size_t index) const
{
    return "default";
}

void SphereObject::intersect(
const ShadingRay&  ray,
IntersectionResult&     result) const 
{
    const double Epsilon = 1.0e-6;

    const double a = foundation::dot(ray.m_org, ray.m_dir);
    const double b = foundation::square(a) - dot(ray.m_org, ray.m_org) + foundation::square(m_radius);

    if (b < 0.0)
    {
        result.m_hit = false;
        return;
    }

    const double c = std::sqrt(b);

    double t = -a - c;
    if (t < std::max(ray.m_tmin, Epsilon) || t >= ray.m_tmax)
    {
        t = -a + c;
        if (t < std::max(ray.m_tmin, Epsilon) || t >= ray.m_tmax)
        {
            result.m_hit = false;
            return;
        }
    }

    result.m_hit = true;
    result.m_distance = t;

    const foundation::Vector3d n = foundation::normalize(ray.point_at(t));
    result.m_geometric_normal = n;
    result.m_shading_normal = n;

    const foundation::Vector3f p(ray.point_at(t) * m_rcp_radius);
    result.m_uv[0] = std::acos(p.y) * foundation::RcpPi<float>();
    result.m_uv[1] = std::atan2(-p.z, p.x) * foundation::RcpTwoPi<float>();

    result.m_material_slot = 0;
}

bool SphereObject::intersect(
    const ShadingRay&  ray) const
{
    const double Epsilon = 1.0e-6;

    const double a = foundation::dot(ray.m_org, ray.m_dir);
    const double b = foundation::square(a) - dot(ray.m_org, ray.m_org) + foundation::square(m_radius);

    if (b < 0.0)
        return false;

    const double c = std::sqrt(b);

    const double t1 = -a - c;
    if (t1 >= std::max(ray.m_tmin, Epsilon) && t1 < ray.m_tmax)
        return true;

    const double t2 = -a + c;
    if (t2 >= std::max(ray.m_tmin, Epsilon) && t2 < ray.m_tmax)
        return true;

    return false;
}

double SphereObject::get_uncached_radius() const
{
    return m_params.get_optional<double>("radius");
}


//
//  Class SphereObjectFactory Implementation
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
    const char*                 name,
    const ParamArray&      params) const 
{
    return auto_release_ptr<Object>(new SphereObject(name, params));
}

bool SphereObjectFactory::create(
    const char*                     name,
    const ParamArray&               params,
    const SearchPaths&              search_paths,
    const bool                      omit_loading_assets,
    ObjectArray&                    objects) const
{
    objects.push_back(create(name, params).release());
    return true;
}

}  // namespace renderer
