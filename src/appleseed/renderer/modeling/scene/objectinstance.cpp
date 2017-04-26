
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "objectinstance.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/visibilityflags.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/makevector.h"

// OpenImageIO headers.
#include "foundation/platform/oiioheaderguards.h"
BEGIN_OIIO_INCLUDES
#include "OpenImageIO/ustring.h"
END_OIIO_INCLUDES

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// MaterialArray class implementation.
//

APPLESEED_DEFINE_APIARRAY(MaterialArray);

bool has_emitting_materials(const MaterialArray& materials)
{
    for (size_t i = 0, e = materials.size(); i < e; ++i)
    {
        if (materials[i] && materials[i]->has_emission())
            return true;
    }

    return false;
}

bool uses_alpha_mapping(const MaterialArray& materials)
{
    for (size_t i = 0, e = materials.size(); i < e; ++i)
    {
        if (materials[i])
        {
            if (const ShaderGroup* sg = materials[i]->get_uncached_osl_surface())
            {
                if (sg->has_transparency())
                    return true;
            }

            if (materials[i]->has_alpha_map() && !materials[i]->has_uniform_alpha_map_value_of_one())
                return true;
        }
    }

    return false;
}


//
// ObjectInstance class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID ObjectInstance::get_class_uid()
{
    return g_class_uid;
}

struct ObjectInstance::Impl
{
    // Order of data members impacts performance, preserve it.
    Transformd              m_transform;
    string                  m_object_name;
    StringDictionary        m_front_material_mappings;
    StringDictionary        m_back_material_mappings;
    OIIO::ustring           m_sss_set_identifier;
};

ObjectInstance::ObjectInstance(
    const char*             name,
    const ParamArray&       params,
    const char*             object_name,
    const Transformd&       transform,
    const StringDictionary& front_material_mappings,
    const StringDictionary& back_material_mappings)
  : Entity(g_class_uid, params)
  , impl(new Impl())
{
    set_name(name);

    impl->m_transform = transform;
    impl->m_object_name = object_name;
    impl->m_front_material_mappings = front_material_mappings;
    impl->m_back_material_mappings = back_material_mappings;

    const EntityDefMessageContext message_context("object instance", this);

    // Retrieve visibility flags.
    m_vis_flags = VisibilityFlags::parse(params.child("visibility"), message_context);

    // Retrieve medium priority.
    m_medium_priority = params.get_optional<uint8>("medium_priority", 0);

    // Retrieve ray bias method.
    const string ray_bias_method =
        params.get_optional<string>(
            "ray_bias_method",
            "none",
            make_vector("none", "normal", "incoming_direction", "outgoing_direction"),
            message_context);
    if (ray_bias_method == "none")
        m_ray_bias_method = RayBiasMethodNone;
    else if (ray_bias_method == "normal")
        m_ray_bias_method = RayBiasMethodNormal;
    else if (ray_bias_method == "incoming_direction")
        m_ray_bias_method = RayBiasMethodIncomingDirection;
    else m_ray_bias_method = RayBiasMethodOutgoingDirection;

    // Retrieve ray bias distance.
    m_ray_bias_distance = params.get_optional<double>("ray_bias_distance", 0.0);

    // Retrieve SSS set ID.
    impl->m_sss_set_identifier = params.get_optional<std::string>("sss_set_id", "");

    // Retrieve flip normals flag.
    m_flip_normals = params.get_optional<bool>("flip_normals");

    // No bound object yet.
    m_object = 0;
}

ObjectInstance::~ObjectInstance()
{
    delete impl;
}

void ObjectInstance::release()
{
    delete this;
}

uint64 ObjectInstance::compute_signature() const
{
    return
        m_object
            ? combine_signatures(Entity::compute_signature(), m_object->compute_signature())
            : Entity::compute_signature();
}

const char* ObjectInstance::get_object_name() const
{
    return impl->m_object_name.c_str();
}

bool ObjectInstance::is_in_same_sss_set(const ObjectInstance& other) const
{
    // If it is the same object instance, the SSS set is also the same.
    if (other.get_uid() == get_uid())
        return true;

    // An empty identifier indicates that the object instance belongs to its own SSS set.
    if (impl->m_sss_set_identifier.empty() || other.impl->m_sss_set_identifier.empty())
        return false;

    return impl->m_sss_set_identifier == other.impl->m_sss_set_identifier;
}

const Transformd& ObjectInstance::get_transform() const
{
    return impl->m_transform;
}

Object* ObjectInstance::find_object() const
{
    const Entity* parent = get_parent();

    while (parent)
    {
        if (dynamic_cast<const Assembly*>(parent) == 0)
            break;

        Object* object =
            static_cast<const Assembly*>(parent)
                ->objects().get_by_name(impl->m_object_name.c_str());

        if (object)
            return object;

        parent = parent->get_parent();
    }

    return 0;
}

GAABB3 ObjectInstance::compute_parent_bbox() const
{
    // In many places, we need the parent-space bounding box of an object instance
    // before input binding is performed, i.e. before the instantiated object is
    // bound to the instance. Therefore we manually look the object up through the
    // assembly hierarchy instead of simply using m_object.

    const Object* object = find_object();

    return
        object
            ? impl->m_transform.to_parent(object->compute_local_bbox())
            : GAABB3::invalid();
}

void ObjectInstance::clear_front_materials()
{
    impl->m_front_material_mappings.clear();

    bump_version_id();
}

void ObjectInstance::clear_back_materials()
{
    impl->m_back_material_mappings.clear();

    bump_version_id();
}

void ObjectInstance::assign_material(
    const char*             slot,
    const Side              side,
    const char*             name)
{
    StringDictionary& material_mappings =
        side == FrontSide
            ? impl->m_front_material_mappings
            : impl->m_back_material_mappings;

    material_mappings.insert(slot, name);

    bump_version_id();
}

void ObjectInstance::unassign_material(
    const char*             slot,
    const Side              side)
{
    StringDictionary& material_mappings =
        side == FrontSide
            ? impl->m_front_material_mappings
            : impl->m_back_material_mappings;

    material_mappings.remove(slot);

    bump_version_id();
}

StringDictionary& ObjectInstance::get_front_material_mappings() const
{
    return impl->m_front_material_mappings;
}

StringDictionary& ObjectInstance::get_back_material_mappings() const
{
    return impl->m_back_material_mappings;
}

const char* ObjectInstance::get_material_name(const size_t pa_index, const Side side) const
{
    const Object* object = find_object();

    if (object == 0)
        return 0;

    const StringDictionary& material_mappings =
        side == FrontSide
            ? impl->m_front_material_mappings
            : impl->m_back_material_mappings;

    if (object->get_material_slot_count() > 0)
    {
        const char* slot_name = object->get_material_slot(pa_index);
        return material_mappings.exist(slot_name) ? material_mappings.get(slot_name) : 0;
    }
    else
    {
        return material_mappings.empty() ? 0 : material_mappings.begin().value();
    }
}

void ObjectInstance::unbind_object()
{
    m_object = 0;
}

void ObjectInstance::bind_object(const ObjectContainer& objects)
{
    if (m_object == 0)
        m_object = objects.get_by_name(impl->m_object_name.c_str());
}

void ObjectInstance::check_object() const
{
    if (m_object == 0)
        throw ExceptionUnknownEntity(impl->m_object_name.c_str(), this);
}

namespace
{
    void do_bind_materials(
        MaterialArray&              material_array,
        const Object&               object,
        const StringDictionary&     material_mappings,
        const MaterialContainer&    materials)
    {
        for (size_t i = 0; i < material_array.size(); ++i)
        {
            if (material_array[i] == 0)
            {
                if (object.get_material_slot_count() > 0)
                {
                    const char* slot_name = object.get_material_slot(i);

                    if (!material_mappings.exist(slot_name))
                        continue;

                    const char* material_name = material_mappings.get(slot_name);
                    material_array[i] = materials.get_by_name(material_name);
                }
                else if (!material_mappings.empty())
                {
                    const char* material_name = material_mappings.begin().value();
                    material_array[i] = materials.get_by_name(material_name);
                }
            }
        }
    }

    void do_check_materials(
        const ObjectInstance*       object_instance,
        const Object&               object,
        const MaterialArray&        material_array,
        const StringDictionary&     material_mappings)
    {
        for (size_t i = 0; i < material_array.size(); ++i)
        {
            if (material_array[i] == 0)
            {
                if (object.get_material_slot_count() > 0)
                {
                    const char* slot_name = object.get_material_slot(i);

                    if (!material_mappings.exist(slot_name))
                        continue;

                    const char* material_name = material_mappings.get(slot_name);
                    throw ExceptionUnknownEntity(material_name, object_instance);
                }
                else if (!material_mappings.empty())
                {
                    const char* material_name = material_mappings.begin().value();
                    throw ExceptionUnknownEntity(material_name, object_instance);
                }
            }
        }
    }
}

void ObjectInstance::unbind_materials()
{
    assert(m_object);

    m_front_materials.clear();
    m_back_materials.clear();

    size_t slot_count = m_object->get_material_slot_count();

    if (slot_count < 1)
        slot_count = 1;

    m_front_materials.resize(slot_count);
    m_back_materials.resize(slot_count);
}

void ObjectInstance::bind_materials(const MaterialContainer& materials)
{
    assert(m_object);

    do_bind_materials(m_front_materials, *m_object, impl->m_front_material_mappings, materials);
    do_bind_materials(m_back_materials, *m_object, impl->m_back_material_mappings, materials);
}

void ObjectInstance::check_materials() const
{
    assert(m_object);

    do_check_materials(this, *m_object, m_front_materials, impl->m_front_material_mappings);
    do_check_materials(this, *m_object, m_back_materials, impl->m_back_material_mappings);
}

bool ObjectInstance::uses_alpha_mapping() const
{
    if (get_object().has_alpha_map())
        return true;

    return
        renderer::uses_alpha_mapping(m_back_materials) ||
        renderer::uses_alpha_mapping(m_front_materials);
}

bool ObjectInstance::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    if (!Entity::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    m_transform_swaps_handedness = get_transform().swaps_handedness();

    const EntityDefMessageContext context("object instance", this);

    const bool uses_materials_alpha_mapping =
        renderer::uses_alpha_mapping(m_back_materials) ||
        renderer::uses_alpha_mapping(m_front_materials);

    if (uses_materials_alpha_mapping)
    {
        if (m_front_materials != m_back_materials)
        {
            RENDERER_LOG_WARNING(
                "%s: object instance uses alpha mapping on one side (or both) but materials are different on front and back faces; "
                "this may lead to unexpected or unphysical results since the direction of shadow rays is unpredictable.",
                context.get());
        }
    }

    return true;
}


//
// ObjectInstanceFactory class implementation.
//

DictionaryArray ObjectInstanceFactory::get_input_metadata()
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "medium_priority")
            .insert("label", "Medium Priority")
            .insert("type", "numeric")
            .insert("min_value", "0")
            .insert("max_value", "255")
            .insert("use", "optional")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ray_bias_method")
            .insert("label", "Ray Bias Method")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("No Ray Bias", "none")
                    .insert("Shift Along Surface Normal", "normal")
                    .insert("Shift Along Incoming Direction", "incoming_direction")
                    .insert("Shift Along Outgoing Direction", "outgoing_direction"))
            .insert("use", "optional")
            .insert("default", "none"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ray_bias_distance")
            .insert("label", "Ray Bias Distance")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
        .insert("name", "sss_set_id")
        .insert("label", "SSS Set Identifier")
        .insert("type", "text")
        .insert("use", "optional")
        .insert("default", ""));

    return metadata;
}

auto_release_ptr<ObjectInstance> ObjectInstanceFactory::create(
    const char*             name,
    const ParamArray&       params,
    const char*             object_name,
    const Transformd&       transform,
    const StringDictionary& front_material_mappings,
    const StringDictionary& back_material_mappings)
{
    return
        auto_release_ptr<ObjectInstance>(
            new ObjectInstance(
                name,
                params,
                object_name,
                transform,
                front_material_mappings,
                back_material_mappings));
}

}   // namespace renderer
