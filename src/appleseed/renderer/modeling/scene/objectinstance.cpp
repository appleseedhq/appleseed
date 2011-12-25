
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "renderer/modeling/object/object.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/memory.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// MaterialArray class implementation.
//

DEFINE_ARRAY(MaterialArray);


//
// ObjectInstance class implementation.
//

struct ObjectInstance::Impl
{
    // Order of data members impacts performance, preserve it.
    Transformd          m_transform;
    GAABB3              m_parent_bbox;
    Object&             m_object;
    MaterialArray       m_materials;
    StringArray         m_material_names;

    explicit Impl(Object& object)
      : m_object(object)
    {
    }
};

namespace
{
    const UniqueID g_class_uid = new_guid();
}

ObjectInstance::ObjectInstance(
    const char*         name,
    const ParamArray&   params,
    Object&             object,
    const Transformd&   transform,
    const StringArray&  material_names)
  : Entity(g_class_uid, params)
  , impl(new Impl(object))
{
    set_name(name);

    impl->m_transform = transform;
    impl->m_parent_bbox = transform.transform_to_parent(object.get_local_bbox());
    impl->m_material_names = material_names;

    m_double_sided = params.get_optional<bool>("double_sided", false);
}

ObjectInstance::~ObjectInstance()
{
    delete impl;
}

void ObjectInstance::release()
{
    delete this;
}

Object& ObjectInstance::get_object() const
{
    return impl->m_object;
}

const Transformd& ObjectInstance::get_transform() const
{
    return impl->m_transform;
}

const GAABB3& ObjectInstance::get_parent_bbox() const
{
    return impl->m_parent_bbox;
}

void ObjectInstance::clear_materials()
{
    impl->m_materials.clear();
    impl->m_material_names.clear();
}

void ObjectInstance::assign_material(const size_t slot, const char* material_name)
{
    ensure_size(impl->m_material_names, slot + 1);
    impl->m_material_names.set(slot, material_name);
}

const StringArray& ObjectInstance::get_material_names() const
{
    return impl->m_material_names;
}

void ObjectInstance::bind_entities(const MaterialContainer& materials)
{
    const size_t material_count = impl->m_material_names.size();

    impl->m_materials.clear();
    impl->m_materials.resize(material_count);

    for (size_t i = 0; i < material_count; ++i)
    {
        const char* material_name = impl->m_material_names[i];

        impl->m_materials[i] = materials.get_by_name(material_name);

        if (impl->m_materials[i] == 0)
            throw ExceptionUnknownEntity(material_name);
    }
}

const MaterialArray& ObjectInstance::get_materials() const
{
    return impl->m_materials;
}


//
// ObjectInstanceFactory class implementation.
//

auto_release_ptr<ObjectInstance> ObjectInstanceFactory::create(
    const char*         name,
    const ParamArray&   params,
    Object&             object,
    const Transformd&   transform,
    const StringArray&  material_names)
{
    return
        auto_release_ptr<ObjectInstance>(
            new ObjectInstance(
                name,
                params,
                object,
                transform,
                material_names));
}

}   // namespace renderer
