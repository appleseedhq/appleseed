
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/modeling/geometry/object.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// MaterialIndexArray class implementation.
//

FOUNDATION_DEFINE_ARRAY(MaterialIndexArray);


//
// ObjectInstance class implementation.
//

struct ObjectInstance::Impl
{
    // Order of data members impacts performance, preserve it.
    Transformd                  m_transform;
    GAABB3                      m_parent_bbox;
    string                      m_name;
    size_t                      m_object_index;
    MaterialIndexArray          m_material_indices;
};

// Constructor.
ObjectInstance::ObjectInstance(
    const char*                 name,
    const Object&               object,
    const size_t                object_index,
    const Transformd&           transform,
    const MaterialIndexArray&   material_indices)
  : impl(new Impl())
{
    assert(name);

    impl->m_transform = transform;
    impl->m_parent_bbox = transform.transform_to_parent(object.get_local_bbox());
    impl->m_name = name;
    impl->m_object_index = object_index;
    impl->m_material_indices = material_indices;
}

// Destructor.
ObjectInstance::~ObjectInstance()
{
    delete impl;
}

// Delete this instance.
void ObjectInstance::release()
{
    delete this;
}

// Return the name of this instance.
const char* ObjectInstance::get_name() const
{
    return impl->m_name.c_str();
}

// Return the index in the assembly of the instantiated object.
size_t ObjectInstance::get_object_index() const
{
    return impl->m_object_index;
}

// Return the transform of the instance.
const Transformd& ObjectInstance::get_transform() const
{
    return impl->m_transform;
}

// Return the parent space bounding box of the instance.
const GAABB3& ObjectInstance::get_parent_bbox() const
{
    return impl->m_parent_bbox;
}

// Return the array of material indices.
const MaterialIndexArray& ObjectInstance::get_material_indices() const
{
    return impl->m_material_indices;
}


//
// ObjectInstanceFactory class implementation.
//

// Create a new object instance.
auto_release_ptr<ObjectInstance> ObjectInstanceFactory::create(
    const char*                 name,
    const Object&               object,
    const size_t                object_index,
    const Transformd&           transform,
    const MaterialIndexArray&   material_indices)
{
    return
        auto_release_ptr<ObjectInstance>(
            new ObjectInstance(
                name,
                object,
                object_index,
                transform,
                material_indices));
}

}   // namespace renderer
