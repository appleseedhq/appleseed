
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "optixtracecontext.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/transformsequencestack.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{
namespace
{

//
// Scene Visitor.
//

class SceneVisitor
  : NonCopyable
{
  public:
    void visit_scene_pre(const Scene& scene) {}
    void visit_scene_post(const Scene& scene) {}

    void visit_assembly_instance_pre(
        const AssemblyInstance& assembly_instance)
    {
    }

    void visit_assembly_instance_post(
        const AssemblyInstance& assembly_instance)
    {
    }

    void visit_object_instance_pre(
        const ObjectInstance&   object_instance,
        const AssemblyInstance& assembly_instance)
    {
    }

    void visit_object_instance_post(
        const ObjectInstance&   object_instance,
        const AssemblyInstance& assembly_instance)
    {
    }

    void visit_object(const Object& object) {}

    void visit_material(const Material& material) {}

    void visit_light(
        const Light&            light,
        const AssemblyInstance& assembly_instance)
    {
    }
};

template <typename Visitor>
void visit_object_instance(
    const ObjectInstance&   object_instance,
    const AssemblyInstance& assembly_instance,
    Visitor&                v)
{
    v.visit_object_instance_pre(object_instance, assembly_instance);

    const Object& obj = object_instance.get_object();
    v.visit_object(obj);

    const MaterialArray& front_materials = object_instance.get_front_materials();
    for (size_t i = 0, e = front_materials.size(); i < e; ++i)
    {
        if (front_materials[i])
            v.visit_material(*front_materials[i]);
    }

    const MaterialArray& back_materials = object_instance.get_back_materials();
    for (size_t i = 0, e = back_materials.size(); i < e; ++i)
    {
        if (back_materials[i])
            v.visit_material(*back_materials[i]);
    }

    v.visit_object_instance_post(object_instance, assembly_instance);
}

template <typename Visitor>
void visit_assembly_instance(
    const AssemblyInstance& assembly_instance,
    Visitor&                v)
{
    v.visit_assembly_instance_pre(assembly_instance);

    const Assembly& assembly = assembly_instance.get_assembly();

    for (const ObjectInstance& i : assembly.object_instances())
        visit_object_instance(i, assembly_instance, v);

    for (const Light& i : assembly.lights())
        v.visit_light(i, assembly_instance);

    for (const AssemblyInstance& i : assembly.assembly_instances())
        visit_assembly_instance(i, v);

    v.visit_assembly_instance_post(assembly_instance);
}

template <typename Visitor>
void visit_scene(const Scene& scene, Visitor& v)
{
    v.visit_scene_pre(scene);

    for (const AssemblyInstance& i : scene.assembly_instances())
        visit_assembly_instance(i, v);

    v.visit_scene_post(scene);
}


//
// Entity conversion.
//

void convert_mesh_object(
    const MeshObject&   mesh,
    OptixContext&       context,
    optix::Geometry&    rt_geom,
    optix::Buffer&      vertices,
    optix::Buffer&      face_indices)
{
    // Alloc / realloc buffers.

    if (vertices.get())
        vertices->destroy();

    if (face_indices.get())
        face_indices->destroy();

    // todo: use the static tesselation inside the object.

    vertices = context.create_buffer(RT_BUFFER_INPUT, RT_FORMAT_FLOAT3, mesh.get_vertex_count());
    face_indices = context.create_buffer(RT_BUFFER_INPUT, RT_FORMAT_INT3, mesh.get_triangle_count());

    // Vertices.

    float* p = reinterpret_cast<float*>(vertices->map());

        for (size_t i = 0, e = mesh.get_vertex_count(); i < e; ++i)
        {
            const Vector3f& v = mesh.get_vertex(i);

            *p++ = static_cast<float>(v.x);
            *p++ = static_cast<float>(v.y);
            *p++ = static_cast<float>(v.z);
        }

    vertices->unmap();

    // todo: normals & uvs.

    // Face & material indices.

    int32_t* indx = reinterpret_cast<int32_t*>(face_indices->map());

        for (size_t i = 0, e = mesh.get_triangle_count(); i < e; ++i)
        {
            const Triangle& triangle = mesh.get_triangle(i);

            *indx++ = static_cast<int32_t>(triangle.m_v0);
            *indx++ = static_cast<int32_t>(triangle.m_v1);
            *indx++ = static_cast<int32_t>(triangle.m_v2);

        }

    face_indices->unmap();

/*
#if OPTIX_VERSION <= 50100
    rt_geom->setIntersectionProgram(context.get_mesh_object_intersect_program());
    rt_geom->setBoundingBoxProgram(context.get_mesh_object_bounds_program());
#endif
*/
    rt_geom->setPrimitiveCount(static_cast<unsigned int>(mesh.get_triangle_count()));
    rt_geom["vertices"]->setBuffer(vertices);
    rt_geom["face_indices"]->setBuffer(face_indices);
}

void convert_transform(const TransformSequence& seq, optix::Transform& xform)
{
    // todo: handle motion blur here...
    const Matrix4f m(seq.get_earliest_transform().get_local_to_parent());
    const Matrix4f m_inv(seq.get_earliest_transform().get_parent_to_local());
    xform->setMatrix(false, &m[0], &m_inv[0]);;
}


//
// OptiX scene build.
//

struct ObjectEntry
{
    ObjectEntry()
      : m_used(true)
      , m_version(~0)
    {
    }

    void create_optix_nodes(OptixContext& context, const Object& object)
    {
        RENDERER_LOG_DEBUG("      Creating OptiX nodes for object %s", object.get_name());

        m_geometry = context.create_geometry();
        m_accel = context.create_acceleration("Trbvh");

        if (strcmp(object.get_model(), MeshObjectFactory().get_model()) == 0)
        {
            m_accel->setProperty("vertex_buffer_name", "vertices");
            m_accel->setProperty("index_buffer_name", "face_indices");
        }
    }

    void update_optix_nodes(OptixContext& context, const Object& object)
    {
        if (m_version != object.get_version_id())
        {
            RENDERER_LOG_DEBUG("      Updating OptiX nodes for object %s", object.get_name());

            const MeshObject* mesh = dynamic_cast<const MeshObject*>(&object);
            convert_mesh_object(*mesh, context, m_geometry, m_vertices, m_face_indices);

            m_version = object.get_version_id();
        }
        else
            RENDERER_LOG_DEBUG("      OptiX nodes for object %s up to date. Skipping", object.get_name());
    }

    void destroy_optix_nodes(OptixContext& context)
    {
        RENDERER_LOG_DEBUG("Destroying OptiX object nodes");

        m_accel->destroy();
        m_geometry->destroy();

        if (m_vertices.get())
            m_vertices->destroy();

        if (m_face_indices.get())
            m_face_indices->destroy();
    }

    optix::Geometry         m_geometry;
    optix::Acceleration     m_accel;

    // Buffers.
    optix::Buffer           m_vertices;
    optix::Buffer           m_face_indices;

    bool                    m_used;
    VersionID               m_version;
};

typedef std::map<const Object*, ObjectEntry>  ObjectsMap;
typedef ObjectsMap::iterator                  ObjectsMapIter;

struct MaterialEntry
{
    MaterialEntry()
      : m_used(true)
      , m_version(~0)
    {
    }

    void create_optix_nodes(OptixContext& context, const Material& material)
    {
        RENDERER_LOG_DEBUG("      Creating OptiX nodes for material %s", material.get_name());
        m_material = context.create_material();
    }

    void update_optix_nodes(OptixContext& context, const Material& material)
    {
        if (m_version != material.get_version_id())
        {
            RENDERER_LOG_DEBUG("      Updating OptiX nodes for material %s", material.get_name());

            // todo: update obj here...
            m_version = material.get_version_id();
        }
        else
            RENDERER_LOG_DEBUG("      OptiX nodes for material %s up to date. Skipping", material.get_name());
    }

    void destroy_optix_nodes(OptixContext& context)
    {
        RENDERER_LOG_DEBUG("Destroying OptiX nodes for material");
        m_material->destroy();
    }

    optix::Material m_material;

    bool            m_used;
    VersionID       m_version;
};

typedef std::map<const Material*, MaterialEntry>  MaterialsMap;
typedef MaterialsMap::iterator                    MaterialsMapIter;

struct ObjectInstanceEntry
{
    ObjectInstanceEntry()
      : m_used(true)
      , m_version(~0)
    {
    }

    void create_optix_nodes(OptixContext& context, const ObjectInstance& object_instance, const ObjectEntry& object_entry)
    {
        RENDERER_LOG_DEBUG("    Creating OptiX nodes for object instance %s", object_instance.get_name());

        m_geom_instance = context.create_geometry_instance();

        m_geom_instance->setGeometry(object_entry.m_geometry);

        m_geom_instance->setMaterialCount(1);
        //m_geom_instance->setMaterial(0, context.get_facing_ratio_diagnostic_material());

        m_geom_group = context.create_geometry_group();
        m_geom_group->setChildCount(1);
        m_geom_group->setChild(0, m_geom_instance);
        m_geom_group->setAcceleration(object_entry.m_accel);

        m_xform = context.create_transform();
        m_xform->setChild(m_geom_group);

        context.get_scene()->addChild(m_xform);
    }

    void update_optix_nodes(
        OptixContext&                   context,
        const TransformSequenceStack&   xform_stack,
        const ObjectInstance&           object_instance)
    {
        // We always update the transform.
        convert_transform(xform_stack.top(), m_xform);
        //context.get_scene()->getAcceleration()->markDirty(); ???

        if (m_version != object_instance.get_version_id())
        {
            RENDERER_LOG_DEBUG("      Updating OptiX nodes for object instance %s", object_instance.get_name());

            // todo: update obj here...
            m_version = object_instance.get_version_id();
        }
        else
            RENDERER_LOG_DEBUG("      OptiX nodes for object instance %s up to date. Skipping", object_instance.get_name());
    }

    void destroy_optix_nodes(OptixContext& context)
    {
        RENDERER_LOG_DEBUG("Destroying OptiX object instance nodes");

        //context.get_scene()->removeChild(m_xform);
        //context.get_scene()->getAcceleration()->markDirty();

        m_geom_group->destroy();
        m_geom_instance->destroy();
        m_xform->destroy();
    }

    optix::GeometryInstance m_geom_instance;
    optix::GeometryGroup    m_geom_group;
    optix::Transform        m_xform;

    bool                    m_used;
    VersionID               m_version;
};

typedef std::pair<const AssemblyInstance*, const ObjectInstance*>   ObjectInstanceKey;
typedef std::map<ObjectInstanceKey, ObjectInstanceEntry>            ObjectInstancesMap;
typedef ObjectInstancesMap::iterator                                ObjectInstancesMapIter;

class FlagAsUsedVisitor
  : public SceneVisitor
{
  public:
    FlagAsUsedVisitor(
        ObjectsMap&          objects,
        MaterialsMap&        materials,
        ObjectInstancesMap&  object_instances)
      : m_objects(objects)
      , m_materials(materials)
      , m_object_instances(object_instances)
    {
    }

    void visit_object_instance_pre(
      const ObjectInstance&   object_instance,
      const AssemblyInstance& assembly_instance)
    {
        flag_used(
            ObjectInstanceKey(&assembly_instance, &object_instance),
            m_object_instances);
    }

    void visit_object(const Object& object)
    {
        flag_used(&object, m_objects);
    }

    void visit_material(const Material& material)
    {
        flag_used(&material, m_materials);
    }

  private:
    ObjectsMap&          m_objects;
    MaterialsMap&        m_materials;
    ObjectInstancesMap&  m_object_instances;

    template <typename Key, typename MapType>
    void flag_used(const Key& key, MapType& map)
    {
        auto it = map.find(key);

        if (it != map.end())
            it->second.m_used = true;
    }
};

class BuildSceneVisitor
  : public SceneVisitor
{
  public:
    BuildSceneVisitor(
        OptixContext&           context,
        TransformSequenceStack& xform_stack,
        ObjectsMap&             objects,
        MaterialsMap&           materials,
        ObjectInstancesMap&     object_instances)
      : m_context(context)
      , m_xform_stack(xform_stack)
      , m_objects(objects)
      , m_materials(materials)
      , m_object_instances(object_instances)
    {
    }

    void visit_scene_pre(const Scene& scene)
    {
        RENDERER_LOG_DEBUG("Init Scene build");

        m_xform_stack.clear();
    }

    void visit_scene_post(const Scene& scene)
    {
        RENDERER_LOG_DEBUG("Finish Scene build");
    }

    void visit_assembly_instance_pre(const AssemblyInstance& assembly_instance)
    {
        RENDERER_LOG_DEBUG("  Entering assembly instance %s", assembly_instance.get_name());

        m_xform_stack.push(assembly_instance.transform_sequence());
    }

    void visit_assembly_instance_post(const AssemblyInstance& assembly_instance)
    {
        m_xform_stack.pop();

        RENDERER_LOG_DEBUG("  Leaving assembly instance %s", assembly_instance.get_name());
    }

    void visit_object_instance_pre(
        const ObjectInstance&   object_instance,
        const AssemblyInstance& assembly_instance)
    {
        RENDERER_LOG_DEBUG("  Entering obj instance %s", object_instance.get_name());

        m_xform_stack.push(object_instance.get_transform());
    }

    void visit_object_instance_post(
        const ObjectInstance&   object_instance,
        const AssemblyInstance& assembly_instance)
    {
        const ObjectInstanceKey key(&assembly_instance, &object_instance);

        ObjectInstancesMapIter it = m_object_instances.find(key);

        if (it == m_object_instances.end())
        {
            it = m_object_instances.insert(std::make_pair(key, ObjectInstanceEntry())).first;

            Object& obj = object_instance.get_object();
            ObjectsMapIter obj_it = m_objects.find(&obj);

            it->second.create_optix_nodes(m_context, object_instance, obj_it->second);
        }

        it->second.update_optix_nodes(m_context, m_xform_stack, object_instance);

        m_xform_stack.pop();

        RENDERER_LOG_DEBUG("  Finishing obj instance %s", object_instance.get_name());
    }

    void visit_object(const Object& object)
    {
        RENDERER_LOG_DEBUG("    Visiting object %s", object.get_name());

        ObjectsMapIter it = m_objects.find(&object);

        if (it == m_objects.end())
        {
            it = m_objects.insert(std::make_pair(&object, ObjectEntry())).first;
            it->second.create_optix_nodes(m_context, object);
        }

        it->second.update_optix_nodes(m_context, object);
    }

    void visit_material(const Material& material)
    {
        RENDERER_LOG_DEBUG("    Visiting material %s", material.get_name());

        MaterialsMapIter it = m_materials.find(&material);

        if (it == m_materials.end())
        {
            it = m_materials.insert(std::make_pair(&material, MaterialEntry())).first;
            it->second.create_optix_nodes(m_context, material);
        }

        it->second.update_optix_nodes(m_context, material);
    }

    void visit_light(const Light& light, const AssemblyInstance& assembly_instance)
    {
        RENDERER_LOG_DEBUG("    Visiting light %s", light.get_name());
    }

  private:
    OptixContext&           m_context;
    TransformSequenceStack& m_xform_stack;

    ObjectsMap&             m_objects;
    MaterialsMap&           m_materials;
    ObjectInstancesMap&     m_object_instances;
};

}

//
// OptixTraceContext class implementation.
//

struct OptixTraceContext::Impl
{
    void flag_objects_unused()
    {
        flag_unused(m_objects);
        flag_unused(m_materials);
        flag_unused(m_object_instances);
    }

    void delete_unused(OptixContext& context)
    {
        delete_unused(m_objects, context);
        delete_unused(m_materials, context);
        delete_unused(m_object_instances, context);
    }

    ObjectsMap           m_objects;
    MaterialsMap         m_materials;
    ObjectInstancesMap   m_object_instances;

  private:
    template <typename MapType>
    void flag_unused(MapType& map)
    {
        for (auto it = map.begin(), e = map.end(); it != e; ++it)
            it->second.m_used = false;
    }

    template <typename MapType>
    void delete_unused(MapType& map, OptixContext& context)
    {
        for (auto it = map.begin(), e = map.end(); it != e; )
        {
            if (!it->second.m_used)
            {
                it->second.destroy_optix_nodes(context);
                it = map.erase(it);
            }
            else
                ++it;
        }
    }
};

OptixTraceContext::OptixTraceContext(
    const Scene&    scene,
    const int       device_number)
  : impl(new Impl)
  , m_scene(scene)
  , m_context(device_number)
{
    update();
}

OptixTraceContext::~OptixTraceContext()
{
    impl->flag_objects_unused();
    impl->delete_unused(m_context);

    delete impl;
}

void OptixTraceContext::update()
{
    // Do garbage collection.
    {
        impl->flag_objects_unused();

        FlagAsUsedVisitor v(impl->m_objects, impl->m_materials, impl->m_object_instances);
        visit_scene(m_scene, v);

        impl->delete_unused(m_context);
    }

    // Build / update the OptiX context.
    {
        TransformSequenceStack xform_stack;

        BuildSceneVisitor v(
            m_context,
            xform_stack,
            impl->m_objects,
            impl->m_materials,
            impl->m_object_instances);
        visit_scene(m_scene, v);
    }
}

void OptixTraceContext::validate() const
{
    m_context.validate();
}

}   // namespace renderer
