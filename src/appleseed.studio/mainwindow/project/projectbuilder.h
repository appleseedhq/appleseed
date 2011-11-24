
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTBUILDER_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTBUILDER_H

// appleseed.studio headers.
#include "mainwindow/project/assemblycollectionitem.h"
#include "mainwindow/project/assemblyitem.h"
#include "mainwindow/project/multimodelentityeditorformfactory.h"
#include "mainwindow/project/projecttree.h"

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"
#include "renderer/api/camera.h"
#include "renderer/api/color.h"
#include "renderer/api/edf.h"
#include "renderer/api/entity.h"
#include "renderer/api/environment.h"
#include "renderer/api/environmentedf.h"
#include "renderer/api/environmentshader.h"
#include "renderer/api/light.h"
#include "renderer/api/material.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QObject>

// Standard headers.
#include <cassert>
#include <string>

// Forward declarations.
namespace appleseed { namespace studio { class ProjectTree; } }
namespace renderer  { class Assembly; }
namespace renderer  { class AssemblyInstance; }
namespace renderer  { class Project; }
namespace renderer  { class Scene; }

namespace appleseed {
namespace studio {

class ProjectBuilder
  : public QObject
  , foundation::NonCopyable
{
    Q_OBJECT

  public:
    ProjectBuilder(
        renderer::Project&                  project,
        ProjectTree&                        project_tree);

    template <typename Entity>
    const typename renderer::EntityTraits<Entity>::FactoryRegistrarType&
        get_factory_registrar() const;

    void notify_project_modification() const;

    template <typename Entity, typename ParentEntity>
    void insert_entity(
        ParentEntity&                       parent,
        const foundation::Dictionary&       values) const;

    template <typename Entity, typename ParentEntity>
    void remove_entity(
        Entity*                             entity,
        ParentEntity&                       parent) const;

    template <typename Entity, typename ParentEntity>
    Entity* edit_entity(
        Entity*                             old_entity,
        ParentEntity&                       parent,
        const foundation::Dictionary&       values) const;

    // Simulate partial specialization of edit_entity() for Entity = renderer::TextureInstance.
    template <typename ParentEntity>
    renderer::TextureInstance* edit_entity(
        renderer::TextureInstance*          old_entity,
        ParentEntity&                       parent,
        const foundation::Dictionary&       values) const;

    void insert_assembly(
        const std::string&                  name) const;

    void remove_assembly(
        const foundation::UniqueID          assembly_id) const;

    void insert_assembly_instance(
        const std::string&                  name,
        renderer::Assembly&                 assembly) const;

    void remove_assembly_instance(
        const foundation::UniqueID          assembly_instance_id) const;

    void insert_objects(
        renderer::Assembly&                 assembly,
        const std::string&                  path) const;

    void remove_object(
        renderer::Assembly&                 assembly,
        const foundation::UniqueID          object_id) const;

    void remove_object_instance(
        renderer::Assembly&                 assembly,
        const foundation::UniqueID          object_instance_id) const;

    void insert_textures(
        renderer::Assembly&                 assembly,
        const std::string&                  path) const;

    void insert_textures(
        const std::string&                  path) const;

  signals:
    void signal_project_modified() const;

  private:
    renderer::Project&                              m_project;
    ProjectTree&                                    m_project_tree;

    renderer::CameraFactoryRegistrar                m_camera_factory_registrar;
    renderer::BSDFFactoryRegistrar                  m_bsdf_factory_registrar;
    renderer::EDFFactoryRegistrar                   m_edf_factory_registrar;
    renderer::EnvironmentEDFFactoryRegistrar        m_environment_edf_factory_registrar;
    renderer::EnvironmentShaderFactoryRegistrar     m_environment_shader_factory_registrar;
    renderer::LightFactoryRegistrar                 m_light_factory_registrar;
    renderer::SurfaceShaderFactoryRegistrar         m_surface_shader_factory_registrar;

    static std::string get_entity_name(const foundation::Dictionary& values);

    static bool is_valid_entity_name(const std::string& name);

    template <typename Entity>
    foundation::auto_release_ptr<Entity> create_entity(
        const foundation::Dictionary&       values) const;

    template <typename Entity>
    void add_item(
        Entity*                             entity,
        renderer::Scene&                    scene) const;

    template <typename Entity>
    void add_item(
        Entity*                             entity,
        renderer::Assembly&                 assembly) const;
};


//
// ProjectBuilder class implementation.
//

template <>
inline const renderer::EntityTraits<renderer::Camera>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::Camera>() const
{
    return m_camera_factory_registrar;
}

template <>
inline const renderer::EntityTraits<renderer::BSDF>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::BSDF>() const
{
    return m_bsdf_factory_registrar;
}

template <>
inline const renderer::EntityTraits<renderer::EDF>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::EDF>() const
{
    return m_edf_factory_registrar;
}

template <>
inline const renderer::EntityTraits<renderer::EnvironmentEDF>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::EnvironmentEDF>() const
{
    return m_environment_edf_factory_registrar;
}

template <>
inline const renderer::EntityTraits<renderer::EnvironmentShader>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::EnvironmentShader>() const
{
    return m_environment_shader_factory_registrar;
}

template <>
inline const renderer::EntityTraits<renderer::Light>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::Light>() const
{
    return m_light_factory_registrar;
}

template <>
inline const renderer::EntityTraits<renderer::SurfaceShader>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::SurfaceShader>() const
{
    return m_surface_shader_factory_registrar;
}

template <typename Entity, typename ParentEntity>
void ProjectBuilder::insert_entity(
    ParentEntity&                       parent,
    const foundation::Dictionary&       values) const
{
    foundation::auto_release_ptr<Entity> entity(create_entity<Entity>(values));

    add_item(entity.get(), parent);

    renderer::EntityTraits<Entity>::insert_entity(entity, parent);

    notify_project_modification();
}

template <typename Entity, typename ParentEntity>
void ProjectBuilder::remove_entity(
    Entity*                             entity,
    ParentEntity&                       parent) const
{
    renderer::EntityTraits<Entity>::remove_entity(entity, parent);

    notify_project_modification();
}

template <typename Entity, typename ParentEntity>
Entity* ProjectBuilder::edit_entity(
    Entity*                             old_entity,
    ParentEntity&                       parent,
    const foundation::Dictionary&       values) const
{
    foundation::auto_release_ptr<Entity> new_entity(create_entity<Entity>(values));
    Entity* new_entity_ptr = new_entity.get();

    renderer::EntityTraits<Entity>::remove_entity(old_entity, parent);
    renderer::EntityTraits<Entity>::insert_entity(new_entity, parent);

    notify_project_modification();

    return new_entity_ptr;
}

// Specialize edit_entity() for cameras because we need to carry over
// the camera's transform sequence when we replace one camera by another.
template <>
inline renderer::Camera* ProjectBuilder::edit_entity(
    renderer::Camera*                   old_entity,
    renderer::Scene&                    parent,
    const foundation::Dictionary&       values) const
{
    foundation::auto_release_ptr<renderer::Camera> new_entity(create_entity<renderer::Camera>(values));
    renderer::Camera* new_entity_ptr = new_entity.get();

    new_entity->transform_sequence().clear();

    for (size_t i = 0; i < old_entity->transform_sequence().size(); ++i)
    {
        double time;
        foundation::Transformd transform;
        old_entity->transform_sequence().get_transform(i, time, transform);
        new_entity->transform_sequence().set_transform(time, transform);
    }

    renderer::EntityTraits<renderer::Camera>::remove_entity(old_entity, parent);
    renderer::EntityTraits<renderer::Camera>::insert_entity(new_entity, parent);

    notify_project_modification();

    return new_entity_ptr;
}

template <typename ParentEntity>
inline renderer::TextureInstance* ProjectBuilder::edit_entity(
    renderer::TextureInstance*          old_entity,
    ParentEntity&                       parent,
    const foundation::Dictionary&       values) const
{
    const size_t texture_index = old_entity->get_texture_index();
    const std::string name = get_entity_name(values);

    foundation::Dictionary clean_values(values);
    clean_values.strings().remove(EntityEditorFormFactoryBase::NameParameter);

    foundation::auto_release_ptr<renderer::TextureInstance> new_entity(
        renderer::TextureInstanceFactory::create(
            name.c_str(),
            clean_values,
            texture_index));
    renderer::TextureInstance* new_entity_ptr = new_entity.get();

    renderer::EntityTraits<renderer::TextureInstance>::remove_entity(old_entity, parent);
    renderer::EntityTraits<renderer::TextureInstance>::insert_entity(new_entity, parent);

    notify_project_modification();

    return new_entity_ptr;
}

template <typename Entity>
foundation::auto_release_ptr<Entity> ProjectBuilder::create_entity(
    const foundation::Dictionary&       values) const
{
    typedef typename renderer::EntityTraits<Entity>::FactoryRegistrarType FactoryRegistrarType;
    typedef typename FactoryRegistrarType::FactoryType FactoryType;
    typedef MultiModelEntityEditorFormFactory<FactoryRegistrarType> EntityEditorFormFactoryType;

    const std::string name = get_entity_name(values);
    const std::string model = values.get<std::string>(EntityEditorFormFactoryType::ModelParameter);

    foundation::Dictionary clean_values(values);
    clean_values.strings().remove(EntityEditorFormFactoryType::NameParameter);
    clean_values.strings().remove(EntityEditorFormFactoryType::ModelParameter);

    const FactoryRegistrarType& factory_registrar = get_factory_registrar<Entity>();
    const FactoryType* factory = factory_registrar.lookup(model.c_str());
    assert(factory);

    return factory->create(name.c_str(), clean_values);
}

template <>
inline foundation::auto_release_ptr<renderer::ColorEntity> ProjectBuilder::create_entity(
    const foundation::Dictionary&       values) const
{
    const std::string name = get_entity_name(values);

    foundation::Dictionary clean_values(values);
    clean_values.strings().remove(EntityEditorFormFactoryBase::NameParameter);

    return renderer::ColorEntityFactory::create(name.c_str(), clean_values);
}

template <>
inline foundation::auto_release_ptr<renderer::TextureInstance> ProjectBuilder::create_entity(
    const foundation::Dictionary&       values) const
{
    static const char* TextureIndexParameter = "__texture_index";

    const std::string name = get_entity_name(values);
    const size_t texture_index = values.get<size_t>(TextureIndexParameter);

    foundation::Dictionary clean_values(values);
    clean_values.strings().remove(EntityEditorFormFactoryBase::NameParameter);
    clean_values.strings().remove(TextureIndexParameter);

    return
        renderer::TextureInstanceFactory::create(
            name.c_str(),
            clean_values,
            texture_index);
}

template <>
inline foundation::auto_release_ptr<renderer::Environment> ProjectBuilder::create_entity(
    const foundation::Dictionary&       values) const
{
    const std::string name = get_entity_name(values);

    foundation::Dictionary clean_values(values);
    clean_values.strings().remove(EntityEditorFormFactoryBase::NameParameter);

    return renderer::EnvironmentFactory::create(name.c_str(), clean_values);
}

template <>
inline foundation::auto_release_ptr<renderer::Material> ProjectBuilder::create_entity(
    const foundation::Dictionary&       values) const
{
    const std::string name = get_entity_name(values);

    foundation::Dictionary clean_values(values);
    clean_values.strings().remove(EntityEditorFormFactoryBase::NameParameter);

    return renderer::MaterialFactory::create(name.c_str(), clean_values);
}

template <typename Entity>
void ProjectBuilder::add_item(
    Entity*                             entity,
    renderer::Scene&                    scene) const
{
    m_project_tree.add_item(entity);
}

template <typename Entity>
void ProjectBuilder::add_item(
    Entity*                             entity,
    renderer::Assembly&                 assembly) const
{
    m_project_tree.get_assembly_collection_item()
                  .get_item(assembly)
                  .add_item(entity);
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTBUILDER_H
