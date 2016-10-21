
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "mainwindow/project/multimodelentityeditorformfactory.h"

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"
#include "renderer/api/bssrdf.h"
#include "renderer/api/camera.h"
#include "renderer/api/color.h"
#include "renderer/api/edf.h"
#include "renderer/api/entity.h"
#include "renderer/api/environment.h"
#include "renderer/api/environmentedf.h"
#include "renderer/api/environmentshader.h"
#include "renderer/api/light.h"
#include "renderer/api/material.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"
#include "renderer/api/texture.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/transform.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QObject>

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>

namespace appleseed {
namespace studio {

class ProjectBuilder
  : public QObject
  , public foundation::NonCopyable
{
    Q_OBJECT

  public:
    explicit ProjectBuilder(renderer::Project& project);

    template <typename Entity>
    const typename renderer::EntityTraits<Entity>::FactoryRegistrarType& get_factory_registrar() const;

    void notify_project_modification() const;

    template <typename Entity, typename ParentEntity>
    Entity* insert_entity(
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

    renderer::Frame* edit_frame(
        const foundation::Dictionary&       values) const;

  signals:
    void signal_project_modified() const;
    void signal_frame_modified() const;

  private:
    renderer::Project&                              m_project;

    renderer::BSDFFactoryRegistrar                  m_bsdf_factory_registrar;
    renderer::BSSRDFFactoryRegistrar                m_bssrdf_factory_registrar;
    renderer::CameraFactoryRegistrar                m_camera_factory_registrar;
    renderer::EDFFactoryRegistrar                   m_edf_factory_registrar;
    renderer::EnvironmentEDFFactoryRegistrar        m_environment_edf_factory_registrar;
    renderer::EnvironmentShaderFactoryRegistrar     m_environment_shader_factory_registrar;
    renderer::LightFactoryRegistrar                 m_light_factory_registrar;
    renderer::MaterialFactoryRegistrar              m_material_factory_registrar;
    renderer::RenderLayerRuleFactoryRegistrar       m_render_layer_rule_factory_registrar;
    renderer::SurfaceShaderFactoryRegistrar         m_surface_shader_factory_registrar;
    renderer::TextureFactoryRegistrar               m_texture_factory_registrar;

    static std::string get_entity_name(const foundation::Dictionary& values);

    static bool is_valid_entity_name(const std::string& name);

    template <typename Entity>
    foundation::auto_release_ptr<Entity> create_entity(
        const foundation::Dictionary&       values) const;
};


//
// ProjectBuilder class implementation.
//

template <>
inline const renderer::EntityTraits<renderer::BSDF>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::BSDF>() const
{
    return m_bsdf_factory_registrar;
}

template <>
inline const renderer::EntityTraits<renderer::BSSRDF>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::BSSRDF>() const
{
    return m_bssrdf_factory_registrar;
}

template <>
inline const renderer::EntityTraits<renderer::Camera>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::Camera>() const
{
    return m_camera_factory_registrar;
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
inline const renderer::EntityTraits<renderer::Material>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::Material>() const
{
    return m_material_factory_registrar;
}

template <>
inline const renderer::EntityTraits<renderer::RenderLayerRule>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::RenderLayerRule>() const
{
    return m_render_layer_rule_factory_registrar;
}

template <>
inline const renderer::EntityTraits<renderer::SurfaceShader>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::SurfaceShader>() const
{
    return m_surface_shader_factory_registrar;
}

template <>
inline const renderer::EntityTraits<renderer::Texture>::FactoryRegistrarType&
ProjectBuilder::get_factory_registrar<renderer::Texture>() const
{
    return m_texture_factory_registrar;
}

template <typename Entity, typename ParentEntity>
Entity* ProjectBuilder::insert_entity(
    ParentEntity&                       parent,
    const foundation::Dictionary&       values) const
{
    foundation::auto_release_ptr<Entity> entity(create_entity<Entity>(values));
    Entity* entity_ptr = entity.get();

    renderer::EntityTraits<Entity>::insert_entity(entity, parent);

    notify_project_modification();

    return entity_ptr;
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

template <>
inline renderer::Camera* ProjectBuilder::edit_entity(
    renderer::Camera*                   old_entity,
    renderer::Scene&                    parent,
    const foundation::Dictionary&       values) const
{
    foundation::auto_release_ptr<renderer::Camera> new_entity(create_entity<renderer::Camera>(values));
    renderer::Camera* new_entity_ptr = new_entity.get();

    new_entity->transform_sequence().clear();

    for (size_t i = 0, e = old_entity->transform_sequence().size(); i < e; ++i)
    {
        float time;
        foundation::Transformd transform;
        old_entity->transform_sequence().get_transform(i, time, transform);
        new_entity->transform_sequence().set_transform(time, transform);
    }

    renderer::EntityTraits<renderer::Camera>::remove_entity(old_entity, parent);
    renderer::EntityTraits<renderer::Camera>::insert_entity(new_entity, parent);

    notify_project_modification();

    return new_entity_ptr;
}

template <>
inline renderer::ObjectInstance* ProjectBuilder::edit_entity(
    renderer::ObjectInstance*           old_entity,
    renderer::Assembly&                 parent,
    const foundation::Dictionary&       values) const
{
    const std::string object_name = old_entity->get_object_name();
    const foundation::Transformd transform = old_entity->get_transform();
    const foundation::StringDictionary front_material_mappings = old_entity->get_front_material_mappings();
    const foundation::StringDictionary back_material_mappings = old_entity->get_back_material_mappings();
    const std::string name = get_entity_name(values);

    foundation::Dictionary clean_values(values);
    clean_values.strings().remove(EntityEditorFormFactoryBase::NameParameter);

    foundation::auto_release_ptr<renderer::ObjectInstance> new_entity(
        renderer::ObjectInstanceFactory::create(
            name.c_str(),
            clean_values,
            object_name.c_str(),
            transform,
            front_material_mappings,
            back_material_mappings));
    renderer::ObjectInstance* new_entity_ptr = new_entity.get();

    renderer::EntityTraits<renderer::ObjectInstance>::remove_entity(old_entity, parent);
    renderer::EntityTraits<renderer::ObjectInstance>::insert_entity(new_entity, parent);

    parent.bump_version_id();
    notify_project_modification();

    return new_entity_ptr;
}

template <typename ParentEntity>
inline renderer::TextureInstance* ProjectBuilder::edit_entity(
    renderer::TextureInstance*          old_entity,
    ParentEntity&                       parent,
    const foundation::Dictionary&       values) const
{
    const std::string texture_name = old_entity->get_texture_name();
    const std::string name = get_entity_name(values);

    foundation::Dictionary clean_values(values);
    clean_values.strings().remove(EntityEditorFormFactoryBase::NameParameter);

    foundation::auto_release_ptr<renderer::TextureInstance> new_entity(
        renderer::TextureInstanceFactory::create(
            name.c_str(),
            clean_values,
            texture_name.c_str(),
            old_entity->get_transform()));
    renderer::TextureInstance* new_entity_ptr = new_entity.get();

    renderer::EntityTraits<renderer::TextureInstance>::remove_entity(old_entity, parent);
    renderer::EntityTraits<renderer::TextureInstance>::insert_entity(new_entity, parent);

    notify_project_modification();

    return new_entity_ptr;
}

template <>
inline renderer::Light* ProjectBuilder::edit_entity(
    renderer::Light*                    old_entity,
    renderer::Assembly&                 parent,
    const foundation::Dictionary&       values) const
{
    foundation::auto_release_ptr<renderer::Light> new_entity(create_entity<renderer::Light>(values));
    renderer::Light* new_entity_ptr = new_entity.get();

    new_entity->set_transform(old_entity->get_transform());

    renderer::EntityTraits<renderer::Light>::remove_entity(old_entity, parent);
    renderer::EntityTraits<renderer::Light>::insert_entity(new_entity, parent);

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
inline foundation::auto_release_ptr<renderer::Texture> ProjectBuilder::create_entity(
    const foundation::Dictionary&       values) const
{
    typedef renderer::EntityTraits<renderer::Texture>::FactoryRegistrarType FactoryRegistrarType;
    typedef FactoryRegistrarType::FactoryType FactoryType;
    typedef MultiModelEntityEditorFormFactory<FactoryRegistrarType> EntityEditorFormFactoryType;

    const std::string name = get_entity_name(values);
    const std::string model = values.get<std::string>(EntityEditorFormFactoryType::ModelParameter);

    foundation::Dictionary clean_values(values);
    clean_values.strings().remove(EntityEditorFormFactoryType::NameParameter);
    clean_values.strings().remove(EntityEditorFormFactoryType::ModelParameter);

    const FactoryRegistrarType& factory_registrar = get_factory_registrar<renderer::Texture>();
    const FactoryType* factory = factory_registrar.lookup(model.c_str());
    assert(factory);

    return factory->create(name.c_str(), clean_values, m_project.search_paths());
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

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTBUILDER_H
