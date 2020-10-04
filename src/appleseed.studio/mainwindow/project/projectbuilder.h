
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

#pragma once

// appleseed.studio headers.
#include "mainwindow/project/multimodelentityeditorformfactory.h"

// appleseed.renderer headers.
#include "renderer/api/camera.h"
#include "renderer/api/color.h"
#include "renderer/api/entity.h"
#include "renderer/api/environment.h"
#include "renderer/api/environmentedf.h"
#include "renderer/api/light.h"
#include "renderer/api/postprocessing.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/shadergroup.h"
#include "renderer/api/texture.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/transform.h"
#include "foundation/memory/autoreleaseptr.h"

// Qt headers.
#include <QObject>

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>

// Forward declarations.
namespace renderer  { class Frame; }

namespace appleseed {
namespace studio {

class ProjectBuilder
  : public QObject
  , public foundation::NonCopyable
{
    Q_OBJECT

  public:
    explicit ProjectBuilder(renderer::Project& project);

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

    // Simulate partial specialization of edit_entity() for Entity = renderer::PostProcessingStage.
    template <typename ParentEntity>
    renderer::PostProcessingStage* edit_entity(
        renderer::PostProcessingStage*      old_entity,
        ParentEntity&                       parent,
        const foundation::Dictionary&       values) const;

    renderer::Frame* edit_frame(
        const foundation::Dictionary&       values) const;

  signals:
    void signal_project_modified() const;
    void signal_frame_modified() const;
    void signal_post_processing_stage_modified(const std::uint64_t stage_uid) const;

  public slots:
    void slot_notify_project_modification() const;

  private:
    renderer::Project& m_project;

    static std::string get_entity_name(const foundation::Dictionary& values);

    static bool is_valid_entity_name(const std::string& name);

    template <typename Entity>
    foundation::auto_release_ptr<Entity> create_entity(
        const foundation::Dictionary&       values) const;
};


//
// ProjectBuilder class implementation.
//

template <typename Entity, typename ParentEntity>
Entity* ProjectBuilder::insert_entity(
    ParentEntity&                       parent,
    const foundation::Dictionary&       values) const
{
    foundation::auto_release_ptr<Entity> entity(create_entity<Entity>(values));
    Entity* entity_ptr = entity.get();

    renderer::EntityTraits<Entity>::insert_entity(entity, parent);

    slot_notify_project_modification();

    return entity_ptr;
}

template <typename Entity, typename ParentEntity>
void ProjectBuilder::remove_entity(
    Entity*                             entity,
    ParentEntity&                       parent) const
{
    renderer::EntityTraits<Entity>::remove_entity(entity, parent);

    slot_notify_project_modification();
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

    slot_notify_project_modification();

    return new_entity_ptr;
}

template <typename Entity>
inline void copy_transform_sequence(
    const Entity&                       src_entity,
    const Entity&                       dst_entity)
{
    dst_entity->transform_sequence().clear();

    for (size_t i = 0, e = src_entity->transform_sequence().size(); i < e; ++i)
    {
        float time;
        foundation::Transformd transform;
        src_entity->transform_sequence().get_transform(i, time, transform);
        dst_entity->transform_sequence().set_transform(time, transform);
    }
}

template <>
inline renderer::EnvironmentEDF* ProjectBuilder::edit_entity(
    renderer::EnvironmentEDF*           old_entity,
    renderer::Scene&                    parent,
    const foundation::Dictionary&       values) const
{
    foundation::auto_release_ptr<renderer::EnvironmentEDF> new_entity(create_entity<renderer::EnvironmentEDF>(values));
    renderer::EnvironmentEDF* new_entity_ptr = new_entity.get();

    copy_transform_sequence(old_entity, new_entity_ptr);

    renderer::EntityTraits<renderer::EnvironmentEDF>::remove_entity(old_entity, parent);
    renderer::EntityTraits<renderer::EnvironmentEDF>::insert_entity(new_entity, parent);

    slot_notify_project_modification();

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

    copy_transform_sequence(old_entity, new_entity_ptr);

    renderer::EntityTraits<renderer::Camera>::remove_entity(old_entity, parent);
    renderer::EntityTraits<renderer::Camera>::insert_entity(new_entity, parent);

    slot_notify_project_modification();

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
    clean_values.strings().remove(EntityEditorFormFactoryBase::NameParameter.c_str());

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
    slot_notify_project_modification();

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
    clean_values.strings().remove(EntityEditorFormFactoryBase::NameParameter.c_str());

    foundation::auto_release_ptr<renderer::TextureInstance> new_entity(
        renderer::TextureInstanceFactory::create(
            name.c_str(),
            clean_values,
            texture_name.c_str(),
            old_entity->get_transform()));
    renderer::TextureInstance* new_entity_ptr = new_entity.get();

    renderer::EntityTraits<renderer::TextureInstance>::remove_entity(old_entity, parent);
    renderer::EntityTraits<renderer::TextureInstance>::insert_entity(new_entity, parent);

    slot_notify_project_modification();

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

    slot_notify_project_modification();

    return new_entity_ptr;
}

template <typename ParentEntity>
inline renderer::PostProcessingStage* ProjectBuilder::edit_entity(
    renderer::PostProcessingStage*      old_entity,
    ParentEntity&                       parent,
    const foundation::Dictionary&       values) const
{
    foundation::auto_release_ptr<renderer::PostProcessingStage> new_entity(
        create_entity<renderer::PostProcessingStage>(values));
    renderer::PostProcessingStage* new_entity_ptr = new_entity.get();

    renderer::EntityTraits<renderer::PostProcessingStage>::remove_entity(old_entity, parent);
    renderer::EntityTraits<renderer::PostProcessingStage>::insert_entity(new_entity, parent);

    slot_notify_project_modification();

    // @Note: we need to know whether or not to emit the signal. For this,
    // we can either:
    //   * change the `PostProcessingStage` class to have a `bool` flag (not good)
    //   * pass this flag in its `params`, so that we can retrieve it here (maybe?)
    //   * pass the flag through `values` instead (not sure if it'd be better or not)
    //
    // @Fixme: if the flag was true and is now false, the effect needs to be "unapplied"!

#if 0
    // @Note: using this to be able to inspect Dictionary values in Visual Studio.. :(
    bool preview_enabled = false;
    for (const auto& str : values.strings())
    {
        const auto key = std::string(str.key());
        const auto value = std::string(str.value());
        if (key == "real_time_preview")
            preview_enabled = true;
        if (value == "real_time_preview")
            preview_enabled = true;
    }
#else
    const bool preview_enabled = new_entity_ptr->get_parameters().get_required<bool>("real_time_preview");
#endif

    // Signal the modified stage, so that it can be previewed.
    if (preview_enabled)
        emit signal_post_processing_stage_modified(new_entity_ptr->get_uid());

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
    const std::string model = values.get<std::string>(EntityEditorFormFactoryType::ModelParameter.c_str());

    foundation::Dictionary clean_values(values);
    clean_values.strings().remove(EntityEditorFormFactoryType::NameParameter.c_str());
    clean_values.strings().remove(EntityEditorFormFactoryType::ModelParameter.c_str());

    const FactoryRegistrarType& factory_registrar = m_project.get_factory_registrar<Entity>();
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
    clean_values.strings().remove(EntityEditorFormFactoryBase::NameParameter.c_str());

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
    const std::string model = values.get<std::string>(EntityEditorFormFactoryType::ModelParameter.c_str());

    foundation::Dictionary clean_values(values);
    clean_values.strings().remove(EntityEditorFormFactoryType::NameParameter.c_str());
    clean_values.strings().remove(EntityEditorFormFactoryType::ModelParameter.c_str());

    const FactoryRegistrarType& factory_registrar = m_project.get_factory_registrar<renderer::Texture>();
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
    clean_values.strings().remove(EntityEditorFormFactoryBase::NameParameter.c_str());

    return renderer::EnvironmentFactory::create(name.c_str(), clean_values);
}

template <>
inline foundation::auto_release_ptr<renderer::ShaderGroup> ProjectBuilder::create_entity(
    const foundation::Dictionary&       values) const
{
    const std::string name = get_entity_name(values);

    foundation::Dictionary clean_values(values);
    clean_values.strings().remove(EntityEditorFormFactoryBase::NameParameter.c_str());

    return renderer::ShaderGroupFactory::create(name.c_str(), clean_values);
}

}   // namespace studio
}   // namespace appleseed
