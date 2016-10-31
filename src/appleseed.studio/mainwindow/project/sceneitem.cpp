
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

// Interface header.
#include "sceneitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblycollectionitem.h"
#include "mainwindow/project/assemblyinstanceitem.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/instancecollectionitem.h"
#include "mainwindow/project/itemregistry.h"
#include "mainwindow/project/multimodelcollectionitem.h"
#include "mainwindow/project/singlemodelcollectionitem.h"
#include "mainwindow/project/texturecollectionitem.h"

// appleseed.renderer headers.
#include "renderer/api/camera.h"
#include "renderer/api/entity.h"
#include "renderer/api/environment.h"
#include "renderer/api/environmentedf.h"
#include "renderer/api/environmentshader.h"
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Qt headers.
#include <QFont>
#include <QMenu>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

namespace
{
    const UniqueID g_class_uid = new_guid();
}

SceneItem::SceneItem(
    EntityEditorContext&    editor_context,
    Scene&                  scene)
  : BaseGroupItem(editor_context, g_class_uid, "Scene", scene)
{
    set_allow_deletion(false);
    set_allow_edition(false);

    QFont font;
    font.setBold(true);
    setFont(0, font);

    int child_index = 0;
    insertChild(
        child_index++,
        m_camera_collection_item =
            new MultiModelCollectionItem<Camera, Scene, SceneItem>(
                editor_context,
                new_guid(),
                EntityTraits<Camera>::get_human_readable_collection_type_name(),
                scene,
                this));
    m_camera_collection_item->add_items(scene.cameras());

    insertChild(
        child_index++,
        m_environment_item =
            new EnvironmentItem(
                editor_context,
                scene.get_environment(),
                scene,
                this));
    m_environment_item->set_allow_deletion(false);
    m_environment_item->set_fixed_position(true);
    editor_context.m_item_registry.insert(*scene.get_environment(), m_environment_item);

    insertChild(
        child_index++,
        m_environment_edf_collection_item =
            new MultiModelCollectionItem<EnvironmentEDF, Scene, SceneItem>(
                editor_context,
                new_guid(),
                EntityTraits<EnvironmentEDF>::get_human_readable_collection_type_name(),
                scene,
                this));
    m_environment_edf_collection_item->add_items(scene.environment_edfs());

    insertChild(
        child_index++,
        m_environment_shader_collection_item =
            new MultiModelCollectionItem<EnvironmentShader, Scene, SceneItem>(
                editor_context,
                new_guid(),
                EntityTraits<EnvironmentShader>::get_human_readable_collection_type_name(),
                scene,
                this));
    m_environment_shader_collection_item->add_items(scene.environment_shaders());
}

void SceneItem::expand()
{
    setExpanded(true);

    get_assembly_collection_item().setExpanded(true);
    get_assembly_instance_collection_item().setExpanded(true);

    if (get_assembly_collection_item().childCount() == 1)
        get_assembly_collection_item().child(0)->setExpanded(true);
}

QMenu* SceneItem::get_single_item_context_menu() const
{
    QMenu* menu = ItemBase::get_single_item_context_menu();

    menu->addSeparator();
    menu->addAction("Import Textures...", &get_texture_collection_item(), SLOT(slot_import_textures()));

    menu->addSeparator();
    menu->addAction("Create Assembly...", &get_assembly_collection_item(), SLOT(slot_create()));
    menu->addAction("Create Camera...", m_camera_collection_item, SLOT(slot_create()));
    menu->addAction("Create Color...", &get_color_collection_item(), SLOT(slot_create()));
    menu->addAction("Create Environment EDF...", m_environment_edf_collection_item, SLOT(slot_create()));
    menu->addAction("Create Environment Shader...", m_environment_shader_collection_item, SLOT(slot_create()));

    return menu;
}

void SceneItem::add_item(Camera* camera)
{
    m_camera_collection_item->add_item(camera);
}

void SceneItem::add_item(EnvironmentEDF* environment_edf)
{
    m_environment_edf_collection_item->add_item(environment_edf);
}

void SceneItem::add_item(EnvironmentShader* environment_shader)
{
    m_environment_shader_collection_item->add_item(environment_shader);
}

}   // namespace studio
}   // namespace appleseed
