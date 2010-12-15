
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
#include "assemblyitem.h"

// appleseed.studio headers.
#include "mainwindow/project/bsdfcollectionitem.h"
#include "mainwindow/project/colorcollectionitem.h"
#include "mainwindow/project/edfcollectionitem.h"
#include "mainwindow/project/lightcollectionitem.h"
#include "mainwindow/project/materialcollectionitem.h"
#include "mainwindow/project/objectcollectionitem.h"
#include "mainwindow/project/objectinstancecollectionitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/surfaceshadercollectionitem.h"
#include "mainwindow/project/texturecollectionitem.h"
#include "mainwindow/project/textureinstancecollectionitem.h"
#include "mainwindow/project/tools.h"

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"
#include "renderer/api/color.h"
#include "renderer/api/edf.h"
#include "renderer/api/light.h"
#include "renderer/api/material.h"
#include "renderer/api/object.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"
#include "renderer/api/texture.h"

// Qt headers.
#include <QFileDialog>
#include <QMenu>
#include <QString>
#include <QStringList>

// Standard headers.
#include <string>

using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

AssemblyItem::AssemblyItem(
    Scene&          scene,
    Assembly&       assembly,
    ProjectBuilder& project_builder)
  : EntityItemBase(assembly)
  , m_scene(scene)
  , m_assembly(assembly)
  , m_project_builder(project_builder)
{
    m_color_collection_item = add_collection_item(assembly.colors());
    m_texture_collection_item = add_collection_item(assembly.textures());
    m_texture_instance_collection_item = add_collection_item(assembly.texture_instances());
    m_bsdf_collection_item = add_collection_item(assembly.bsdfs());
    m_edf_collection_item = add_collection_item(assembly.edfs());
    m_surface_shader_collection_item = add_collection_item(assembly.surface_shaders());
    m_material_collection_item = add_collection_item(assembly.materials());
    m_light_collection_item = add_collection_item(assembly.lights());
    m_object_collection_item = add_collection_item(assembly.objects());
    m_object_instance_collection_item = add_collection_item(assembly.object_instances());
}

QMenu* AssemblyItem::get_single_item_context_menu() const
{
    QMenu* menu = EntityItemBase::get_single_item_context_menu();
    menu->addSeparator();
    menu->addAction("Instantiate...", this, SLOT(slot_instantiate()));
    menu->addSeparator();
    menu->addAction("Import Objects...", m_object_collection_item, SLOT(slot_import_objects()));
    menu->addAction("Import Textures...", m_texture_collection_item, SLOT(slot_import_textures()));
    menu->addSeparator();
    menu->addAction("Create BSDF...", m_bsdf_collection_item, SLOT(slot_create_bsdf()));
    menu->addAction("Create EDF...", m_edf_collection_item, SLOT(slot_create_edf()));
    menu->addAction("Create Surface Shader...", m_surface_shader_collection_item, SLOT(slot_create_surface_shader()));
    menu->addAction("Create Material...", m_material_collection_item, SLOT(slot_create_material()));
    return menu;
}

void AssemblyItem::add_item(ColorEntity& color)
{
    m_color_collection_item->add_item(color);
}

void AssemblyItem::add_item(Texture& texture)
{
    m_texture_collection_item->add_item(texture);
}

void AssemblyItem::add_item(TextureInstance& texture_instance)
{
    m_texture_instance_collection_item->add_item(texture_instance);
}

void AssemblyItem::add_item(BSDF& bsdf)
{
    m_bsdf_collection_item->add_item(bsdf);
}

void AssemblyItem::add_item(EDF& edf)
{
    m_edf_collection_item->add_item(edf);
}

void AssemblyItem::add_item(SurfaceShader& surface_shader)
{
    m_surface_shader_collection_item->add_item(surface_shader);
}

void AssemblyItem::add_item(Material& material)
{
    m_material_collection_item->add_item(material);
}

void AssemblyItem::add_item(Light& light)
{
    m_light_collection_item->add_item(light);
}

void AssemblyItem::add_item(Object& object)
{
    m_object_collection_item->add_item(object);
}

void AssemblyItem::add_item(ObjectInstance& object_instance)
{
    m_object_instance_collection_item->add_item(object_instance);
}

template <typename EntityContainer>
typename ItemTypeMap<EntityContainer>::T* AssemblyItem::add_collection_item(EntityContainer& entities)
{
    typedef ItemTypeMap<EntityContainer>::T ItemType;

    ItemType* item = new ItemType(m_assembly, entities, m_project_builder);
    addChild(item);

    return item;
}

void AssemblyItem::slot_instantiate()
{
    const string instance_name_suggestion =
        get_name_suggestion(
            string(m_assembly.get_name()) + "_inst",
            m_scene.assembly_instances());

    const string instance_name =
        get_entity_name_dialog(
            treeWidget(),
            "Instantiate Assembly",
            "Assembly Instance Name:",
            instance_name_suggestion);

    if (!instance_name.empty())
        m_project_builder.insert_assembly_instance(instance_name, m_assembly);
}

}   // namespace studio
}   // namespace appleseed
