
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
#include "mainwindow/project/surfaceshadercollectionitem.h"
#include "mainwindow/project/texturecollectionitem.h"
#include "mainwindow/project/textureinstancecollectionitem.h"

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"
#include "renderer/api/color.h"
#include "renderer/api/edf.h"
#include "renderer/api/geometry.h"
#include "renderer/api/light.h"
#include "renderer/api/material.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"
#include "renderer/api/texture.h"

using namespace renderer;

namespace appleseed {
namespace studio {

AssemblyItem::AssemblyItem(
    ProjectBuilder& project_builder,
    Assembly&       assembly)
  : EntityItem(assembly)
  , m_project_builder(project_builder)
  , m_assembly(assembly)
{
    m_color_collection_item = add_item(assembly.colors());
    m_texture_collection_item = add_item(assembly.textures());
    m_texture_instance_collection_item = add_item(assembly.texture_instances());
    m_bsdf_collection_item = add_item(assembly.bsdfs());
    m_edf_collection_item = add_item(assembly.edfs());
    m_surface_shader_collection_item = add_item(assembly.surface_shaders());
    m_material_collection_item = add_item(assembly.materials());
    m_light_collection_item = add_item(assembly.lights());
    m_object_collection_item = add_item(assembly.objects());
    m_object_instance_collection_item = add_item(assembly.object_instances());
}

QMenu* AssemblyItem::get_context_menu() const
{
    return 0;
}

void AssemblyItem::add_item(const ColorEntity& color)
{
    m_color_collection_item->addChild(new EntityItem(color));
}

void AssemblyItem::add_item(const Texture& texture)
{
    m_texture_collection_item->addChild(new EntityItem(texture));
}

void AssemblyItem::add_item(const TextureInstance& texture_instance)
{
    m_texture_instance_collection_item->addChild(new EntityItem(texture_instance));
}

void AssemblyItem::add_item(const BSDF& bsdf)
{
    m_bsdf_collection_item->addChild(new EntityItem(bsdf));
}

void AssemblyItem::add_item(const EDF& edf)
{
    m_edf_collection_item->addChild(new EntityItem(edf));
}

void AssemblyItem::add_item(const SurfaceShader& surface_shader)
{
    m_surface_shader_collection_item->addChild(new EntityItem(surface_shader));
}

void AssemblyItem::add_item(const Material& material)
{
    m_material_collection_item->addChild(new EntityItem(material));
}

void AssemblyItem::add_item(const Light& light)
{
    m_light_collection_item->addChild(new EntityItem(light));
}

void AssemblyItem::add_item(const Object& object)
{
    m_object_collection_item->addChild(new EntityItem(object));
}

void AssemblyItem::add_item(const ObjectInstance& object_instance)
{
    m_object_instance_collection_item->addChild(new EntityItem(object_instance));
}

template <typename EntityContainer>
typename ItemTypeMap<EntityContainer>::T* AssemblyItem::add_item(EntityContainer& entities)
{
    typedef ItemTypeMap<EntityContainer>::T ItemType;
    ItemType* item = new ItemType(m_project_builder, entities, &m_assembly);
    addChild(item);
    return item;
}

}   // namespace studio
}   // namespace appleseed
