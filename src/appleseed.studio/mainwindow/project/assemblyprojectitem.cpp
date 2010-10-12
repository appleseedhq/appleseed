
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
#include "assemblyprojectitem.h"

// appleseed.studio headers.
#include "mainwindow/project/colorcollectionprojectitem.h"
#include "mainwindow/project/texturecollectionprojectitem.h"
#include "mainwindow/project/textureinstancecollectionprojectitem.h"

// appleseed.renderer headers.
#include "renderer/api/color.h"
#include "renderer/api/scene.h"
#include "renderer/api/texture.h"

using namespace renderer;

namespace appleseed {
namespace studio {

AssemblyProjectItem::AssemblyProjectItem(
    ProjectBuilder& project_builder,
    Assembly&       assembly)
  : EntityProjectItem(assembly)
  , m_project_builder(project_builder)
  , m_assembly(assembly)
{
    m_color_collection_item = add_item(assembly.colors());
    m_texture_collection_project_item = add_item(assembly.textures());
    m_texture_instance_collection_project_item = add_item(assembly.texture_instances());
}

QMenu* AssemblyProjectItem::get_context_menu() const
{
    return 0;
}

void AssemblyProjectItem::add_item(const ColorEntity& color)
{
    m_color_collection_item->addChild(new EntityProjectItem(color));
}

void AssemblyProjectItem::add_item(const Texture& texture)
{
    m_texture_collection_project_item->addChild(new EntityProjectItem(texture));
}

void AssemblyProjectItem::add_item(const TextureInstance& texture_instance)
{
    m_texture_instance_collection_project_item->addChild(
        new EntityProjectItem(texture_instance));
}

void AssemblyProjectItem::add_item(const BSDF& bsdf)
{
}

void AssemblyProjectItem::add_item(const EDF& edf)
{
}

void AssemblyProjectItem::add_item(const SurfaceShader& surface_shader)
{
}

void AssemblyProjectItem::add_item(const Material& material)
{
}

void AssemblyProjectItem::add_item(const Light& light)
{
}

void AssemblyProjectItem::add_item(const Object& object)
{
}

void AssemblyProjectItem::add_item(const ObjectInstance& object_instance)
{
}

template <typename EntityContainer>
typename ProjectItemTypeMap<EntityContainer>::T* AssemblyProjectItem::add_item(EntityContainer& entities)
{
    typedef ProjectItemTypeMap<EntityContainer>::T ItemType;
    ItemType* item = new ItemType(m_project_builder, entities, &m_assembly);
    addChild(item);
    return item;
}

}   // namespace studio
}   // namespace appleseed
