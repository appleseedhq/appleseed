
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "mainwindow/project/assemblyitem.h"
#include "mainwindow/project/collectionitem.h"

// appleseed.renderer headers.
#include "renderer/api/material.h"

// Forward declarations.
namespace appleseed { namespace studio { class EntityEditorContext; } }
namespace renderer  { class Assembly; }

namespace appleseed {
namespace studio {

class MaterialCollectionItem
  : public CollectionItem<renderer::Material, renderer::Assembly, AssemblyItem>
{
    Q_OBJECT

  public:
    MaterialCollectionItem(
        EntityEditorContext&            editor_context,
        renderer::MaterialContainer&    materials,
        renderer::Assembly&             parent,
        AssemblyItem*                   parent_item);

  protected:
    typedef CollectionItem<renderer::Material, renderer::Assembly, AssemblyItem> Base;

    QMenu* get_single_item_context_menu() const override;

  private slots:
    void slot_create_generic();
    void slot_create_osl();

  private:
    ItemBase* create_item(renderer::Material* material) override;

    void do_create_material(const char* model);

    renderer::Assembly& m_parent;
    AssemblyItem*       m_parent_item;
};

}   // namespace studio
}   // namespace appleseed
