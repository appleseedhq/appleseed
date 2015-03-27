
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_BASEGROUPITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_BASEGROUPITEM_H

// appleseed.studio headers.
#include "mainwindow/project/itembase.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Forward declarations.
namespace appleseed { namespace studio { class AssemblyCollectionItem; } }
namespace appleseed { namespace studio { class AssemblyInstanceItem; } }
namespace appleseed { namespace studio { class EntityEditorContext; } }
namespace appleseed { namespace studio { template <typename Entity, typename EntityItem, typename ParentEntity> class InstanceCollectionItem; } }
namespace appleseed { namespace studio { template <typename Entity, typename ParentEntity, typename ParentItem> class SingleModelCollectionItem; } }
namespace appleseed { namespace studio { class TextureCollectionItem; } }
namespace appleseed { namespace studio { class TextureInstanceItem; } }
namespace renderer  { class Assembly; }
namespace renderer  { class AssemblyInstance; }
namespace renderer  { class BaseGroup; }
namespace renderer  { class ColorEntity; }
namespace renderer  { class Texture; }
namespace renderer  { class TextureInstance; }
class QMenu;
class QString;

namespace appleseed {
namespace studio {

class BaseGroupItem
  : public ItemBase
{
  public:
    BaseGroupItem(
        EntityEditorContext&            editor_context,
        const foundation::UniqueID      class_uid,
        renderer::BaseGroup&            base_group);

    BaseGroupItem(
        EntityEditorContext&            editor_context,
        const foundation::UniqueID      class_uid,
        const QString&                  title,
        renderer::BaseGroup&            base_group);

    ItemBase* add_item(renderer::ColorEntity* color);
    ItemBase* add_item(renderer::Texture* texture);
    ItemBase* add_item(renderer::TextureInstance* texture_instance);
    ItemBase* add_item(renderer::Assembly* assembly);
    ItemBase* add_item(renderer::AssemblyInstance* assembly_instance);

    typedef SingleModelCollectionItem<renderer::ColorEntity, renderer::BaseGroup, BaseGroupItem> ColorCollectionItem;
    typedef InstanceCollectionItem<renderer::AssemblyInstance, AssemblyInstanceItem, renderer::BaseGroup> AssemblyInstanceCollectionItem;
    typedef InstanceCollectionItem<renderer::TextureInstance, TextureInstanceItem, renderer::BaseGroup> TextureInstanceCollectionItem;

    ColorCollectionItem& get_color_collection_item() const;
    TextureCollectionItem& get_texture_collection_item() const;
    TextureInstanceCollectionItem& get_texture_instance_collection_item() const;
    AssemblyCollectionItem& get_assembly_collection_item() const;
    AssemblyInstanceCollectionItem& get_assembly_instance_collection_item() const;

  private:
    ColorCollectionItem*                m_color_collection_item;
    TextureCollectionItem*              m_texture_collection_item;
    TextureInstanceCollectionItem*      m_texture_instance_collection_item;
    AssemblyCollectionItem*             m_assembly_collection_item;
    AssemblyInstanceCollectionItem*     m_assembly_instance_collection_item;

    void add_items(renderer::BaseGroup& base_group);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_BASEGROUPITEM_H
