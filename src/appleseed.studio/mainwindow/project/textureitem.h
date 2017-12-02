
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_TEXTUREITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_TEXTUREITEM_H

// appleseed.studio headers.
#include "mainwindow/project/entityactions.h"
#include "mainwindow/project/multimodelentityitem.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QList>

// Forward declarations.
namespace appleseed { namespace studio { class BaseGroupItem; } }
namespace appleseed { namespace studio { class EntityEditorContext; } }
namespace appleseed { namespace studio { class ItemBase; } }
namespace appleseed { namespace studio { class TextureCollectionItem; } }
namespace renderer  { class BaseGroup; }
namespace renderer  { class Texture; }

namespace appleseed {
namespace studio {

class TextureItem
  : public MultiModelEntityItem<renderer::Texture, renderer::BaseGroup, TextureCollectionItem>
{
  public:
    TextureItem(
        EntityEditorContext&    editor_context,
        renderer::Texture*      texture,
        renderer::BaseGroup&    parent,
        TextureCollectionItem*  parent_item,
        BaseGroupItem*          base_group_item);

  private:
    friend class EntityDeletionAction<TextureItem>;

    typedef MultiModelEntityItem<renderer::Texture, renderer::BaseGroup, TextureCollectionItem> Base;

    BaseGroupItem* m_base_group_item;

    void delete_multiple(const QList<ItemBase*>& items) override;
    void do_delete();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_TEXTUREITEM_H
