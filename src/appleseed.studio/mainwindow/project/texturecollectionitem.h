
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_TEXTURECOLLECTIONITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_TEXTURECOLLECTIONITEM_H

// appleseed.studio headers.
#include "mainwindow/project/collectionitembase.h"

// appleseed.renderer headers.
#include "renderer/api/scene.h"
#include "renderer/api/texture.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QObject>

// Forward declarations.
namespace appleseed { namespace studio { class BaseGroupItem; } }
namespace appleseed { namespace studio { class EntityEditorContext; } }
namespace appleseed { namespace studio { class ItemBase; } }
namespace renderer  { class BaseGroup; }
class QMenu;

namespace appleseed {
namespace studio {

class TextureCollectionItem
  : public CollectionItemBase<renderer::Texture>
{
    Q_OBJECT

  public:
    TextureCollectionItem(
        EntityEditorContext&        editor_context,
        renderer::TextureContainer& textures,
        renderer::BaseGroup&        parent,
        BaseGroupItem*              parent_item);

    virtual QMenu* get_single_item_context_menu() const APPLESEED_OVERRIDE;

  public slots:
    void slot_import_textures();

  private:
    renderer::BaseGroup&            m_parent;
    BaseGroupItem*                  m_parent_item;

    virtual ItemBase* create_item(renderer::Texture* texture) APPLESEED_OVERRIDE;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_TEXTURECOLLECTIONITEM_H
