
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_OBJECTITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_OBJECTITEM_H

// appleseed.studio headers.
#include "mainwindow/project/entityactions.h"
#include "mainwindow/project/entityitembase.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QList>

// Forward declarations.
namespace appleseed { namespace studio { class AssemblyItem; } }
namespace appleseed { namespace studio { class EntityEditorContext; } }
namespace appleseed { namespace studio { class ItemBase; } }
namespace renderer  { class Assembly; }
namespace renderer  { class Object; }
class QMenu;

namespace appleseed {
namespace studio {

class ObjectItem
  : public EntityItemBase<renderer::Object>
{
    Q_OBJECT

  public:
    ObjectItem(
        EntityEditorContext&    editor_context,
        renderer::Object*       object,
        renderer::Assembly&     parent,
        AssemblyItem*           parent_item);

    virtual QMenu* get_single_item_context_menu() const APPLESEED_OVERRIDE;

  private:
    friend class EntityInstantiationAction<ObjectItem>;
    friend class EntityDeletionAction<ObjectItem>;

    renderer::Assembly&         m_parent;
    AssemblyItem*               m_parent_item;

    virtual void slot_instantiate() APPLESEED_OVERRIDE;
    void do_instantiate(const std::string& name);

    virtual void delete_multiple(const QList<ItemBase*>& items) APPLESEED_OVERRIDE;
    void do_delete();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_OBJECTITEM_H
