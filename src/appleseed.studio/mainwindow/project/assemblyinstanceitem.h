
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYINSTANCEITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYINSTANCEITEM_H

// appleseed.studio headers.
#include "mainwindow/project/entityactions.h"
#include "mainwindow/project/entityitembase.h"
#include "mainwindow/project/instancecollectionitem.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QList>

// Forward declarations.
namespace appleseed { namespace studio { class EntityEditorContext; } }
namespace appleseed { namespace studio { class ItemBase; } }
namespace renderer  { class AssemblyInstance; }
namespace renderer  { class BaseGroup; }

namespace appleseed {
namespace studio {

class AssemblyInstanceItem
  : public EntityItemBase<renderer::AssemblyInstance>
{
  public:
    typedef InstanceCollectionItem<renderer::AssemblyInstance, AssemblyInstanceItem, renderer::BaseGroup> AssemblyInstanceCollectionItem;

    AssemblyInstanceItem(
        EntityEditorContext&            editor_context,
        renderer::AssemblyInstance*     assembly_instance,
        renderer::BaseGroup&            parent,
        AssemblyInstanceCollectionItem* collection_item);

  private:
    friend class EntityDeletionAction<AssemblyInstanceItem>;

    renderer::BaseGroup&                m_parent;
    AssemblyInstanceCollectionItem*     m_collection_item;

    virtual void delete_multiple(const QList<ItemBase*>& items) APPLESEED_OVERRIDE;
    void do_delete();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYINSTANCEITEM_H
