
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "mainwindow/project/entityactions.h"
#include "mainwindow/project/instancecollectionitem.h"
#include "mainwindow/project/itembase.h"
#include "mainwindow/project/singlemodelentityitem.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QList>
#include <QObject>

// Forward declarations.
namespace appleseed     { namespace studio { class EntityEditorContext; } }
namespace appleseed     { namespace studio { class ObjectInstanceItem; } }
namespace foundation    { class Dictionary; }
namespace renderer      { class Assembly; }
namespace renderer      { class ObjectInstance; }
class QMenu;
class QString;
class QVariant;

namespace appleseed {
namespace studio {

typedef InstanceCollectionItem<
    renderer::ObjectInstance,
    ObjectInstanceItem,
    renderer::Assembly
> ObjectInstanceCollectionItem;

class ObjectInstanceItem
  : public SingleModelEntityItem<renderer::ObjectInstance, renderer::Assembly, ObjectInstanceCollectionItem>
{
    Q_OBJECT

  public:
    // The name of the default material slot, when the object does not have any slot.
    static const char* DefaultSlotName;

    ObjectInstanceItem(
        EntityEditorContext&            editor_context,
        renderer::ObjectInstance*       object_instance,
        renderer::Assembly&             parent,
        ObjectInstanceCollectionItem*   collection_item);

    const renderer::Assembly& get_assembly() const;

    QMenu* get_single_item_context_menu() const override;
    QMenu* get_multiple_items_context_menu(const QList<ItemBase*>& items) const override;

    void assign_material(
        const QString&                  page_name,
        const QString&                  entity_name,
        const QVariant&                 untyped_data);

    void clear_material(const QVariant& untyped_data);

    void update_style();

  private slots:
    void slot_open_material_assignment_editor();
    void slot_assign_material();
    void slot_assign_material_accepted(QString page_name, QString entity_name, QVariant data);
    void slot_clear_material();

  private:
    friend class EntityDeletionAction<ObjectInstanceItem>;

    typedef SingleModelEntityItem<renderer::ObjectInstance, renderer::Assembly, ObjectInstanceCollectionItem> Base;

    void delete_multiple(const QList<ItemBase*>& items) override;
    void do_delete();

    void add_material_assignment_menu_actions(
        QMenu*                          menu,
        const QList<ItemBase*>&         items = QList<ItemBase*>()) const;

    void add_material_assignment_menu_actions(
        QMenu*                          menu,
        const char*                     slot,
        const QList<ItemBase*>&         items) const;

    void do_assign_material(
        const char*                     slot_name,
        const int                       sides,
        const char*                     material_name);

    void do_unassign_material(
        const char*                     slot_name,
        const int                       sides);
};

}   // namespace studio
}   // namespace appleseed
