
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

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Qt headers.
#include <QAction>
#include <QList>
#include <QMetaType>
#include <QObject>
#include <QTreeWidgetItem>
#include <QVariant>

// Forward declarations.
namespace appleseed { namespace studio { class AttributeEditor; } }
namespace appleseed { namespace studio { class EntityEditorContext; } }
class QMenu;
class QString;

namespace appleseed {
namespace studio {

class ItemBase
  : public QObject
  , public QTreeWidgetItem
{
    Q_OBJECT

  public:
    ItemBase(
        EntityEditorContext&        editor_context,
        const foundation::UniqueID  class_uid);
    ItemBase(
        EntityEditorContext&        editor_context,
        const foundation::UniqueID  class_uid,
        const QString&              title);

    ~ItemBase() override {}

    foundation::UniqueID get_class_uid() const;

    void set_allow_edition(const bool allow);
    bool allows_edition() const;

    void set_allow_deletion(const bool allow);
    bool allows_deletion() const;

    void set_title(const QString& title);

    virtual QMenu* get_single_item_context_menu() const;
    virtual QMenu* get_multiple_items_context_menu(const QList<ItemBase*>& items) const;

    virtual void delete_multiple(const QList<ItemBase*>& items);

  public slots:
    virtual void slot_edit(AttributeEditor* attribute_editor = nullptr);
    virtual void slot_instantiate();
    void slot_delete_multiple();

  protected:
    EntityEditorContext&        m_editor_context;

    template <typename Item>
    QList<Item*> get_action_items();

  private:
    const foundation::UniqueID  m_class_uid;
    bool                        m_allow_edition;
    bool                        m_allow_deletion;
};


//
// ItemBase class implementation.
//

template <typename Item>
QList<Item*> ItemBase::get_action_items()
{
    QAction* action = qobject_cast<QAction*>(sender());

    if (action && !action->data().isNull())
    {
        const QList<Item*> items =
            qtcommon::qlist_static_cast<Item*>(
                action->data().value<QList<ItemBase*>>());

        if (!items.empty())
            return items;
    }

    return qtcommon::make_qlist(static_cast<Item*>(this));
}

}   // namespace studio
}   // namespace appleseed

Q_DECLARE_METATYPE(QList<appleseed::studio::ItemBase*>);
Q_DECLARE_METATYPE(QList<const appleseed::studio::ItemBase*>);
