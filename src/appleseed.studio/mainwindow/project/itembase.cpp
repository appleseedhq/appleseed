
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

// Interface header.
#include "itembase.h"

// Qt headers.
#include <QKeySequence>
#include <QMenu>
#include <QString>
#include <Qt>

using namespace appleseed::qtcommon;
using namespace foundation;

namespace appleseed {
namespace studio {

ItemBase::ItemBase(
    EntityEditorContext&    editor_context,
    const UniqueID          class_uid)
  : m_editor_context(editor_context)
  , m_class_uid(class_uid)
  , m_allow_edition(true)
  , m_allow_deletion(true)
{
    setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemIsUserCheckable);
}

ItemBase::ItemBase(
    EntityEditorContext&    editor_context,
    const UniqueID          class_uid,
    const QString&          title)
  : m_editor_context(editor_context)
  , m_class_uid(class_uid)
  , m_allow_edition(true)
  , m_allow_deletion(true)
{
    setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemIsUserCheckable);
    set_title(title);
}

UniqueID ItemBase::get_class_uid() const
{
    return m_class_uid;
}

void ItemBase::set_allow_edition(const bool allow)
{
    m_allow_edition = allow;
}

bool ItemBase::allows_edition() const
{
    return m_allow_edition;
}

void ItemBase::set_allow_deletion(const bool allow)
{
    m_allow_deletion = allow;
}

bool ItemBase::allows_deletion() const
{
    return m_allow_deletion;
}

void ItemBase::set_title(const QString& title)
{
    setText(0, title);
}

QMenu* ItemBase::get_single_item_context_menu() const
{
    QMenu* menu = new QMenu(treeWidget());

    if (m_allow_edition)
        menu->addAction("Edit...", this, SLOT(slot_edit()));

    if (m_allow_deletion)
    {
        menu->addAction("Delete", this, SLOT(slot_delete_multiple()), QKeySequence(Qt::Key_Delete))
            ->setData(QVariant::fromValue(make_qlist(this)));
    }

    return menu;
}

QMenu* ItemBase::get_multiple_items_context_menu(const QList<ItemBase*>& items) const
{
    QMenu* menu = new QMenu(treeWidget());

    bool any_item_allows_deletion = false;

    for (int i = 0; i < items.size(); ++i)
    {
        if (items[i]->allows_deletion())
        {
            any_item_allows_deletion = true;
            break;
        }
    }

    if (any_item_allows_deletion)
    {
        menu->addAction("Delete", this, SLOT(slot_delete_multiple()), QKeySequence(Qt::Key_Delete))
            ->setData(QVariant::fromValue(items));
    }

    return menu;
}

void ItemBase::delete_multiple(const QList<ItemBase*>& items)
{
}

void ItemBase::slot_edit(AttributeEditor* attribute_editor)
{
}

void ItemBase::slot_instantiate()
{
}

void ItemBase::slot_delete_multiple()
{
    delete_multiple(get_action_items<ItemBase>());
}

}   // namespace studio
}   // namespace appleseed
