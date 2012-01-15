
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ITEMBASE_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ITEMBASE_H

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Qt headers.
#include <QList>
#include <QMetaType>
#include <QObject>
#include <QTreeWidgetItem>

// Forward declarations.
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
    explicit ItemBase(const foundation::UniqueID class_uid);
    ItemBase(const foundation::UniqueID class_uid, const QString& title);

    virtual ~ItemBase() {}

    foundation::UniqueID get_class_uid() const;

    void set_allow_edition(const bool allow);
    bool allows_edition() const;

    void set_allow_deletion(const bool allow);
    bool allows_deletion() const;

    void set_title(const QString& title);
    void set_render_layer(const QString& name);

    virtual QMenu* get_single_item_context_menu() const;
    virtual QMenu* get_multiple_items_context_menu(const QList<ItemBase*>& items) const;

  public slots:
    virtual void slot_edit();
    virtual void slot_delete();
    virtual void slot_delete_multiple(QList<ItemBase*> items = QList<ItemBase*>());

  protected:
    ItemBase();

  private:
    foundation::UniqueID    m_class_uid;
    bool                    m_allow_edition;
    bool                    m_allow_deletion;
};

}       // namespace studio
}       // namespace appleseed

Q_DECLARE_METATYPE(QList<appleseed::studio::ItemBase*>);

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ITEMBASE_H
