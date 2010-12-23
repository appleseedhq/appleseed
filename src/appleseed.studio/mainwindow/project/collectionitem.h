
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_COLLECTIONITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_COLLECTIONITEM_H

// appleseed.studio headers.
#include "mainwindow/project/collectionitembase.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QObject>

// Forward declarations.
namespace renderer  { class Entity; }
class QString;

namespace appleseed {
namespace studio {

class CollectionItem
  : public CollectionItemBase
{
    Q_OBJECT

  public:
    CollectionItem(
        const foundation::UniqueID  class_uid,
        const QString&              title);

    template <typename EntityContainer>
    CollectionItem(
        const foundation::UniqueID  class_uid,
        const QString&              title,
        EntityContainer&            entities);

    void add_item(renderer::Entity& entity);
};

template <typename EntityContainer>
CollectionItem::CollectionItem(
    const foundation::UniqueID      class_uid,
    const QString&                  title,
    EntityContainer&                entities)
  : CollectionItemBase(class_uid, title)
{
    for (foundation::each<EntityContainer> i = entities; i; ++i)
        add_item(*i);
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_COLLECTIONITEM_H
