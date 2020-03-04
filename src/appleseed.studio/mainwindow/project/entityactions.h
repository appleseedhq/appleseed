
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
#include "mainwindow/rendering/renderingmanager.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QList>

// Standard headers.
#include <string>

// Forward declarations.
namespace renderer { class Project; }

namespace appleseed {
namespace studio {

//
// These actions allow to schedule the creation and edition of scene entities
// right before rendering starts. They are used to edit the scene during rendering.
//

template <typename EntityItem>
class EntityCreationAction
  : public RenderingManager::IScheduledAction
{
  public:
    EntityCreationAction(
        EntityItem*                     item,
        const foundation::Dictionary&   values)
      : m_item(item)
      , m_values(values)
    {
    }

    void operator()(
        renderer::Project&              project) override
    {
        m_item->create(m_values);
    }

  private:
    EntityItem*                         m_item;
    const foundation::Dictionary        m_values;
};

template <typename EntityItem>
class EntityEditionAction
  : public RenderingManager::IScheduledAction
{
  public:
    EntityEditionAction(
        EntityItem*                     item,
        const foundation::Dictionary&   values)
      : m_item(item)
      , m_values(values)
    {
    }

    void operator()(
        renderer::Project&              project) override
    {
        m_item->edit(m_values);
    }

  private:
    EntityItem*                         m_item;
    const foundation::Dictionary        m_values;
};

template <typename EntityItem>
class EntityInstantiationAction
  : public RenderingManager::IScheduledAction
{
  public:
    explicit EntityInstantiationAction(
        EntityItem*                     item,
        const std::string               name)
      : m_item(item)
      , m_name(name)
    {
    }

    void operator()(
        renderer::Project&              project) override
    {
        m_item->do_instantiate(m_name);
    }

  private:
    EntityItem*                         m_item;
    const std::string                   m_name;
};

template <typename EntityItem>
class EntityDeletionAction
  : public RenderingManager::IScheduledAction
{
  public:
    explicit EntityDeletionAction(
        EntityItem*                     item)
    {
        m_items.append(item);
    }

    explicit EntityDeletionAction(
        const QList<EntityItem*>&       items)
      : m_items(items)
    {
    }

    void operator()(
        renderer::Project&              project) override
    {
        for (int i = 0; i < m_items.size(); ++i)
            m_items[i]->do_delete();
    }

  private:
    QList<EntityItem*>                  m_items;
};

}   // namespace studio
}   // namespace appleseed
