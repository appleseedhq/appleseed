
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_OBJECTINSTANCEITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_OBJECTINSTANCEITEM_H

// appleseed.studio headers.
#include "mainwindow/project/entityitembase.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QList>
#include <QObject>

// Forward declarations.
namespace appleseed { namespace studio { class ProjectBuilder; } }
namespace renderer  { class Assembly; }
namespace renderer  { class ObjectInstance; }
class QMenu;
class QString;
class QVariant;

namespace appleseed {
namespace studio {

class ObjectInstanceItem
  : public EntityItemBase<renderer::ObjectInstance>
{
    Q_OBJECT

  public:
    ObjectInstanceItem(
        renderer::ObjectInstance*   object_instance,
        renderer::Assembly&         assembly,
        ProjectBuilder&             project_builder);

    virtual QMenu* get_single_item_context_menu() const override;

    virtual QMenu* get_multiple_items_context_menu(const QList<ItemBase*>& items) const override;

    const renderer::Assembly& get_assembly() const;

  private slots:
    void slot_assign_material();
    void slot_assign_material_accepted(QString page_name, QString entity_name, QVariant untyped_data);
    void slot_unassign_material();

  private:
    renderer::ObjectInstance*       m_object_instance;
    renderer::Assembly&             m_assembly;
    ProjectBuilder&                 m_project_builder;

    virtual void slot_delete() override;

    void assign_material(
        const bool                  font_side,
        const bool                  back_side,
        const char*                 material_name);

    void unassign_material(
        const bool                  font_side,
        const bool                  back_side);

    void update_style();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_OBJECTINSTANCEITEM_H
