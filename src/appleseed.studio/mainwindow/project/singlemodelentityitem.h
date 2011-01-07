
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_SINGLEMODELENTITYITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_SINGLEMODELENTITYITEM_H

// appleseed.studio headers.
#include "mainwindow/project/entitybrowser.h"
#include "mainwindow/project/entityitem.h"
#include "mainwindow/project/entitytraits.h"
#include "mainwindow/project/singlemodelentityeditorformfactory.h"
#include "mainwindow/project/tools.h"

// appleseed.renderer headers.
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <memory>
#include <string>

// Forward declarations.
namespace appleseed { namespace studio { class ProjectBuilder; } }

namespace appleseed {
namespace studio {

template <typename Entity, typename ParentEntity, typename Factory>
class SingleModelEntityItem
  : public EntityItem<Entity>
{
  public:
    SingleModelEntityItem(
        ParentEntity&       parent,
        Entity&             entity,
        ProjectBuilder&     project_builder);

  private:
    ParentEntity&           m_parent;

    virtual void slot_edit();
};


//
// SingleModelEntityItem class implementation.
//

template <typename Entity, typename ParentEntity, typename Factory>
SingleModelEntityItem<Entity, ParentEntity, Factory>::SingleModelEntityItem(
    ParentEntity&           parent,
    Entity&                 entity,
    ProjectBuilder&         project_builder)
  : EntityItem(entity, project_builder)
  , m_parent(parent)
{
}

template <typename Entity, typename ParentEntity, typename Factory>
void SingleModelEntityItem<Entity, ParentEntity, Factory>::slot_edit()
{
    const std::string window_title =
        std::string("Edit ") + EntityTraits<Entity>::get_entity_type_name();

    std::auto_ptr<EntityEditorWindow::IFormFactory> form_factory(
        new SingleModelEntityEditorFormFactory(
            m_entity.get_name(),
            Factory::get_widget_definitions()));

    std::auto_ptr<EntityEditorWindow::IEntityBrowser> entity_browser(
        new EntityBrowser<ParentEntity>(m_parent));

    foundation::Dictionary values = m_entity.get_parameters();

    open_entity_editor(
        treeWidget(),
        window_title,
        form_factory,
        entity_browser,
        values,
        this,
        SLOT(slot_edit_accepted(foundation::Dictionary)));
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_SINGLEMODELENTITYITEM_H
