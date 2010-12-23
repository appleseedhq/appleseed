
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_MULTIMODELENTITYITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_MULTIMODELENTITYITEM_H

// appleseed.studio headers.
#include "mainwindow/project/assemblyentitybrowser.h"
#include "mainwindow/project/entityitem.h"
#include "mainwindow/project/entitynames.h"
#include "mainwindow/project/multimodelentityeditorformfactory.h"

// appleseed.renderer headers.
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <string>

// Forward declarations.
namespace appleseed { namespace studio { class ProjectBuilder; } }
namespace renderer  { class Assembly; }

namespace appleseed {
namespace studio {

template <typename Entity, typename FactoryRegistrar>
class MultiModelEntityItem
  : public EntityItem<Entity>
{
  public:
    MultiModelEntityItem(
        renderer::Assembly& assembly,
        FactoryRegistrar&   registrar,
        Entity&             entity,
        ProjectBuilder&     project_builder);

  protected:
    virtual void slot_edit();

  private:
    FactoryRegistrar& m_registrar;
};


//
// Implementation.
//

template <typename Entity, typename FactoryRegistrar>
MultiModelEntityItem<Entity, FactoryRegistrar>::MultiModelEntityItem(
    renderer::Assembly&     assembly,
    FactoryRegistrar&       registrar,
    Entity&                 entity,
    ProjectBuilder&         project_builder)
  : EntityItem(assembly, entity, project_builder)
  , m_registrar(registrar)
{
}

template <typename Entity, typename FactoryRegistrar>
void MultiModelEntityItem<Entity, FactoryRegistrar>::slot_edit()
{
    const std::string window_title =
        std::string("Edit ") + get_entity_name<Entity>();

    auto_ptr<EntityEditorWindow::IFormFactory> form_factory(
        new MultiModelEntityEditorFormFactory<FactoryRegistrar>(
            m_registrar,
            m_entity.get_name()));

    auto_ptr<EntityEditorWindow::IEntityBrowser> entity_browser(
        new AssemblyEntityBrowser(m_assembly));

    foundation::Dictionary values = m_entity.get_parameters();
    values.insert("model", m_entity.get_model());

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

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_MULTIMODELENTITYITEM_H
