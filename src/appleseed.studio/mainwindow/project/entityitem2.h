
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYITEM2_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYITEM2_H

// appleseed.studio headers.
#include "mainwindow/project/entitycreatorbase.h"
#include "mainwindow/project/entityitem.h"

// appleseed.renderer headers.
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"

// Qt headers.
#include <QObject>

// Forward declarations.
namespace renderer      { class Assembly; }

namespace appleseed {
namespace studio {

template <typename Entity, typename FactoryRegistrar>
class EntityItem2
  : public EntityItem
  , private EntityCreatorBase
{
  public:
    explicit EntityItem2(
        renderer::Assembly& assembly,
        FactoryRegistrar&   registrar,
        Entity&             entity);

  protected:
    virtual void slot_edit();
    virtual void slot_edit_accepted(foundation::Dictionary values);

    virtual void slot_delete();

  private:
    renderer::Assembly&     m_assembly;
    FactoryRegistrar&       m_registrar;
    Entity&                 m_entity;

    void edit(const foundation::Dictionary& values);
};


//
// Implementation.
//

template <typename Entity, typename FactoryRegistrar>
EntityItem2<Entity, FactoryRegistrar>::EntityItem2(
    renderer::Assembly&     assembly,
    FactoryRegistrar&       registrar,
    Entity&                 entity)
  : EntityItem(entity)
  , m_assembly(assembly)
  , m_registrar(registrar)
  , m_entity(entity)
{
}

template <typename Entity, typename FactoryRegistrar>
void EntityItem2<Entity, FactoryRegistrar>::slot_edit()
{
    foundation::Dictionary values = m_entity.get_parameters();
    values.insert("model", m_entity.get_model());

    auto_ptr<EntityEditorWindow::IFormFactory> form_factory(
        new EntityEditorFormFactory<FactoryRegistrar>(
            m_registrar,
            m_entity.get_name()));

    auto_ptr<EntityEditorWindow::IEntityBrowser> entity_browser(
        new AssemblyEntityBrowser(m_assembly));

    open_entity_editor(
        treeWidget(),
        "Create BSDF",
        form_factory,
        entity_browser,
        values,
        this,
        SLOT(slot_edit_accepted(foundation::Dictionary)));
}

template <typename Entity, typename FactoryRegistrar>
void EntityItem2<Entity, FactoryRegistrar>::slot_edit_accepted(foundation::Dictionary values)
{
    catch_entity_creation_errors(&EntityItem2::edit, values, "BSDF");
}

template <typename Entity, typename FactoryRegistrar>
void EntityItem2<Entity, FactoryRegistrar>::edit(const foundation::Dictionary& values)
{
    renderer::ParamArray& params = m_entity.get_parameters();

    for (foundation::const_each<foundation::StringDictionary> i = values.strings(); i; ++i)
    {
        if (params.strings().exist(i->name()))
            params.insert(i->name(), i->value());
    }

    qobject_cast<QWidget*>(sender())->close();
}

template <typename Entity, typename FactoryRegistrar>
void EntityItem2<Entity, FactoryRegistrar>::slot_delete()
{
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYITEM2_H
