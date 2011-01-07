
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

// Interface header.
#include "environmentedfcollectionitem.h"

// appleseed.studio headers.
#include "mainwindow/project/entitybrowser.h"
#include "mainwindow/project/entityeditorwindow.h"
#include "mainwindow/project/multimodelentityeditorformfactory.h"
#include "mainwindow/project/multimodelentityitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/tools.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Qt headers.
#include <QMenu>
#include <QWidget>

// Standard headers.
#include <memory>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

namespace
{
    const UniqueID g_class_uid = new_guid();
}

EnvironmentEDFCollectionItem::EnvironmentEDFCollectionItem(
    Scene&                      scene,
    EnvironmentEDFContainer&    environment_edfs,
    ProjectBuilder&             project_builder)
  : ItemBase(g_class_uid, "Environment EDFs")
  , m_scene(scene)
  , m_project_builder(project_builder)
{
    add_items(environment_edfs);
}

QMenu* EnvironmentEDFCollectionItem::get_single_item_context_menu() const
{
    QMenu* menu = new QMenu(treeWidget());
    menu->addAction("Create Environment EDF...", this, SLOT(slot_create_environment_edf()));
    return menu;
}

void EnvironmentEDFCollectionItem::add_item(EnvironmentEDF& environment_edf)
{
    addChild(
        new MultiModelEntityItem<EnvironmentEDF, Scene>(
            m_scene,
            environment_edf,
            m_project_builder));
}

void EnvironmentEDFCollectionItem::slot_create_environment_edf()
{
    auto_ptr<EntityEditorWindow::IFormFactory> form_factory(
        new MultiModelEntityEditorFormFactory<EnvironmentEDFFactoryRegistrar>(
            m_registrar,
            get_name_suggestion("environment_edf", m_scene.environment_edfs())));

    auto_ptr<EntityEditorWindow::IEntityBrowser> entity_browser(
        new EntityBrowser<Scene>(m_scene));

    open_entity_editor(
        treeWidget(),
        "Create Environment EDF",
        form_factory,
        entity_browser,
        this,
        SLOT(slot_create_environment_edf_accepted(foundation::Dictionary)));
}

void EnvironmentEDFCollectionItem::slot_create_environment_edf_accepted(Dictionary values)
{
    catch_entity_creation_errors(&EnvironmentEDFCollectionItem::create_environment_edf, values, "Environment EDF");
}

void EnvironmentEDFCollectionItem::create_environment_edf(const Dictionary& values)
{
    m_project_builder.insert_environment_edf(values);

    qobject_cast<QWidget*>(sender())->close();
}

}   // namespace studio
}   // namespace appleseed
