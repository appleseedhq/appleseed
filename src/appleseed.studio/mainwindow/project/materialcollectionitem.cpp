
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
#include "materialcollectionitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblyentitybrowser.h"
#include "mainwindow/project/entityeditorwindow.h"
#include "mainwindow/project/entityitem.h"
#include "mainwindow/project/materialeditorformfactory.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/tools.h"

// appleseed.renderer headers.
#include "renderer/api/material.h"

// Qt headers.
#include <QMenu>

// Standard headers.
#include <memory>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

MaterialCollectionItem::MaterialCollectionItem(
    Assembly&           assembly,
    MaterialContainer&  materials,
    ProjectBuilder&     project_builder)
  : CollectionItem("Materials", materials)
  , m_assembly(assembly)
  , m_project_builder(project_builder)
{
}

QMenu* MaterialCollectionItem::get_context_menu() const
{
    QMenu* menu = new QMenu(treeWidget());
    menu->addAction("Create Material...", this, SLOT(slot_create_material()));
    return menu;
}

void MaterialCollectionItem::slot_create_material()
{
    auto_ptr<EntityEditorWindow::IFormFactory> form_factory(
        new MaterialEditorFormFactory(m_assembly));

    auto_ptr<EntityEditorWindow::IEntityBrowser> entity_browser(
        new AssemblyEntityBrowser(m_assembly));

    open_entity_editor(
        treeWidget(),
        "Create Material",
        form_factory,
        entity_browser,
        this,
        SLOT(slot_create_material_accept(foundation::Dictionary)));
}

void MaterialCollectionItem::slot_create_material_accept(Dictionary values)
{
    catch_entity_creation_errors(&MaterialCollectionItem::create_material, values, "Material");
}

void MaterialCollectionItem::create_material(const Dictionary& values)
{
    m_project_builder.insert_material(m_assembly, values);

    //emit project_modified();

    qobject_cast<QWidget*>(sender())->close();
}

}   // namespace studio
}   // namespace appleseed
