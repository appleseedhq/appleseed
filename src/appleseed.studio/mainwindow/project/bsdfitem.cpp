
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
#include "bsdfitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblyentitybrowser.h"
#include "mainwindow/project/entityeditorformfactory.h"
#include "mainwindow/project/entityeditorwindow.h"
#include "mainwindow/project/tools.h"

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <memory>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

BSDFItem::BSDFItem(
    Assembly&               assembly,
    BSDFFactoryRegistrar&   registrar,
    BSDF&                   bsdf)
  : EntityItem(bsdf)
  , m_assembly(assembly)
  , m_registrar(registrar)
  , m_bsdf(bsdf)
{
}

void BSDFItem::slot_edit()
{
    Dictionary values = m_bsdf.get_parameters();
    values.insert("model", m_bsdf.get_model());

    auto_ptr<EntityEditorWindow::IFormFactory> form_factory(
        new EntityEditorFormFactory<BSDFFactoryRegistrar>(
            m_registrar,
            m_bsdf.get_name()));

    auto_ptr<EntityEditorWindow::IEntityBrowser> entity_browser(
        new AssemblyEntityBrowser(m_assembly));

    open_entity_editor(
        treeWidget(),
        "Create BSDF",
        form_factory,
        entity_browser,
        values,
        this,
        SLOT(slot_edit_bsdf_accepted(foundation::Dictionary)));
}

void BSDFItem::slot_edit_bsdf_accepted(Dictionary values)
{
    catch_entity_creation_errors(&BSDFItem::edit_bsdf, values, "BSDF");
}

void BSDFItem::edit_bsdf(const Dictionary& values)
{
    ParamArray& params = m_bsdf.get_parameters();

    for (const_each<StringDictionary> i = values.strings(); i; ++i)
    {
        if (params.strings().exist(i->name()))
        {
            params.insert(i->name(), i->value());
        }
    }

    qobject_cast<QWidget*>(sender())->close();
}

void BSDFItem::slot_delete()
{
}

}   // namespace studio
}   // namespace appleseed
