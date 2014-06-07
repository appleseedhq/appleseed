
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_MATERIALITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_MATERIALITEM_H

// appleseed.studio headers.
#include "mainwindow/project/itembase.h"
#include "mainwindow/project/entityitembase.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QList>
#include <QObject>

// Forward declarations.
namespace appleseed     { namespace studio { class AssemblyItem; } }
namespace appleseed     { namespace studio { class ProjectBuilder; } }
namespace renderer      { class Assembly; }
namespace renderer      { class Material; }
class QMenu;
class QString;
class QVariant;

namespace appleseed {
namespace studio {

class MaterialItem
  : public EntityItemBase<renderer::Material>
{
    Q_OBJECT

  public:

    MaterialItem(
        renderer::Material*       material,
        renderer::Assembly&       parent,
        AssemblyItem*             parent_item,
        ProjectBuilder&           project_builder);
    
  private:
    renderer::Assembly&     m_parent;
    AssemblyItem*           m_parent_item;
    ProjectBuilder&         m_project_builder;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_MATERIALITEM_H
