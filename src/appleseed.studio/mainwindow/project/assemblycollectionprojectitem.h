
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYCOLLECTIONPROJECTITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYCOLLECTIONPROJECTITEM_H

// appleseed.studio headers.
#include "mainwindow/project/collectionprojectitembase.h"

// appleseed.renderer headers.
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Qt headers.
#include <QObject>

// Standard headers.
#include <map>

// Forward declarations.
namespace appleseed { namespace studio { class AssemblyProjectItem; }}
namespace appleseed { namespace studio { class ProjectBuilder; }}
namespace renderer  { class Assembly; }

namespace appleseed {
namespace studio {

class AssemblyCollectionProjectItem
  : public CollectionProjectItemBase
{
    Q_OBJECT

  public:
    AssemblyCollectionProjectItem(
        ProjectBuilder&                 project_builder,
        renderer::AssemblyContainer&    assemblies);

    void add_item(renderer::Assembly& assembly);

    AssemblyProjectItem& get_item(const renderer::Assembly& assembly) const;

  private:
    typedef std::map<foundation::UniqueID, AssemblyProjectItem*> AssemblyProjectItemMap;

    ProjectBuilder&         m_project_builder;
    AssemblyProjectItemMap  m_assembly_project_items;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYCOLLECTIONPROJECTITEM_H
