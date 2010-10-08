
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTTREE_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTTREE_H

// appleseed.studio headers.
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/projectitemtypemap.h"

// Forward declarations.
namespace appleseed { namespace studio { class AssemblyCollectionProjectItem; }}
namespace appleseed { namespace studio { class AssemblyInstanceCollectionProjectItem; }}
namespace appleseed { namespace studio { class ColorCollectionProjectItem; }}
namespace appleseed { namespace studio { class EnvironmentEDFCollectionProjectItem; }}
namespace appleseed { namespace studio { class EnvironmentShaderCollectionProjectItem; }}
namespace appleseed { namespace studio { class TextureCollectionProjectItem; }}
namespace appleseed { namespace studio { class TextureInstanceCollectionProjectItem; }}
namespace renderer  { class Project; }
class QTreeWidget;

namespace appleseed {
namespace studio {

class ProjectTree
{
  public:
    ProjectTree(
        renderer::Project&  project,
        QTreeWidget*        tree_widget);

    ColorCollectionProjectItem& get_color_collection_item() const;
    TextureCollectionProjectItem& get_texture_collection_item() const;
    TextureInstanceCollectionProjectItem& get_texture_instance_collection_item() const;
    AssemblyCollectionProjectItem& get_assembly_collection_item() const;
    AssemblyInstanceCollectionProjectItem& get_assembly_instance_collection_item() const;

  private:
    renderer::Project&                      m_project;
    QTreeWidget*                            m_tree_widget;
    ProjectBuilder                          m_project_builder;

    ColorCollectionProjectItem*             m_color_collection_item;
    TextureCollectionProjectItem*           m_texture_collection_item;
    TextureInstanceCollectionProjectItem*   m_texture_instance_collection_item;
    EnvironmentEDFCollectionProjectItem*    m_environment_edf_collection_item;
    EnvironmentShaderCollectionProjectItem* m_environment_shader_collection_item;
    AssemblyCollectionProjectItem*          m_assembly_collection_item;
    AssemblyInstanceCollectionProjectItem*  m_assembly_instance_collection_item;

    template <typename EntityContainer>
    typename ProjectItemTypeMap<EntityContainer>::T* add_item(EntityContainer& entities);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTTREE_H
