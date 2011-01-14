
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTTREE_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTTREE_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Forward declarations.
namespace appleseed { namespace studio { class AssemblyCollectionItem; } }
namespace appleseed { namespace studio { class ProjectBuilder; } }
namespace renderer  { class Assembly; }
namespace renderer  { class AssemblyInstance; }
namespace renderer  { class ColorEntity; }
namespace renderer  { class EnvironmentEDF; }
namespace renderer  { class EnvironmentShader; }
namespace renderer  { class Project; }
namespace renderer  { class Texture; }
namespace renderer  { class TextureInstance; }
class QTreeWidget;

namespace appleseed {
namespace studio {

class ProjectTree
  : foundation::NonCopyable
{
  public:
    explicit ProjectTree(
        QTreeWidget*        tree_widget);

    ~ProjectTree();

    void initialize(
        renderer::Project&  project,
        ProjectBuilder&     project_builder);

    void add_item(renderer::ColorEntity* color);
    void add_item(renderer::Texture* texture);
    void add_item(renderer::TextureInstance* texture_instance);
    void add_item(renderer::EnvironmentEDF* environment_edf);
    void add_item(renderer::EnvironmentShader* environment_shader);
    void add_item(renderer::Assembly* assembly);
    void add_item(renderer::AssemblyInstance* assembly_instance);

    AssemblyCollectionItem& get_assembly_collection_item() const;

  private:
    struct Impl;
    Impl* impl;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTTREE_H
