
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_TEXTURECOLLECTIONITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_TEXTURECOLLECTIONITEM_H

// appleseed.studio headers.
#include "mainwindow/project/collectionitembase.h"

// appleseed.renderer headers.
#include "renderer/api/scene.h"
#include "renderer/api/texture.h"

// Qt headers.
#include <QObject>

// Forward declarations.
namespace appleseed { namespace studio { class ProjectBuilder; } }
namespace renderer  { class ParamArray; }
class QMenu;

namespace appleseed {
namespace studio {

class TextureCollectionItem
  : public CollectionItemBase<renderer::Texture>
{
    Q_OBJECT

  public:
    TextureCollectionItem(
        renderer::Scene&            scene,
        renderer::TextureContainer& textures,
        ProjectBuilder&             project_builder,
        renderer::ParamArray&       settings);

    TextureCollectionItem(
        renderer::Assembly&         assembly,
        renderer::TextureContainer& textures,
        ProjectBuilder&             project_builder,
        renderer::ParamArray&       settings);

    virtual QMenu* get_single_item_context_menu() const;

  public slots:
    void slot_import_textures();

  private:
    renderer::Assembly*             m_assembly;
    ProjectBuilder&                 m_project_builder;
    renderer::ParamArray&           m_settings;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_TEXTURECOLLECTIONITEM_H
