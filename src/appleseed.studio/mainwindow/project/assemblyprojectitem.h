
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYPROJECTITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYPROJECTITEM_H

// appleseed.studio headers.
#include "mainwindow/project/entityprojectitem.h"
#include "mainwindow/project/projectitemtypemap.h"

// Qt headers.
#include <QObject>

// Forward declarations.
namespace appleseed { namespace studio { class ProjectBuilder; }}
namespace appleseed { namespace studio { class ProjectItemBase; }}
namespace renderer  { class Assembly; }
namespace renderer  { class ColorEntity; }
namespace renderer  { class BSDF; }
namespace renderer  { class EDF; }
namespace renderer  { class Light; }
namespace renderer  { class Material; }
namespace renderer  { class Object; }
namespace renderer  { class ObjectInstance; }
namespace renderer  { class SurfaceShader; }
namespace renderer  { class Texture; }
namespace renderer  { class TextureInstance; }
class QMenu;

namespace appleseed {
namespace studio {

class AssemblyProjectItem
  : public EntityProjectItem
{
    Q_OBJECT

  public:
    AssemblyProjectItem(
        ProjectBuilder&     project_builder,
        renderer::Assembly& assembly);

    virtual QMenu* get_context_menu() const;

    void add_item(const renderer::ColorEntity& color);
    void add_item(const renderer::Texture& texture);
    void add_item(const renderer::TextureInstance& texture_instance);
    void add_item(const renderer::BSDF& bsdf);
    void add_item(const renderer::EDF& edf);
    void add_item(const renderer::SurfaceShader& surface_shader);
    void add_item(const renderer::Material& material);
    void add_item(const renderer::Light& light);
    void add_item(const renderer::Object& object);
    void add_item(const renderer::ObjectInstance& object_instance);

  private:
    ProjectBuilder&     m_project_builder;
    renderer::Assembly& m_assembly;

    ProjectItemBase*    m_color_collection_item;
    ProjectItemBase*    m_texture_collection_project_item;
    ProjectItemBase*    m_texture_instance_collection_project_item;

    template <typename EntityContainer>
    typename ProjectItemTypeMap<EntityContainer>::T* add_item(EntityContainer& entities);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYPROJECTITEM_H
