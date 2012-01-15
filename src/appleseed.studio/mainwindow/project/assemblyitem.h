
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYITEM_H

// appleseed.studio headers.
#include "mainwindow/project/entityitembase.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QObject>

// Forward declarations.
namespace appleseed { namespace studio { class ObjectCollectionItem; } }
namespace appleseed { namespace studio { class ObjectInstanceCollectionItem; } }
namespace appleseed { namespace studio { class ProjectBuilder; } }
namespace renderer  { class Assembly; }
namespace renderer  { class ColorEntity; }
namespace renderer  { class BSDF; }
namespace renderer  { class EDF; }
namespace renderer  { class Light; }
namespace renderer  { class Material; }
namespace renderer  { class Object; }
namespace renderer  { class ObjectInstance; }
namespace renderer  { class ParamArray; }
namespace renderer  { class Scene; }
namespace renderer  { class SurfaceShader; }
namespace renderer  { class Texture; }
namespace renderer  { class TextureInstance; }
class QMenu;

namespace appleseed {
namespace studio {

class AssemblyItem
  : public EntityItemBase<renderer::Assembly>
{
    Q_OBJECT

  public:
    AssemblyItem(
        renderer::Assembly*     assembly,
        renderer::Scene&        scene,
        ProjectBuilder&         project_builder,
        renderer::ParamArray&   settings);

    ~AssemblyItem();

    virtual QMenu* get_single_item_context_menu() const override;

    void add_item(renderer::ColorEntity* color);
    void add_item(renderer::Texture* texture);
    void add_item(renderer::TextureInstance* texture_instance);
    void add_item(renderer::BSDF* bsdf);
    void add_item(renderer::EDF* edf);
    void add_item(renderer::SurfaceShader* surface_shader);
    void add_item(renderer::Material* material);
    void add_item(renderer::Light* light);
    void add_item(renderer::Object* object);
    void add_item(renderer::ObjectInstance* object_instance);

    ObjectCollectionItem& get_object_collection_item() const;
    ObjectInstanceCollectionItem& get_object_instance_collection_item() const;

  private slots:
    void slot_instantiate();

  private:
    struct Impl;
    Impl* impl;

    virtual void slot_delete() override;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYITEM_H
