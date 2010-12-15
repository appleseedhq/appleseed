
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYITEM_H

// appleseed.studio headers.
#include "mainwindow/project/entityitembase.h"
#include "mainwindow/project/itemtypemap.h"

// Qt headers.
#include <QObject>

// Forward declarations.
namespace appleseed { namespace studio { class BSDFCollectionItem; }}
namespace appleseed { namespace studio { class ColorCollectionItem; }}
namespace appleseed { namespace studio { class EDFCollectionItem; }}
namespace appleseed { namespace studio { class LightCollectionItem; }}
namespace appleseed { namespace studio { class MaterialCollectionItem; }}
namespace appleseed { namespace studio { class ObjectCollectionItem; }}
namespace appleseed { namespace studio { class ObjectInstanceCollectionItem; }}
namespace appleseed { namespace studio { class ProjectBuilder; }}
namespace appleseed { namespace studio { class SurfaceShaderCollectionItem; }}
namespace appleseed { namespace studio { class TextureCollectionItem; }}
namespace appleseed { namespace studio { class TextureInstanceCollectionItem; }}
namespace renderer  { class Assembly; }
namespace renderer  { class ColorEntity; }
namespace renderer  { class BSDF; }
namespace renderer  { class EDF; }
namespace renderer  { class Light; }
namespace renderer  { class Material; }
namespace renderer  { class Object; }
namespace renderer  { class ObjectInstance; }
namespace renderer  { class Scene; }
namespace renderer  { class SurfaceShader; }
namespace renderer  { class Texture; }
namespace renderer  { class TextureInstance; }
class QMenu;

namespace appleseed {
namespace studio {

class AssemblyItem
  : public EntityItemBase
{
    Q_OBJECT

  public:
    AssemblyItem(
        renderer::Scene&            scene,
        renderer::Assembly&         assembly,
        ProjectBuilder&             project_builder);

    virtual QMenu* get_single_item_context_menu() const;

    void add_item(renderer::ColorEntity& color);
    void add_item(renderer::Texture& texture);
    void add_item(renderer::TextureInstance& texture_instance);
    void add_item(renderer::BSDF& bsdf);
    void add_item(renderer::EDF& edf);
    void add_item(renderer::SurfaceShader& surface_shader);
    void add_item(renderer::Material& material);
    void add_item(renderer::Light& light);
    void add_item(renderer::Object& object);
    void add_item(renderer::ObjectInstance& object_instance);

  private:
    renderer::Scene&                m_scene;
    ProjectBuilder&                 m_project_builder;
    renderer::Assembly&             m_assembly;

    ColorCollectionItem*            m_color_collection_item;
    TextureCollectionItem*          m_texture_collection_item;
    TextureInstanceCollectionItem*  m_texture_instance_collection_item;
    BSDFCollectionItem*             m_bsdf_collection_item;
    EDFCollectionItem*              m_edf_collection_item;
    SurfaceShaderCollectionItem*    m_surface_shader_collection_item;
    MaterialCollectionItem*         m_material_collection_item;
    LightCollectionItem*            m_light_collection_item;
    ObjectCollectionItem*           m_object_collection_item;
    ObjectInstanceCollectionItem*   m_object_instance_collection_item;

    template <typename EntityContainer>
    typename ItemTypeMap<EntityContainer>::T* add_collection_item(EntityContainer& entities);

  private slots:
    void slot_instantiate();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYITEM_H
