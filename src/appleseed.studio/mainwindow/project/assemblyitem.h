
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "mainwindow/project/basegroupitem.h"
#include "mainwindow/project/entityactions.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QList>
#include <QObject>

// Forward declarations.
namespace appleseed { namespace studio { template <typename Entity, typename ParentEntity, typename ParentItem> class CollectionItem; } }
namespace appleseed { namespace studio { class EntityEditorContext; } }
namespace appleseed { namespace studio { template <typename Entity, typename EntityItem, typename ParentEntity> class InstanceCollectionItem; } }
namespace appleseed { namespace studio { class ItemBase; } }
namespace appleseed { namespace studio { class MaterialCollectionItem; } }
namespace appleseed { namespace studio { class ObjectCollectionItem; } }
namespace appleseed { namespace studio { class ObjectInstanceItem; } }
namespace renderer  { class Assembly; }
namespace renderer  { class BaseGroup; }
namespace renderer  { class BSDF; }
namespace renderer  { class BSSRDF; }
namespace renderer  { class EDF; }
namespace renderer  { class Light; }
namespace renderer  { class Material; }
namespace renderer  { class Object; }
namespace renderer  { class ObjectInstance; }
namespace renderer  { class SurfaceShader; }
class QMenu;

namespace appleseed {
namespace studio {

class AssemblyItem
  : public BaseGroupItem
{
    Q_OBJECT

  public:
    AssemblyItem(
        EntityEditorContext&        editor_context,
        renderer::Assembly&         assembly,
        renderer::BaseGroup&        parent,
        BaseGroupItem*              parent_item);

    virtual QMenu* get_single_item_context_menu() const APPLESEED_OVERRIDE;

    void add_item(renderer::BSDF* bsdf);
    void add_item(renderer::BSSRDF* bssrdf);
    void add_item(renderer::EDF* edf);
    void add_item(renderer::SurfaceShader* surface_shader);
    void add_item(renderer::Material* material);
    void add_item(renderer::Light* light);
    void add_item(renderer::Object* object);
    void add_item(renderer::ObjectInstance* object_instance);

    typedef InstanceCollectionItem<renderer::ObjectInstance, ObjectInstanceItem, renderer::Assembly> ObjectInstanceCollectionItem;

    MaterialCollectionItem& get_material_collection_item() const;
    ObjectCollectionItem& get_object_collection_item() const;
    ObjectInstanceCollectionItem& get_object_instance_collection_item() const;

    void instantiate(const std::string& name);

  private:
    friend class EntityInstantiationAction<AssemblyItem>;
    friend class EntityDeletionAction<AssemblyItem>;

    renderer::Assembly&             m_assembly;
    renderer::BaseGroup&            m_parent;
    BaseGroupItem*                  m_parent_item;

    typedef CollectionItem<renderer::BSDF, renderer::Assembly, AssemblyItem> BSDFCollectionItem;
    typedef CollectionItem<renderer::BSSRDF, renderer::Assembly, AssemblyItem> BSSRDFCollectionItem;
    typedef CollectionItem<renderer::EDF, renderer::Assembly, AssemblyItem> EDFCollectionItem;
    typedef CollectionItem<renderer::SurfaceShader, renderer::Assembly, AssemblyItem> SurfaceShaderCollectionItem;
    typedef CollectionItem<renderer::Light, renderer::Assembly, AssemblyItem> LightCollectionItem;

    BSDFCollectionItem*             m_bsdf_collection_item;
    BSSRDFCollectionItem*           m_bssrdf_collection_item;
    EDFCollectionItem*              m_edf_collection_item;
    SurfaceShaderCollectionItem*    m_surface_shader_collection_item;
    MaterialCollectionItem*         m_material_collection_item;
    LightCollectionItem*            m_light_collection_item;
    ObjectCollectionItem*           m_object_collection_item;
    ObjectInstanceCollectionItem*   m_object_instance_collection_item;

    virtual void slot_instantiate() APPLESEED_OVERRIDE;
    void do_instantiate(const std::string& name);

    virtual void delete_multiple(const QList<ItemBase*>& items) APPLESEED_OVERRIDE;
    void do_delete();

    template <typename Entity, typename EntityContainer>
    CollectionItem<Entity, renderer::Assembly, AssemblyItem>* add_single_model_collection_item(EntityContainer& entities);

    template <typename Entity, typename EntityContainer>
    CollectionItem<Entity, renderer::Assembly, AssemblyItem>* add_multi_model_collection_item(EntityContainer& entities);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ASSEMBLYITEM_H
