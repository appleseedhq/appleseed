
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECTEXPLORER_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECTEXPLORER_H

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QList>
#include <QObject>
#include <QString>
#include <QVariant>

// Standard headers.
#include <map>
#include <string>

// Forward declarations.
namespace renderer  { class Project; }
class QMenu;
class QPoint;
class QTreeWidget;
class QTreeWidgetItem;

namespace appleseed {
namespace studio {

class ProjectExplorer
  : public QObject
  , foundation::NonCopyable
{
    Q_OBJECT

  public:
    ProjectExplorer(QTreeWidget* tree_widget, renderer::Project* project);

  private:
    struct SceneItems
    {
        QTreeWidgetItem*    m_color_items;
        QTreeWidgetItem*    m_texture_items;
        QTreeWidgetItem*    m_texture_instance_items;
        QTreeWidgetItem*    m_environment_edf_items;
        QTreeWidgetItem*    m_environment_shader_items;
        QTreeWidgetItem*    m_assembly_items;
        QTreeWidgetItem*    m_assembly_instance_items;

        void clear();
    };

    struct AssemblyItems
    {
        QTreeWidgetItem*    m_assembly_item;
        QTreeWidgetItem*    m_color_items;
        QTreeWidgetItem*    m_texture_items;
        QTreeWidgetItem*    m_texture_instance_items;
        QTreeWidgetItem*    m_bsdf_items;
        QTreeWidgetItem*    m_edf_items;
        QTreeWidgetItem*    m_surface_shader_items;
        QTreeWidgetItem*    m_material_items;
        QTreeWidgetItem*    m_light_items;
        QTreeWidgetItem*    m_object_items;
        QTreeWidgetItem*    m_object_instance_items;
    };

    typedef std::map<foundation::UniqueID, AssemblyItems> AssemblyItemsMap;

    QTreeWidget*                            m_tree_widget;
    renderer::Project*                      m_project;
    SceneItems                              m_scene_items;
    AssemblyItemsMap                        m_assembly_items;

    renderer::BSDFFactoryRegistrar          m_bsdf_factory_registrar;
    renderer::SurfaceShaderFactoryRegistrar m_surface_shader_factory_registrar;

    void build_tree_widget();
    void build_assembly_branch(renderer::Assembly& assembly);

    QMenu* build_generic_context_menu() const;
    QMenu* build_context_menu(const QList<QTreeWidgetItem*>& items) const;
    QMenu* build_assembly_context_menu() const;
    QMenu* build_assembly_collection_context_menu() const;
    QMenu* build_bsdf_collection_context_menu() const;
    QMenu* build_material_collection_context_menu() const;
    QMenu* build_object_instance_context_menu() const;
    QMenu* build_surface_shader_collection_context_menu() const;
    QMenu* build_texture_collection_context_menu() const;

    void create_bsdf_entity(
        renderer::Assembly&                 assembly,
        const AssemblyItems&                assembly_items,
        const foundation::Dictionary&       values);

    void create_surface_shader_entity(
        renderer::Assembly&                 assembly,
        const AssemblyItems&                assembly_items,
        const foundation::Dictionary&       values);

    void create_material_entity(
        renderer::Assembly&                 assembly,
        const AssemblyItems&                assembly_items,
        const foundation::Dictionary&       values);

    void import_objects(
        renderer::Assembly&                 assembly,
        QTreeWidgetItem*                    object_items,
        QTreeWidgetItem*                    object_instance_items,
        const std::string&                  path) const;

    void import_textures(
        renderer::Assembly&                 assembly,
        QTreeWidgetItem*                    texture_items,
        QTreeWidgetItem*                    texture_instance_items,
        const std::string&                  path) const;

  private slots:
    void slot_context_menu(const QPoint& point);

    void slot_add_assembly();
    void slot_instantiate_assembly();

    void slot_import_objects_to_assembly();
    void slot_import_textures_to_assembly();

    void slot_add_bsdf_to_assembly();
    void slot_add_surface_shader_to_assembly();
    void slot_add_material_to_assembly();
    void slot_create_entity(QVariant payload, foundation::Dictionary values);

    void slot_assign_material_to_object_instance();
    void slot_do_assign_material_to_object_instance(
        QList<QVariant>                     items_data,
        QString                             page_name,
        QString                             entity_name);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECTEXPLORER_H
