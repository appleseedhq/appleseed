
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTEXPLORER_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTEXPLORER_H

// appleseed.studio headers.
#include "mainwindow/project/projecttree.h"

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"
#include "renderer/api/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/containers/dictionary.h"

// Qt headers.
#include <QList>
#include <QObject>
#include <QString>
#include <QVariant>

// Standard headers.
#include <map>

// Forward declarations.
namespace renderer  { class Assembly; }
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
    ProjectExplorer(
        renderer::Project&  project,
        QTreeWidget*        tree_widget);

  signals:
    void project_modified();

  private:
    renderer::Project&                          m_project;
    QTreeWidget*                                m_tree_widget;

    ProjectTree                                 m_project_tree;

    renderer::BSDFFactoryRegistrar              m_bsdf_factory_registrar;
    renderer::SurfaceShaderFactoryRegistrar     m_surface_shader_factory_registrar;

    QMenu* build_generic_context_menu() const;
    QMenu* build_context_menu(const QList<QTreeWidgetItem*>& items) const;

  private slots:
    void slot_context_menu(const QPoint& point);

/*
    void slot_instantiate_assembly();

    void slot_import_objects_to_assembly();
    void slot_import_textures_to_assembly();
    void slot_import_textures_to_scene();

    void slot_add_bsdf_to_assembly();
    void slot_add_surface_shader_to_assembly();
    void slot_add_material_to_assembly();
    void slot_create_entity(QVariant payload, foundation::Dictionary values);

    void slot_assign_material_to_object_instance();
    void slot_do_assign_material_to_object_instance(
        QList<QVariant>     items_data,
        QString             page_name,
        QString             entity_name);
*/
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTEXPLORER_H
