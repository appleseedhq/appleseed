
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTBUILDER_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTBUILDER_H

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"
#include "renderer/api/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/containers/dictionary.h"

// Qt headers.
#include <QObject>

// Standard headers.
#include <string>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class Project; }
namespace appleseed { namespace studio { class ProjectTree; }}

namespace appleseed {
namespace studio {

class ProjectBuilder
  : public QObject
  , foundation::NonCopyable
{
    Q_OBJECT

  public:
    ProjectBuilder(
        renderer::Project&                  project,
        ProjectTree&                        project_tree);

    void insert_assembly(
        const std::string&                  name) const;

    void insert_assembly_instance(
        const std::string&                  name,
        renderer::Assembly&                 assembly) const;

    void insert_bsdf(
        renderer::Assembly&                 assembly,
        const foundation::Dictionary&       values) const;

    void insert_surface_shader(
        renderer::Assembly&                 assembly,
        const foundation::Dictionary&       values) const;

    void insert_material(
        renderer::Assembly&                 assembly,
        const foundation::Dictionary&       values) const;

    void insert_objects(
        renderer::Assembly&                 assembly,
        const std::string&                  path) const;

    void insert_textures(
        renderer::Assembly&                 assembly,
        const std::string&                  path) const;

    void insert_textures(
        const std::string&                  path) const;

    void notify_project_modification() const;

  signals:
    void project_modified() const;

  private:
    renderer::Project&                      m_project;
    ProjectTree&                            m_project_tree;

    renderer::BSDFFactoryRegistrar          m_bsdf_factory_registrar;
    renderer::SurfaceShaderFactoryRegistrar m_surface_shader_factory_registrar;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTBUILDER_H