
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Marius Avram, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_MATERIALDELAYEDACTIONS_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_MATERIALDELAYEDACTIONS_H

// appleseed.studio headers.
#include "mainwindow/rendering/renderingmanager.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/containers/dictionary.h"

// Forward declarations.
namespace applessed { namespace studio { class MaterialCollectionItem; } }
namespace renderer { class MasterRenderer; }
namespace renderer { class Project; }

namespace appleseed {
namespace studio {

// There delayed actions allow to schedule the creation and edition of materials
// right before rendering starts. They are used to live-edit materials during
// rendering.
template <typename CollectionItem, typename Material>
class MaterialCreationDelayedAction
  : public RenderingManager::IDelayedAction
{
  public:
    MaterialCreationDelayedAction(
        CollectionItem*                     parent,
        const foundation::Dictionary&       values)
      : m_parent(parent)
      , m_values(values)
    {
    }

    virtual void operator()(
        renderer::MasterRenderer&           master_renderer,
        renderer::Project&                  project) OVERRIDE
    {
        (*m_parent).template create<Material>(m_values);
    }

  private:
    CollectionItem*                         m_parent;
    const foundation::Dictionary            m_values;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_MATERIALDELAYEDACTIONS_H
