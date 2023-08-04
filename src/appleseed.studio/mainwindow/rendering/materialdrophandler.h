
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Thibault Vergne, The appleseedhq Organization
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

#pragma once

// appleseed.renderer headers.
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/objectinstance.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Qt headers.
#include <QObject>

// Standard headers.
#include <memory>

namespace appleseed { namespace studio { class RenderingManager; } }

class QString;

namespace appleseed {
namespace studio {

class MaterialDropHandler
  : public QObject
{
    Q_OBJECT

  public:
    MaterialDropHandler(
        const renderer::Project&        project,
        RenderingManager&               rendering_manager);

  public slots:
    void slot_material_dropped(
        const foundation::Vector2d&     drop_pos,
        const QString&                  material_name);

  private:
    const renderer::Project&            m_project;
    RenderingManager&                   m_rendering_manager;
    foundation::Vector2d                m_drop_pos;
    std::string                         m_material_name;

    void assign_material(const renderer::ObjectInstance::Side side);

  private slots:
    void slot_apply_to_front();
    void slot_apply_to_back();
    void slot_apply_to_both();
};

}   // namespace studio
}   // namespace appleseed
