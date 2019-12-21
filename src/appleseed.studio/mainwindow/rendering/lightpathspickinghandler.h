
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

// Qt headers.
#include <QObject>

// Forward declarations.
namespace appleseed { namespace studio { class LightPathsLayer; } }
namespace appleseed { namespace studio { class MouseCoordinatesTracker; } }
namespace appleseed { namespace studio { class ViewportWidget; } }
namespace renderer  { class Project; }
class QEvent;

namespace appleseed {
namespace studio {

class LightPathsPickingHandler
  : public QObject
{
    Q_OBJECT

  public:
    LightPathsPickingHandler(
        ViewportWidget*                     viewport_widget,
        const MouseCoordinatesTracker&      mouse_tracker,
        const renderer::Project&            project);

    void set_enabled(const bool enabled);

    void pick(const foundation::Vector2i& pixel, QString* lpe);
    void pick(const foundation::AABB2i& rect, QString* lpe);
    void pick(QString* lpe);

  private:
    ViewportWidget*                         m_viewport_widget;
    const renderer::Project&                m_project;
    bool                                    m_enabled;
    size_t                                  m_prev_query_x_min;
    size_t                                  m_prev_query_y_min;
    size_t                                  m_prev_query_x_max;
    size_t                                  m_prev_query_y_max;
    bool                                    m_prev_query_valid;
};

}   // namespace studio
}   // namespace appleseed
