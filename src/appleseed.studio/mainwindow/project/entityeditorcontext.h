
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORCONTEXT_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORCONTEXT_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Forward declarations.
namespace appleseed { namespace studio { class ItemRegistry; } }
namespace appleseed { namespace studio { class RenderingManager; } }

namespace appleseed {
namespace studio {

//
// This context is passed to all entities of the project explorer.
//

class EntityEditorContext
  : public foundation::NonCopyable
{
  public:
    EntityEditorContext(
        ItemRegistry&       item_registry,
        RenderingManager&   rendering_manager);

    // todo: add settings
    // todo: add attribute editor
    ItemRegistry&           m_item_registry;
    RenderingManager&       m_rendering_manager;

};


//
// EntityEditorContext class implementation.
//

inline EntityEditorContext::EntityEditorContext(
    ItemRegistry&           item_registry,
    RenderingManager&       rendering_manager)
  : m_item_registry(item_registry)
  , m_rendering_manager(rendering_manager)
{
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORCONTEXT_H
