
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_FRAMEITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_FRAMEITEM_H

// appleseed.studio headers.
#include "mainwindow/project/entityactions.h"
#include "mainwindow/project/entitycreatorbase.h"
#include "mainwindow/project/entityvalueprovider.h"
#include "mainwindow/project/itembase.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QObject>

// Forward declarations.
namespace appleseed     { namespace studio { class AttributeEditor; } }
namespace appleseed     { namespace studio { class EntityEditorContext; } }
namespace foundation    { class Dictionary; }
namespace renderer      { class Frame; }

namespace appleseed {
namespace studio {

class FrameItem
  : public ItemBase
  , public EntityCreatorBase
  , public IEntityValueProvider
{
    Q_OBJECT

  public:
    FrameItem(
        EntityEditorContext&    editor_context,
        renderer::Frame*        frame);

  virtual const foundation::Dictionary get_values() APPLESEED_OVERRIDE;

  private slots:
    void slot_edit_accepted(foundation::Dictionary values);

  private:
    friend class EntityEditionAction<FrameItem>;

    renderer::Frame* m_frame;

    virtual void slot_edit(AttributeEditor* attribute_editor) APPLESEED_OVERRIDE;
    void edit(const foundation::Dictionary& values);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_FRAMEITEM_H
