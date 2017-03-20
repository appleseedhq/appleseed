
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

// Interface header.
#include "frameitem.h"

// appleseed.studio headers.
#include "mainwindow/project/attributeeditor.h"
#include "mainwindow/project/entitybrowser.h"
#include "mainwindow/project/entityeditor.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/singlemodelentityeditorformfactory.h"
#include "mainwindow/project/tools.h"
#include "mainwindow/rendering/renderingmanager.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Qt headers.
#include <QString>
#include <QWidget>

// Standard headers.
#include <memory>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

namespace
{
    const UniqueID g_class_uid = new_guid();
}

FrameItem::FrameItem(
    EntityEditorContext&    editor_context,
    Frame*                  frame)
  : ItemBase(editor_context, g_class_uid, frame->get_name())
  , m_frame(frame)
{
    set_allow_deletion(false);
}

void FrameItem::slot_edit(AttributeEditor* attribute_editor)
{
    auto_ptr<EntityEditor::IFormFactory> form_factory(
        new SingleModelEntityEditorFormFactory(
            m_frame->get_name(),
            FrameFactory::get_input_metadata()));

    const Scene& scene = *m_editor_context.m_project.get_scene();
    std::auto_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<Scene>(scene));

    if (attribute_editor)
    {
        attribute_editor->edit(
            form_factory,
            entity_browser,
            auto_ptr<CustomEntityUI>(),
            m_frame->get_parameters(),
            this,
            SLOT(slot_edit_accepted(foundation::Dictionary)));
    }
    else
    {
        open_entity_editor(
            treeWidget(),
            "Edit Frame",
            m_editor_context.m_project,
            form_factory,
            entity_browser,
            m_frame->get_parameters(),
            this,
            SLOT(slot_edit_accepted(foundation::Dictionary)),
            SLOT(slot_edit_accepted(foundation::Dictionary)),
            SLOT(slot_edit_accepted(foundation::Dictionary)));
    }
}

void FrameItem::slot_edit_accepted(Dictionary values)
{
    if (m_editor_context.m_rendering_manager.is_rendering())
    {
        m_editor_context.m_rendering_manager.schedule(
            auto_ptr<RenderingManager::IScheduledAction>(
                new EntityEditionAction<FrameItem>(this, values)));

        m_editor_context.m_rendering_manager.reinitialize_rendering();
    }
    else
    {
        catch_entity_creation_errors(&FrameItem::edit, values, "Frame");
    }
}

void FrameItem::edit(const Dictionary& values)
{
    m_frame = m_editor_context.m_project_builder.edit_frame(values);

    set_title(QString::fromAscii(m_frame->get_name()));
}

const Dictionary FrameItem::get_values()
{
    return m_frame->get_parameters();
}

}   // namespace studio
}   // namespace appleseed
