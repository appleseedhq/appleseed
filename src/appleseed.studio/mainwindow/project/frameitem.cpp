
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "mainwindow/project/multimodelcollectionitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/singlemodelentityeditorformfactory.h"
#include "mainwindow/project/tools.h"
#include "mainwindow/rendering/renderingmanager.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/postprocessing.h"
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Qt headers.
#include <QString>
#include <QWidget>

// Standard headers.
#include <memory>
#include <utility>

using namespace foundation;
using namespace renderer;

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

    addChild(
        m_post_processing_stage_collection_item =
            new MultiModelCollectionItem<PostProcessingStage, Frame, FrameItem>(
                editor_context,
                new_guid(),
                EntityTraits<PostProcessingStage>::get_human_readable_collection_type_name(),
                *m_frame,
                this));
    m_post_processing_stage_collection_item->add_items(m_frame->post_processing_stages());
}

Dictionary FrameItem::get_values() const
{
    return m_frame->get_parameters();
}

void FrameItem::add_item(PostProcessingStage* stage)
{
    m_post_processing_stage_collection_item->add_item(stage);
}

void FrameItem::slot_edit(AttributeEditor* attribute_editor)
{
    std::unique_ptr<EntityEditor::IFormFactory> form_factory(
        new SingleModelEntityEditorFormFactory(
            m_frame->get_name(),
            FrameFactory::get_input_metadata()));

    const Scene& scene = *m_editor_context.m_project.get_scene();
    std::unique_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<Scene>(scene));

    if (attribute_editor)
    {
        attribute_editor->edit(
            std::move(form_factory),
            std::move(entity_browser),
            std::unique_ptr<CustomEntityUI>(),
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
            m_editor_context.m_settings,
            std::move(form_factory),
            std::move(entity_browser),
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
            std::unique_ptr<RenderingManager::IScheduledAction>(
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
    delete m_post_processing_stage_collection_item;

    m_frame = m_editor_context.m_project_builder.edit_frame(values);

    set_title(m_frame->get_name());

    addChild(
        m_post_processing_stage_collection_item =
            new MultiModelCollectionItem<PostProcessingStage, Frame, FrameItem>(
                m_editor_context,
                new_guid(),
                EntityTraits<PostProcessingStage>::get_human_readable_collection_type_name(),
                *m_frame,
                this));
    m_post_processing_stage_collection_item->add_items(m_frame->post_processing_stages());
}

}   // namespace studio
}   // namespace appleseed
