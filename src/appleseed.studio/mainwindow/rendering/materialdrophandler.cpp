
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

// Interface header.
#include "materialdrophandler.h"

// appleseed.studio headers.
#include "mainwindow/rendering/renderingmanager.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/scenepicker.h"

// Qt headers.
#include <QMenu>
#include <QString>

namespace appleseed {
namespace studio {

MaterialDropHandler::MaterialDropHandler(
    const renderer::Project&        project,
    RenderingManager&               rendering_manager)
  : m_project(project), m_rendering_manager(rendering_manager)
{
}

void MaterialDropHandler::slot_material_dropped(
    const foundation::Vector2d&     drop_pos,
    const QString&                  material_name)
{
    m_drop_pos = drop_pos;
    m_material_name = material_name.toStdString();

    if (!m_project.has_trace_context())
    {
        RENDERER_LOG_INFO("the scene must be rendering or must have been rendered at least once for drag and drop to be available.");
        return;
    }

    QMenu* selection_menu = new QMenu();
    connect(selection_menu->addAction("Front Side"), SIGNAL(triggered()), SLOT(slot_apply_to_front()));
    connect(selection_menu->addAction("Back Side"), SIGNAL(triggered()), SLOT(slot_apply_to_back()));
    connect(selection_menu->addAction("Both Sides"), SIGNAL(triggered()), SLOT(slot_apply_to_both()));

    connect(selection_menu, SIGNAL(aboutToHide()), selection_menu, SLOT(deleteLater()));

    selection_menu->exec(QCursor::pos());
}

namespace
{
    struct MaterialSlot
    {
        std::string                     m_name;
        renderer::ObjectInstance::Side  m_side;
    };

    class ApplyDroppedMaterial
        : public RenderingManager::IScheduledAction
    {
      public:
        ApplyDroppedMaterial(
            renderer::ObjectInstance&           instance,
            const std::vector<MaterialSlot>&    material_slots,
            const std::string&                  material_name)
            : m_instance(instance),
            m_material_slots(material_slots),
            m_material_name(material_name)
        {
        }

        void operator()(renderer::Project& project) override
        {
            for (const MaterialSlot &slot : m_material_slots)
            {
                m_instance.assign_material(slot.m_name.c_str(), slot.m_side, m_material_name.c_str());
            }
        }

      private:
        renderer::ObjectInstance&       m_instance;
        const std::vector<MaterialSlot> m_material_slots;
        const std::string               m_material_name;
    };
}

void MaterialDropHandler::assign_material(const renderer::ObjectInstance::Side side)
{
    const renderer::ScenePicker scene_picker(m_project);
    const renderer::ScenePicker::PickingResult result = scene_picker.pick(m_drop_pos);
    renderer::ObjectInstance* instance = result.m_object_instance;

    std::vector<MaterialSlot> material_slots;
    if (side & renderer::ObjectInstance::Side::FrontSide)
    {
        for (auto item = instance->get_front_material_mappings().begin();
            item != instance->get_front_material_mappings().end();
            ++item)
        {
            material_slots.push_back({ item.key(), renderer::ObjectInstance::Side::FrontSide });
        }
    }
    if (side & renderer::ObjectInstance::Side::BackSide)
    {
        for (auto item = instance->get_back_material_mappings().begin();
            item != instance->get_back_material_mappings().end();
            ++item)
        {
            material_slots.push_back({ item.key(), renderer::ObjectInstance::Side::BackSide });
        }
    }

    m_rendering_manager.schedule_or_execute(
        std::unique_ptr<RenderingManager::IScheduledAction>(
            new ApplyDroppedMaterial(
                *instance,
                material_slots,
                m_material_name)));
}

void MaterialDropHandler::slot_apply_to_front()
{
    assign_material(renderer::ObjectInstance::Side::FrontSide);
}

void MaterialDropHandler::slot_apply_to_back()
{
    assign_material(renderer::ObjectInstance::Side::BackSide);
}

void MaterialDropHandler::slot_apply_to_both()
{
    assign_material(renderer::ObjectInstance::Side::BothSides);
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/rendering/moc_cpp_materialdrophandler.cxx"
