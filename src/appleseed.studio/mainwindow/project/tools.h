
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_TOOLS_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_TOOLS_H

// appleseed.studio headers.
#include "mainwindow/project/entityeditor.h"

// appleseed.renderer headers.
#include "renderer/api/entity.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <memory>
#include <string>

// Forward declarations.
namespace appleseed     { namespace studio { class EntityEditorWindow; } }
namespace foundation    { class Dictionary; }
namespace renderer      { class Project; }
class QObject;
class QWidget;

namespace appleseed {
namespace studio {

template <typename EntityContainer>
std::string get_name_suggestion(
    const std::string&                          prefix,
    const EntityContainer&                      entities);

std::string get_entity_name_dialog(
    QWidget*                                    parent,
    const std::string&                          title,
    const std::string&                          label,
    const std::string&                          text);

void open_entity_editor(
    QWidget*                                        parent,
    const std::string&                              window_title,
    const renderer::Project&                        project,
    std::auto_ptr<EntityEditor::IFormFactory>       form_factory,
    std::auto_ptr<EntityEditor::IEntityBrowser>     entity_browser,
    std::auto_ptr<ICustomEntityUI>                  custom_entity_ui,
    const foundation::Dictionary&                   values,
    QObject*                                        receiver,
    const char*                                     slot_apply,
    const char*                                     slot_accept,
    const char*                                     slot_cancel);

void open_entity_editor(
    QWidget*                                        parent,
    const std::string&                              window_title,
    const renderer::Project&                        project,
    std::auto_ptr<EntityEditor::IFormFactory>       form_factory,
    std::auto_ptr<EntityEditor::IEntityBrowser>     entity_browser,
    const foundation::Dictionary&                   values,
    QObject*                                        receiver,
    const char*                                     slot_apply,
    const char*                                     slot_accept,
    const char*                                     slot_cancel);

void open_entity_editor(
    QWidget*                                        parent,
    const std::string&                              window_title,
    const renderer::Project&                        project,
    std::auto_ptr<EntityEditor::IFormFactory>       form_factory,
    std::auto_ptr<EntityEditor::IEntityBrowser>     entity_browser,
    QObject*                                        receiver,
    const char*                                     slot_apply,
    const char*                                     slot_accept,
    const char*                                     slot_cancel);

//
// Implementation.
//

template <typename EntityContainer>
std::string get_name_suggestion(
    const std::string&      prefix,
    const EntityContainer&  entities)
{
    int max_number = 0;

    for (foundation::const_each<EntityContainer> i = entities; i; ++i)
    {
        const renderer::Entity& entity = *i;

        const std::string entity_name = entity.get_name();
        const std::string entity_name_prefix = entity_name.substr(0, prefix.size());

        if (entity_name_prefix == prefix)
        {
            try
            {
                const std::string entity_name_suffix = entity_name.substr(prefix.size());
                const int number = foundation::from_string<int>(entity_name_suffix);

                if (max_number < number)
                    max_number = number;
            }
            catch (const foundation::ExceptionStringConversionError&)
            {
            }
        }
    }

    return prefix + foundation::to_string(max_number + 1);
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_TOOLS_H
