
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

#pragma once

// appleseed.studio headers.
#include "mainwindow/project/customentityui.h"
#include "mainwindow/project/entityeditor.h"
#include "utility/windowbase.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"

// Qt headers.
#include <QObject>

// Standard headers.
#include <memory>
#include <string>

// Forward declarations.
namespace renderer  { class ParamArray; }
namespace renderer  { class Project; }
namespace Ui        { class EntityEditorWindow; }
class QWidget;

namespace appleseed {
namespace studio {

class EntityEditorWindow
  : public WindowBase
{
    Q_OBJECT

  public:
    // Constructor.
    EntityEditorWindow(
        QWidget*                                        parent,
        const std::string&                              window_title,
        const renderer::Project&                        project,
        renderer::ParamArray&                           settings,
        std::unique_ptr<EntityEditor::IFormFactory>     form_factory,
        std::unique_ptr<EntityEditor::IEntityBrowser>   entity_browser,
        std::unique_ptr<CustomEntityUI>                 custom_entity_ui,
        const foundation::Dictionary&                   values = foundation::Dictionary());

    // Destructor.
    ~EntityEditorWindow() override;

  signals:
    void signal_applied(foundation::Dictionary values);
    void signal_accepted(foundation::Dictionary values);
    void signal_canceled(foundation::Dictionary values);

  private:
    // Not wrapped in std::unique_ptr<> to avoid pulling in the UI definition code.
    Ui::EntityEditorWindow*         m_ui;

    std::unique_ptr<EntityEditor>   m_entity_editor;
    foundation::Dictionary          m_initial_values;

    void create_connections();

  private slots:
    void slot_accept();
    void slot_cancel();
};

}   // namespace studio
}   // namespace appleseed
