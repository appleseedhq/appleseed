
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
#include "entityeditorwindow.h"

// UI definition header.
#include "ui_entityeditorwindow.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// Qt headers.
#include <QDialogButtonBox>
#include <QShortcut>
#include <QString>
#include <Qt>

// Standard headers.
#include <utility>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

EntityEditorWindow::EntityEditorWindow(
    QWidget*                                         parent,
    const std::string&                               window_title,
    const Project&                                   project,
    ParamArray&                                      settings,
    std::unique_ptr<EntityEditor::IFormFactory>      form_factory,
    std::unique_ptr<EntityEditor::IEntityBrowser>    entity_browser,
    std::unique_ptr<CustomEntityUI>                  custom_entity_ui,
    const Dictionary&                                values)
  : WindowBase(parent, "entity_editor_window")
  , m_ui(new Ui::EntityEditorWindow())
{
    m_ui->setupUi(this);

    setWindowTitle(QString::fromStdString(window_title));
    setWindowFlags(Qt::Tool);
    setAttribute(Qt::WA_DeleteOnClose);

    m_entity_editor.reset(
        new EntityEditor(
            m_ui->scrollarea_contents,
            project,
            settings,
            std::move(form_factory),
            std::move(entity_browser),
            std::move(custom_entity_ui),
            values));

    m_initial_values = m_entity_editor->get_values();

    create_connections();

    WindowBase::load_settings();
}

EntityEditorWindow::~EntityEditorWindow()
{
    delete m_ui;
}

void EntityEditorWindow::create_connections()
{
    connect(
        m_entity_editor.get(), SIGNAL(signal_applied(foundation::Dictionary)),
        SIGNAL(signal_applied(foundation::Dictionary)));

    connect(m_ui->buttonbox, SIGNAL(accepted()), SLOT(slot_accept()));
    connect(m_ui->buttonbox, SIGNAL(rejected()), SLOT(slot_cancel()));

    connect(
        create_window_local_shortcut(this, QKeySequence("Ctrl+Return")), SIGNAL(activated()),
        SLOT(slot_accept()));
    connect(
        create_window_local_shortcut(this, QKeySequence("Ctrl+Enter")), SIGNAL(activated()),
        SLOT(slot_accept()));

    connect(
        create_window_local_shortcut(this, QKeySequence(Qt::Key_Escape)), SIGNAL(activated()),
        SLOT(slot_cancel()));
}

void EntityEditorWindow::slot_accept()
{
    emit signal_accepted(m_entity_editor->get_values());

    close();
}

void EntityEditorWindow::slot_cancel()
{
    if (m_initial_values != m_entity_editor->get_values())
        emit signal_canceled(m_initial_values);

    close();
}

}   // namespace studio
}   // namespace appleseed
