
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

// Interface header.
#include "tools.h"

// appleseed.studio headers.
#include "mainwindow/project/entityeditorwindow.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/searchpaths.h"

// Qt headers.
#include <QDir>
#include <QFileInfo>
#include <QInputDialog>
#include <QLineEdit>
#include <QObject>
#include <QString>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

string get_entity_name_dialog(
    QWidget*        parent,
    const string&   title,
    const string&   label,
    const string&   text)
{
    QString result;
    bool ok;

    do
    {
        result =
            QInputDialog::getText(
                parent,
                QString::fromStdString(title),
                QString::fromStdString(label),
                QLineEdit::Normal,
                QString::fromStdString(text),
                &ok);
    } while (ok && result.isEmpty());

    return ok ? result.toStdString() : string();
}

void open_entity_editor(
    QWidget*                                parent,
    const string&                           window_title,
    const Project&                          project,
    auto_ptr<EntityEditor::IFormFactory>    form_factory,
    auto_ptr<EntityEditor::IEntityBrowser>  entity_browser,
    auto_ptr<CustomEntityUI>                custom_entity_ui,
    const Dictionary&                       values,
    QObject*                                receiver,
    const char*                             slot_apply,
    const char*                             slot_accept,
    const char*                             slot_cancel)
{
    EntityEditorWindow* editor_window =
        new EntityEditorWindow(
            parent,
            window_title,
            project,
            form_factory,
            entity_browser,
            custom_entity_ui,
            values);

    QObject::connect(
        editor_window, SIGNAL(signal_applied(foundation::Dictionary)),
        receiver, slot_apply);

    QObject::connect(
        editor_window, SIGNAL(signal_accepted(foundation::Dictionary)),
        receiver, slot_accept);

    QObject::connect(
        editor_window, SIGNAL(signal_canceled(foundation::Dictionary)),
        receiver, slot_cancel);

    editor_window->showNormal();
    editor_window->activateWindow();
}

void open_entity_editor(
    QWidget*                                parent,
    const string&                           window_title,
    const Project&                          project,
    auto_ptr<EntityEditor::IFormFactory>    form_factory,
    auto_ptr<EntityEditor::IEntityBrowser>  entity_browser,
    const Dictionary&                       values,
    QObject*                                receiver,
    const char*                             slot_apply,
    const char*                             slot_accept,
    const char*                             slot_cancel)
{
    open_entity_editor(
        parent,
        window_title,
        project,
        form_factory,
        entity_browser,
        auto_ptr<CustomEntityUI>(),
        values,
        receiver,
        slot_apply,
        slot_accept,
        slot_cancel);
}

void open_entity_editor(
    QWidget*                                parent,
    const string&                           window_title,
    const Project&                          project,
    auto_ptr<EntityEditor::IFormFactory>    form_factory,
    auto_ptr<EntityEditor::IEntityBrowser>  entity_browser,
    QObject*                                receiver,
    const char*                             slot_apply,
    const char*                             slot_accept,
    const char*                             slot_cancel)
{
    open_entity_editor(
        parent,
        window_title,
        project,
        form_factory,
        entity_browser,
        auto_ptr<CustomEntityUI>(),
        Dictionary(),
        receiver,
        slot_apply,
        slot_accept,
        slot_cancel);
}

QString find_path_in_searchpaths(const SearchPaths& s, const QString& filename)
{
    const QFileInfo file_info(filename);

    if (file_info.isAbsolute())
    {
        if (s.size() > 0)
        {
            for (size_t i = s.size() - 1; i >= 0; --i)
            {
                QString search_path(QString::fromStdString(s[i]));
                const QFileInfo search_path_info(search_path);

                if (search_path_info.isRelative() && s.has_root_path())
                {
                    search_path = QDir::cleanPath(
                        QString::fromStdString(s.get_root_path()) +
                        QDir::separator() +
                        search_path);

                    const QDir search_dir(search_path);
                    const QString relative_path = search_dir.relativeFilePath(filename);
                    const QFileInfo relative_file_info(relative_path);

                    if (!relative_file_info.isAbsolute() && !(relative_path == filename))
                        return relative_path;
                }
            }
        }

        if (s.has_root_path())
        {
            const QDir root_dir(QString::fromStdString(s.get_root_path()));
            return root_dir.relativeFilePath(filename);
        }
    }

    return filename;
}

}   // namespace studio
}   // namespace appleseed
