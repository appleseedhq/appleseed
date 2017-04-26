
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
#include "miscellaneous.h"

// appleseed.studio headers.
#include "utility/interop.h"
#include "utility/settingskeys.h"

// appleseed.renderer headers.
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// appleseed.shared headers.
#include "application/application.h"

// OpenImageIO headers.
#include "foundation/platform/oiioheaderguards.h"
BEGIN_OIIO_INCLUDES
#include "OpenImageIO/texture.h"
END_OIIO_INCLUDES

// Qt headers.
#include <QDir>
#include <QFileInfo>
#include <QGridLayout>
#include <QIcon>
#include <QKeySequence>
#include <QLayout>
#include <QLayoutItem>
#include <QMessageBox>
#include <QMutex>
#include <QMutexLocker>
#include <QPixmap>
#include <QShortcut>
#include <QSpacerItem>
#include <QString>
#include <QStringList>
#include <Qt>
#include <QWidget>

// Standard headers.
#include <sstream>
#include <string>
#include <vector>

using namespace appleseed::shared;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

const QString g_appleseed_image_files_filter = "Bitmap Files (*.png *.exr);;OpenEXR (*.exr);;PNG (*.png);;All Files (*.*)";

QString get_oiio_image_files_filter()
{
    static QString filter;
    static QMutex mutex;

    QMutexLocker locker(&mutex);

    if (filter.isEmpty())
    {
        stringstream sstr;

        string extensions;
        OIIO::getattribute("extension_list", extensions);

        vector<string> formats;
        split(extensions, ";", formats);

        for (const_each<vector<string> > i = formats; i; ++i)
        {
            const string::size_type sep = i->find_first_of(':');
            const string format = i->substr(0, sep);
            const string extlist = i->substr(sep + 1);

            vector<string> exts;
            split(extlist, ",", exts);

            sstr << upper_case(format) << " Files (";

            for (const_each<vector<string> > e = exts; e; ++e)
            {
                if (e.it() != exts.begin())
                    sstr << " ";
                sstr << "*." << *e;
            }

            sstr << ");;";
        }

        sstr << "All Files (*.*)";

        filter = QString::fromStdString(sstr.str());
    }

    return filter;
}

QString combine_paths(const QString& lhs, const QString& rhs)
{
    QString result(lhs);

    if (!result.endsWith(QDir::separator()))
        result.append(QDir::separator());

    result.append(rhs);

    return QDir::toNativeSeparators(result);
}

QString make_app_path(const QString& path)
{
    return combine_paths(Application::get_root_path(), path);
}

QString combine_name_and_shortcut(const QString& name, const QKeySequence& shortcut)
{
    return name + " (" + shortcut.toString(QKeySequence::NativeText) + ")";
}

bool file_exists(const QString& path)
{
    const QFileInfo info(path);
    return info.exists() && info.isFile();
}

QIcon load_icons(const QString& base_name)
{
    const QString base_icon_filepath(make_app_path("icons/%1.png").arg(base_name));
    const QString hover_icon_filepath(make_app_path("icons/%1_hover.png").arg(base_name));
    const QString disabled_icon_filepath(make_app_path("icons/%1_disabled.png").arg(base_name));

    QIcon icon(base_icon_filepath);

    if (file_exists(hover_icon_filepath))
        icon.addPixmap(QPixmap(hover_icon_filepath), QIcon::Active);

    if (file_exists(disabled_icon_filepath))
        icon.addPixmap(QPixmap(disabled_icon_filepath), QIcon::Disabled);

    return icon;
}

namespace
{
    QString get_value(const ParamArray& settings, const QString& key)
    {
        return settings.get_path_optional<QString>(key.toAscii().constData());
    }

    void set_value(ParamArray& settings, const QString& key, const QString& value)
    {
        settings.insert_path(key.toAscii().constData(), value);
    }
}

QString get_open_filename(
    QWidget*                parent,
    const QString&          caption,
    const QString&          filter,
    ParamArray&             settings,
    const QString&          settings_key,
    QFileDialog::Options    options)
{
    const QString dir = get_value(settings, settings_key + SETTINGS_LAST_DIRECTORY);
    QString selected_filter = get_value(settings, settings_key + SETTINGS_SELECTED_FILTER);

    const QString filepath =
        QFileDialog::getOpenFileName(
            parent,
            caption,
            dir,
            filter,
            &selected_filter,
            options);

    set_value(settings, settings_key + SETTINGS_SELECTED_FILTER, selected_filter);

    if (!filepath.isEmpty())
    {
        set_value(
            settings,
            settings_key + SETTINGS_LAST_DIRECTORY,
            QDir::toNativeSeparators(QFileInfo(filepath).path()));
    }

    return filepath;
}

QStringList get_open_filenames(
    QWidget*                parent,
    const QString&          caption,
    const QString&          filter,
    ParamArray&             settings,
    const QString&          settings_key,
    QFileDialog::Options    options)
{
    const QString dir = get_value(settings, settings_key + SETTINGS_LAST_DIRECTORY);
    QString selected_filter = get_value(settings, settings_key + SETTINGS_SELECTED_FILTER);

    const QStringList filepaths =
        QFileDialog::getOpenFileNames(
            parent,
            caption,
            dir,
            filter,
            &selected_filter,
            options);

    set_value(settings, settings_key + SETTINGS_SELECTED_FILTER, selected_filter);

    if (!filepaths.isEmpty())
    {
        set_value(
            settings,
            settings_key + SETTINGS_LAST_DIRECTORY,
            QDir::toNativeSeparators(QFileInfo(filepaths.first()).path()));
    }

    return filepaths;
}

QString get_save_filename(
    QWidget*                parent,
    const QString&          caption,
    const QString&          filter,
    ParamArray&             settings,
    const QString&          settings_key,
    QFileDialog::Options    options)
{
    const QString dir = get_value(settings, settings_key + SETTINGS_LAST_DIRECTORY);
    QString selected_filter = get_value(settings, settings_key + SETTINGS_SELECTED_FILTER);

    const QString filepath =
        QFileDialog::getSaveFileName(
            parent,
            caption,
            dir,
            filter,
            &selected_filter,
            options);

    set_value(settings, settings_key + SETTINGS_SELECTED_FILTER, selected_filter);

    if (!filepath.isEmpty())
    {
        set_value(
            settings,
            settings_key + SETTINGS_LAST_DIRECTORY,
            QDir::toNativeSeparators(QFileInfo(filepath).path()));
    }

    return filepath;
}

void disable_osx_focus_rect(QWidget* widget)
{
    widget->setAttribute(Qt::WA_MacShowFocusRect, false);
}

void set_minimum_width(QMessageBox& msgbox, const int minimum_width)
{
    QSpacerItem* spacer =
        new QSpacerItem(
            minimum_width,
            0,
            QSizePolicy::Minimum,
            QSizePolicy::Expanding);

    QGridLayout* layout = static_cast<QGridLayout*>(msgbox.layout());

    layout->addItem(
        spacer,
        layout->rowCount(),         // row
        0,                          // column
        1,                          // row span
        layout->columnCount());     // column span
}

QShortcut* create_window_local_shortcut(QWidget* parent, const int key)
{
    return
        new QShortcut(
            QKeySequence(key),
            parent,
            0,
            0,
            Qt::WidgetWithChildrenShortcut);
}

void clear_layout(QLayout* layout)
{
    for (int i = layout->count(); i > 0; --i)
    {
        QLayoutItem* item = layout->takeAt(0);

        if (item->layout())
            clear_layout(item->layout());
        else item->widget()->deleteLater();

        delete item;
    }
}

}   // namespace studio
}   // namespace appleseed
