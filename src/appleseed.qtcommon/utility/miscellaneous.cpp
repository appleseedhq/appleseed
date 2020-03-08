
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
#include "miscellaneous.h"

// appleseed.qtcommon headers.
#include "utility/interop.h"

// appleseed.common headers.
#include "application/application.h"

// appleseed.renderer headers.
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/texture.h"
#include "foundation/platform/_endoiioheaders.h"

// Qt headers.
#include <QDir>
#include <QFile>
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

using namespace appleseed::common;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace qtcommon {

const QString g_qt_image_files_filter = "Bitmap Files (*.bmp *.jpg *.png *.tif);;BMP (*.bmp);;JPEG (*.jpg);;PNG (*.png);;TIFF (*.tif);;All Files (*.*)";

const QString SettingsLastDirectory = ".last_directory";
const QString SettingsSelectedFilter = ".selected_filter";

namespace
{
    std::string join_exts(const std::vector<std::string>& exts)
    {
        std::stringstream sstr;

        for (size_t i = 0, e = exts.size(); i < e; ++i)
        {
            if (i > 0)
                sstr << " ";
            sstr << "*." << exts[i];
        }

        return sstr.str();
    }
}

QString get_oiio_image_files_filter()
{
    static QString filter;
    static QMutex mutex;

    QMutexLocker locker(&mutex);

    if (filter.isEmpty())
    {
        std::stringstream sstr;

        // Retrieve the extension list as a string of the form "bmp:bmp;openexr:exr,sxr,mxr;png:png".
        std::string extension_list;
        OIIO::getattribute("extension_list", extension_list);

        // Split the extension list into an array of the form [ "bmp:bmp", "openexr:exr,sxr,mxr", "png:png" ].
        std::vector<std::string> formats;
        split(extension_list, ";", formats);

        // Collect all extensions into an array of the form [ "bmp", "exr", "png" ].
        std::vector<std::string> all_exts;
        for (const auto& format : formats)
        {
            const auto sep = format.find_first_of(':');
            const auto extlist = format.substr(sep + 1);
            split(extlist, ",", all_exts);
        }

        // The first filter shows all bitmap files.
        sstr << "Bitmap Files (" << join_exts(all_exts) << ");;";

        // Then one filter per file format.
        for (const auto& format : formats)
        {
            const auto sep = format.find_first_of(':');
            const auto name = format.substr(0, sep);
            const auto extlist = format.substr(sep + 1);
            std::vector<std::string> exts;
            split(extlist, ",", exts);
            sstr << upper_case(name) << " Files (" << join_exts(exts) << ");;";
        }

        // The last filter shows all files.
        sstr << "All Files (*.*)";

        filter = QString::fromStdString(sstr.str());
    }

    return filter;
}

QString get_project_files_filter(const int filters)
{
    QStringList filter_list;

    if (filters & ProjectFilesFilterAllProjects)
        filter_list << "Project Files (*.appleseed *.appleseedz)";

    if (filters & ProjectFilesFilterPlainProjects)
        filter_list << "Plain Project Files (*.appleseed)";

    if (filters & ProjectFilesFilterPackedProjects)
        filter_list << "Packed Project Files (*.appleseedz)";

    filter_list << "All Files (*.*)";

    return filter_list.join(";;");
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

bool file_exists(const QString& path)
{
    const QFileInfo info(path);
    return info.exists() && info.isFile();
}

QByteArray compute_file_hash(const QString& filepath, const QCryptographicHash::Algorithm hashAlgorithm)
{
    QFile file(filepath);

    if (file.open(QFile::ReadOnly))
    {
        QCryptographicHash hash(hashAlgorithm);
        if (hash.addData(&file))
            return hash.result();
    }

    return QByteArray();
}

QByteArray load_gl_shader(const QString& base_name)
{
    const QString resource_path(QString(":/shaders/%1").arg(base_name));

    QFile file(resource_path);
    file.open(QFile::ReadOnly);

    return file.readAll();
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
        return settings.get_path_optional<QString>(key.toUtf8().constData());
    }

    void set_value(ParamArray& settings, const QString& key, const QString& value)
    {
        settings.insert_path(key.toUtf8().constData(), value);
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
    const QString dir = get_value(settings, settings_key + SettingsLastDirectory);
    QString selected_filter = get_value(settings, settings_key + SettingsSelectedFilter);

    const QString filepath =
        QFileDialog::getOpenFileName(
            parent,
            caption,
            dir,
            filter,
            &selected_filter,
            options);

    set_value(settings, settings_key + SettingsSelectedFilter, selected_filter);

    if (filepath.isEmpty())
        return filepath;

    set_value(
        settings,
        settings_key + SettingsLastDirectory,
        QDir::toNativeSeparators(QFileInfo(filepath).path()));

    return QDir::toNativeSeparators(filepath);
}

QStringList get_open_filenames(
    QWidget*                parent,
    const QString&          caption,
    const QString&          filter,
    ParamArray&             settings,
    const QString&          settings_key,
    QFileDialog::Options    options)
{
    const QString dir = get_value(settings, settings_key + SettingsLastDirectory);
    QString selected_filter = get_value(settings, settings_key + SettingsSelectedFilter);

    QStringList filepaths =
        QFileDialog::getOpenFileNames(
            parent,
            caption,
            dir,
            filter,
            &selected_filter,
            options);

    set_value(settings, settings_key + SettingsSelectedFilter, selected_filter);

    if (filepaths.isEmpty())
        return filepaths;

    set_value(
        settings,
        settings_key + SettingsLastDirectory,
        QDir::toNativeSeparators(QFileInfo(filepaths.first()).path()));

    for (int i = 0; i < filepaths.size(); ++i)
    {
        filepaths[i] = QDir::toNativeSeparators(filepaths[i]);
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
    const QString dir = get_value(settings, settings_key + SettingsLastDirectory);
    QString selected_filter = get_value(settings, settings_key + SettingsSelectedFilter);

    QString filepath =
        QFileDialog::getSaveFileName(
            parent,
            caption,
            dir,
            filter,
            &selected_filter,
            options);

    set_value(settings, settings_key + SettingsSelectedFilter, selected_filter);

    if (filepath.isEmpty())
        return filepath;

    QFileInfo file_info(filepath);

    set_value(
        settings,
        settings_key + SettingsLastDirectory,
        QDir::toNativeSeparators(file_info.path()));

    // If the file name has no extension and the selected filter has a single extension
    // (i.e. it contains one *.ext substring) then add that extension to the file name.
    if (file_info.suffix().isEmpty() && selected_filter.count('*') == 1)
    {
        const int begin = selected_filter.indexOf("*") + 1;
        const int end = selected_filter.indexOf(")", begin);
        assert(begin > 0 && end > begin);
        filepath += selected_filter.mid(begin, end - begin);
    }

    return QDir::toNativeSeparators(filepath);
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

QShortcut* create_window_local_shortcut(QWidget* parent, const QKeySequence key_sequence)
{
    return
        new QShortcut(
            QKeySequence(key_sequence),
            parent,
            nullptr,
            nullptr,
            Qt::WindowShortcut);
}

QString combine_name_and_shortcut(const QString& name, const QKeySequence& shortcut)
{
    return name + " (" + shortcut.toString(QKeySequence::NativeText) + ")";
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

}   // namespace qtcommon
}   // namespace appleseed
