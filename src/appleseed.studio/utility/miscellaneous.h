
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

#ifndef APPLESEED_STUDIO_UTILITY_MISCELLANEOUS_H
#define APPLESEED_STUDIO_UTILITY_MISCELLANEOUS_H

// Qt headers.
#include <QFileDialog>
#include <QList>

// Forward declarations.
namespace renderer  { class ParamArray; }
class QIcon;
class QLayout;
class QMessageBox;
class QShortcut;
class QString;
class QStringList;
class QWidget;

namespace appleseed {
namespace studio {

// File dialog filter string for bitmap files supported by appleseed's own image subsystem.
extern const QString g_appleseed_image_files_filter;

// Return the file dialog filter string for image file formats supported by OpenImageIO.
QString get_oiio_image_files_filter();

// Combine two filesystem paths and convert the result to native separators.
QString combine_paths(const QString& lhs, const QString& rhs);

// Combine the application's base path and a given relative path.
QString make_app_path(const QString& path);

// Check whether a file exists.
bool file_exists(const QString& path);

// Load an icon and its variants (hover, disabled...) from the application's icons directory.
QIcon load_icons(const QString& base_name);

QString get_extension(renderer::ParamArray& settings, const QString& target_dialog);

QString get_open_filename(
    QWidget*                parent,
    const QString&          caption,
    const QString&          filter,
    renderer::ParamArray&   settings,
    const QString&          settings_key,
    QFileDialog::Options    options = 0);

QStringList get_open_filenames(
    QWidget*                parent,
    const QString&          caption,
    const QString&          filter,
    renderer::ParamArray&   settings,
    const QString&          settings_key,
    QFileDialog::Options    options = 0);

QString get_save_filename(
    QWidget*                parent,
    const QString&          caption,
    const QString&          filter,
    renderer::ParamArray&   settings,
    const QString&          settings_key,
    QFileDialog::Options    options = 0);

// Disable the blue focus rectangle of certain widgets. OS X only.
void disable_osx_focus_rect(QWidget* widget);

// Set the minimum width of a QMessageBox.
void set_minimum_width(QMessageBox& msgbox, const int minimum_width);

// Create a keyboard shortcut that is active for a given window and its
// child widgets, but not for its top-level children like subwindows.
QShortcut* create_window_local_shortcut(QWidget* parent, const int key);

// Remove all widgets and sub-layouts from a layout.
void clear_layout(QLayout* layout);

// Create a QList from a single item (workaround for pre-C++11 compilers).
template <typename T>
QList<T> make_qlist(const T& item);

// Convert a QList<FromType> to a QList<ToType> by static_cast<>'ing items to type ToType.
template <typename ToType, typename FromType>
QList<ToType> qlist_static_cast(const QList<FromType>& list);


//
// Implementation.
//

template <typename T>
QList<T> make_qlist(const T& item)
{
    QList<T> result;
    result.append(item);
    return result;
}

template <typename ToType, typename FromType>
QList<ToType> qlist_static_cast(const QList<FromType>& list)
{
    QList<ToType> result;
    result.reserve(list.size());

    for (int i = 0, e = list.size(); i < e; ++i)
        result.append(static_cast<ToType>(list[i]));

    return result;
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_UTILITY_MISCELLANEOUS_H
