
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

#ifndef APPLESEED_STUDIO_UTILITY_MISCELLANEOUS_H
#define APPLESEED_STUDIO_UTILITY_MISCELLANEOUS_H

// Qt headers.
#include <Qt>

// Forward declarations.
class QLayout;
class QMessageBox;
class QShortcut;
class QWidget;

namespace appleseed {
namespace studio {

// Disable the blue focus rectangle of certain widgets. Mac OS X only.
void disable_osx_focus_rect(QWidget* widget);

// Set the minimum width of a QMessageBox.
void set_minimum_width(QMessageBox& msgbox, const int minimum_width);

// Create a keyboard shortcut that is active for a given window and its
// child widgets, but not for its top-level children like subwindows.
QShortcut* create_window_local_shortcut(QWidget* parent, const Qt::Key key);

// Remove all widgets and sub-layouts from a layout.
void clear_layout(QLayout* layout);

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_UTILITY_MISCELLANEOUS_H
