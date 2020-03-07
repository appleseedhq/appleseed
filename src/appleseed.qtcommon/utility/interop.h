
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

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/scalar.h"
#include "foundation/string/string.h"

// Qt headers.
#include <QColor>
#include <QString>

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <iostream>
#include <string>

namespace appleseed {
namespace qtcommon {

template <typename T>
inline int unit_color_component_to_int(const T x)
{
    return
        std::min(
            255,
            foundation::truncate<int>(
                foundation::saturate(x) * T(256.0)));
}

template <typename T>
inline T int_to_unit_color_component(const int x)
{
    return static_cast<T>(x) * T(1.0 / 255.0);
}

template <typename T, size_t N>
QColor color_to_qcolor(const foundation::Color<T, N>& color);

template <typename T>
inline QColor color_to_qcolor(const foundation::Color<T, 3>& color)
{
    return
        QColor(
            unit_color_component_to_int(color.r),
            unit_color_component_to_int(color.g),
            unit_color_component_to_int(color.b));
}

template <typename T>
inline QColor color_to_qcolor(const foundation::Color<T, 4>& color)
{
    return
        QColor(
            unit_color_component_to_int(color.r),
            unit_color_component_to_int(color.g),
            unit_color_component_to_int(color.b),
            unit_color_component_to_int(color.a));
}

template <typename T>
inline foundation::Color<T, 3> qcolor_to_color3(const QColor& color)
{
    return
        foundation::Color<T, 3>(
            int_to_unit_color_component<T>(color.red()),
            int_to_unit_color_component<T>(color.green()),
            int_to_unit_color_component<T>(color.blue()));
}

template <typename T>
inline foundation::Color<T, 4> qcolor_to_color4(const QColor& color)
{
    return
        foundation::Color<T, 4>(
            int_to_unit_color_component<T>(color.red()),
            int_to_unit_color_component<T>(color.green()),
            int_to_unit_color_component<T>(color.blue()),
            int_to_unit_color_component<T>(color.alpha()));
}

template <typename ColorType>
ColorType qcolor_to_color(const QColor& color);

template <>
inline foundation::Color3f qcolor_to_color(const QColor& color)
{
    return qcolor_to_color3<float>(color);
}

template <>
inline foundation::Color3d qcolor_to_color(const QColor& color)
{
    return qcolor_to_color3<double>(color);
}

template <>
inline foundation::Color4f qcolor_to_color(const QColor& color)
{
    return qcolor_to_color4<float>(color);
}

template <>
inline foundation::Color4d qcolor_to_color(const QColor& color)
{
    return qcolor_to_color4<double>(color);
}

}   // namespace qtcommon
}   // namespace appleseed

namespace foundation
{

template <>
inline std::string to_string(const QString& s)
{
    return s.toStdString();
}

template <typename T>
T from_string(const QString& s)
{
    return from_string<T>(s.toStdString());
}

template <>
inline QString from_string(const std::string& s)
{
    return QString::fromStdString(s);
}

}   // namespace foundation

namespace std
{

inline std::ostream& operator<<(std::ostream& s, const QColor& color)
{
    return
        s << color.red() << ' '
          << color.green() << ' '
          << color.blue() << ' '
          << color.alpha();
}

}   // namespace std
