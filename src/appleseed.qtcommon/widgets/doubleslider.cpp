
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
#include "doubleslider.h"

namespace appleseed {
namespace qtcommon {

namespace
{
    const double Multiplier = 1000.0;
}

DoubleSlider::DoubleSlider(QWidget* parent)
  : QSlider(parent)
{
    connect(
        this, &QSlider::valueChanged,
        this, static_cast<void (DoubleSlider::*)(const int)>(&DoubleSlider::setValue));

    setSingleStep(1);
}

DoubleSlider::DoubleSlider(const Qt::Orientation orientation, QWidget* parent)
  : QSlider(orientation, parent)
{
    connect(
        this, &QSlider::valueChanged,
        this, static_cast<void (DoubleSlider::*)(const int)>(&DoubleSlider::setValue));

    setSingleStep(1);
}

void DoubleSlider::setValue(const int value)
{
    emit valueChanged(static_cast<double>(value) / Multiplier);
}

void DoubleSlider::setValue(const double value)
{
    QSlider::setValue(static_cast<int>(value * Multiplier));
}

void DoubleSlider::setRange(const double min, const double max)
{
    QSlider::setRange(
        static_cast<int>(min * Multiplier),
        static_cast<int>(max * Multiplier));

    emit rangeChanged(min, max);
}

void DoubleSlider::setMinimum(const double min)
{
    QSlider::setMinimum(static_cast<int>(min * Multiplier));

    emit rangeChanged(minimum(), maximum());
}

void DoubleSlider::setMaximum(const double max)
{
    QSlider::setMaximum(static_cast<int>(max * Multiplier));

    emit rangeChanged(minimum(), maximum());
}

double DoubleSlider::minimum() const
{
    return static_cast<double>(QSlider::minimum()) / Multiplier;
}

double DoubleSlider::maximum() const
{
    return static_cast<double>(QSlider::maximum()) / Multiplier;
}

void DoubleSlider::setPageStep(const double step)
{
    QSlider::setPageStep(static_cast<int>(step * Multiplier));
}

double DoubleSlider::pageStep() const
{
    return static_cast<double>(QSlider::pageStep()) / Multiplier;
}

double DoubleSlider::value() const
{
    return static_cast<double>(QSlider::value()) / Multiplier;
}

}   // namespace qtcommon
}   // namespace appleseed
