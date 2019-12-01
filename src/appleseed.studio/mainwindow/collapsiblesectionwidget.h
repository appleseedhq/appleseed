
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Smolin Oleg, The appleseedhq Organization
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

// Qt headers.
#include <QFrame>
#include <QGridLayout>
#include <QScrollArea>
#include <QString>
#include <QToolButton>
#include <QWidget>

namespace appleseed {
namespace studio {

//
// Collapsing section class.
//

class CollapsibleSectionWidget
  : public QWidget
{
    Q_OBJECT

  public:
    explicit CollapsibleSectionWidget(
        const QString&  title = "",
        QWidget*        parent = nullptr);

    void set_content_layout(QLayout* content_layout);

  private:
    QGridLayout     m_main_layout;
    QToolButton     m_toggle_button;
    QFrame          m_upper_line;
    QScrollArea     m_content_area;
    int             m_content_height;

  private slots:
    void slot_on_click(const bool);
};

}   // namespace studio
}   // namespace appleseed
