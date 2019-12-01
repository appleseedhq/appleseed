
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

// Interface header.
#include "collapsiblesectionwidget.h"

namespace appleseed {
namespace studio {

//
// CollapsibleSectionWidget class implementation.
//

CollapsibleSectionWidget::CollapsibleSectionWidget(const QString& title, QWidget* parent)
  : QWidget(parent)
  , m_content_height(0)
{
    m_toggle_button.setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
    m_toggle_button.setArrowType(Qt::ArrowType::RightArrow);
    m_toggle_button.setText(title);
    m_toggle_button.setCheckable(true);
    m_toggle_button.setChecked(false);

    m_upper_line.setFrameShape(QFrame::HLine);
    m_upper_line.setFrameShadow(QFrame::Sunken);
    m_upper_line.setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Maximum);

    m_content_area.setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
    m_content_area.setFixedHeight(0);

    m_main_layout.setVerticalSpacing(0);
    m_main_layout.setContentsMargins(0, 0, 0, 0);
    m_main_layout.addWidget(&m_toggle_button, 0, 0, 1, 1, Qt::AlignLeft);
    m_main_layout.addWidget(&m_upper_line, 0, 2, 1, 1);
    m_main_layout.addWidget(&m_content_area, 1, 0, 1, -1);

    setLayout(&m_main_layout);

    connect(&m_toggle_button, SIGNAL(toggled(bool)), SLOT(slot_on_click(const bool)));
}

void CollapsibleSectionWidget::set_content_layout(QLayout* content_layout)
{
    delete m_content_area.layout();
    m_content_area.setLayout(content_layout);

    m_content_height = content_layout->sizeHint().height();
}

void CollapsibleSectionWidget::slot_on_click(const bool checked)
{
    m_toggle_button.setArrowType(checked ? Qt::ArrowType::DownArrow : Qt::ArrowType::RightArrow);
    m_content_area.setFixedHeight(checked ? m_content_height : 0);
}

}   // namespace studio
}   // namespace appleseed
