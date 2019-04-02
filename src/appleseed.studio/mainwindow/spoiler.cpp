
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
#include "spoiler.h"

// QT headers
#include <QPropertyAnimation>

namespace appleseed {
namespace studio {

    //
    // Collapsing section implementation.
    //
    Spoiler::Spoiler(const QString& title, const int animation_duration, QWidget* parent)
            : QWidget(parent)
            , m_animation_duration(animation_duration)
    {
        m_toggle_button.setStyleSheet("QToolButton { border: none; }");
        m_toggle_button.setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
        m_toggle_button.setArrowType(Qt::ArrowType::RightArrow);
        m_toggle_button.setText(title);
        m_toggle_button.setCheckable(true);
        m_toggle_button.setChecked(false);

        m_upper_line.setFrameShape(QFrame::HLine);
        m_upper_line.setFrameShadow(QFrame::Sunken);
        m_upper_line.setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Maximum);

        m_content_area.setStyleSheet("QScrollArea { border: none; }");
        m_content_area.setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);

        // start out collapsed
        m_content_area.setMaximumHeight(0);
        m_content_area.setMinimumHeight(0);

        // animate collapsing and expanding
        m_toggle_animation.addAnimation(new QPropertyAnimation(this, "minimumHeight"));
        m_toggle_animation.addAnimation(new QPropertyAnimation(this, "maximumHeight"));
        m_toggle_animation.addAnimation(new QPropertyAnimation(&m_content_area, "maximumHeight"));

        m_main_layout.setVerticalSpacing(0);
        m_main_layout.setContentsMargins(0, 0, 0, 0);
        int row = 0;
        m_main_layout.addWidget(&m_toggle_button, row, 0, 1, 1, Qt::AlignLeft);
        m_main_layout.addWidget(&m_upper_line, row++, 2, 1, 1);
        m_main_layout.addWidget(&m_content_area, row, 0, 1, 3);

        setLayout(&m_main_layout);
        connect(&m_toggle_button, SIGNAL(toggled(bool)), SLOT(slot_spoiler_interaction(const bool)));
    }

    void Spoiler::set_content_layout(QLayout& content_layout)
    {
        delete m_content_area.layout();
        m_content_area.setLayout(&content_layout);
        const auto collapsed_height = sizeHint().height() - m_content_area.maximumHeight();
        auto content_height = content_layout.sizeHint().height();
        for (int i = 0; i < m_toggle_animation.animationCount() - 1; ++i)
        {
            QPropertyAnimation* spoiler_animation = static_cast<QPropertyAnimation*>(m_toggle_animation.animationAt(i));
            spoiler_animation->setDuration(m_animation_duration);
            spoiler_animation->setStartValue(collapsed_height);
            spoiler_animation->setEndValue(collapsed_height + content_height);
        }
        QPropertyAnimation* contentAnimation = static_cast<QPropertyAnimation*>(m_toggle_animation.animationAt(
                m_toggle_animation.animationCount() - 1));
        contentAnimation->setDuration(m_animation_duration);
        contentAnimation->setStartValue(0);
        contentAnimation->setEndValue(content_height);
    }

    void Spoiler::slot_spoiler_interaction(const bool checked)
    {
        m_toggle_button.setArrowType(checked ? Qt::ArrowType::DownArrow : Qt::ArrowType::RightArrow);
        m_toggle_animation.setDirection(checked ? QAbstractAnimation::Forward : QAbstractAnimation::Backward);
        m_toggle_animation.start();
    }
}
}
