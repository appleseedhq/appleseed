
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
#include "foldablepanelwidget.h"

// Qt headers.
#include <QFrame>
#include <QLayout>
#include <QPushButton>
#include <QVariant>
#include <QVBoxLayout>

namespace appleseed {
namespace qtcommon {

FoldablePanelWidget::FoldablePanelWidget(const QString& title, QWidget* parent)
  : QWidget(parent)
  , m_button(new QPushButton(title))
  , m_container(new QFrame())
{
    setLayout(new QVBoxLayout());

    layout()->setSpacing(0);

    layout()->addWidget(m_button);
    layout()->addWidget(m_container);

    m_container->setProperty("hasFrame", true);

    connect(m_button, &QPushButton::clicked, this, &FoldablePanelWidget::slot_fold_unfold);
}

QFrame* FoldablePanelWidget::container()
{
    return m_container;
}

bool FoldablePanelWidget::is_folded() const
{
    return !m_container->isVisible();
}

void FoldablePanelWidget::set_folded(const bool folded)
{
    m_container->setVisible(!folded);
}

void FoldablePanelWidget::fold()
{
    set_folded(true);
}

void FoldablePanelWidget::unfold()
{
    set_folded(false);
}

void FoldablePanelWidget::slot_fold_unfold()
{
    set_folded(!is_folded());
}

}   // namespace qtcommon
}   // namespace appleseed
