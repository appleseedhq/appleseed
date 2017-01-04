
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

#ifndef APPLESEED_STUDIO_UTILITY_FOLDABLEPANELWIDGET_H
#define APPLESEED_STUDIO_UTILITY_FOLDABLEPANELWIDGET_H

// Qt headers.
#include <QObject>
#include <QWidget>

// Forward declarations.
class QFrame;
class QPushButton;
class QString;

namespace appleseed {
namespace studio {

class FoldablePanelWidget
  : public QWidget
{
    Q_OBJECT

  public:
    FoldablePanelWidget(const QString& title, QWidget* parent = 0);

    QFrame* container();

    bool is_folded() const;
    void set_folded(const bool folded);
    void fold();
    void unfold();

  private:
    QPushButton*    m_button;
    QFrame*         m_container;

  private slots:
    void slot_fold_unfold();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_UTILITY_FOLDABLEPANELWIDGET_H
