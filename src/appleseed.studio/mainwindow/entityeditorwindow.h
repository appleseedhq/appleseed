
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_ENTITYEDITORWINDOW_H
#define APPLESEED_STUDIO_MAINWINDOW_ENTITYEDITORWINDOW_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/containers/dictionary.h"

// Qt headers.
#include <QComboBox>
#include <QLineEdit>
#include <QObject>
#include <QString>
#include <QVariant>
#include <QWidget>

// Standard headers.
#include <map>
#include <string>
#include <vector>

// Forward declarations.
namespace Ui        { class EntityEditorWindow; }
class QFormLayout;

namespace appleseed {
namespace studio {

class EntityEditorWindow
  : public QWidget
{
    Q_OBJECT

  public:
    typedef std::vector<foundation::Dictionary> InputWidgetCollection;

    EntityEditorWindow(
        QWidget*                        parent,
        const std::string&              window_title,
        const QVariant&                 payload);

    ~EntityEditorWindow();

    void build_form(const InputWidgetCollection& input_widgets);

  signals:
    void accepted(QVariant payload, foundation::Dictionary values);

  private:
    // Not wrapped in std::auto_ptr<> to avoid pulling in the UI definition code.
    Ui::EntityEditorWindow*             m_ui;

    InputWidgetCollection               m_input_widgets;
    const QVariant                      m_payload;

    class IValueReader
      : public foundation::NonCopyable
    {
      public:
        virtual std::string read() const = 0;
    };

    class LineEditValueReader
      : public IValueReader
    {
      public:
        explicit LineEditValueReader(QLineEdit* line_edit)
          : m_line_edit(line_edit)
        {
        }

        virtual std::string read() const
        {
            return m_line_edit->text().toStdString();
        }

      private:
        QLineEdit*  m_line_edit;
    };

    class ComboBoxValueReader
      : public IValueReader
    {
      public:
        explicit ComboBoxValueReader(QComboBox* combo_box)
          : m_combo_box(combo_box)
        {
        }

        virtual std::string read() const
        {
            const QVariant data = m_combo_box->itemData(m_combo_box->currentIndex());
            return data.value<QString>().toStdString();
        }

      private:
        QComboBox*  m_combo_box;
    };

    typedef std::map<std::string, IValueReader*> ValueReaderCollection;

    ValueReaderCollection               m_value_readers;
    
    void create_input_widget(
        QFormLayout*                    layout,
        const foundation::Dictionary&   widget_params);

    void create_text_box_input_widget(
        QFormLayout*                    layout,
        const foundation::Dictionary&   widget_params);

    void create_entity_picker_input_widget(
        QFormLayout*                    layout,
        const foundation::Dictionary&   widget_params);

    void create_dropdown_list_input_widget(
        QFormLayout*                    layout,
        const foundation::Dictionary&   widget_params);

  private slots:
    void slot_accept();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_ENTITYEDITORWINDOW_H
