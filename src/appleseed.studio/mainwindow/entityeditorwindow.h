
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
#include <memory>
#include <string>
#include <vector>

// Forward declarations.
namespace Ui { class EntityEditorWindow; }
class QFormLayout;

namespace appleseed {
namespace studio {

class EntityEditorWindow
  : public QWidget
{
    Q_OBJECT

  public:
    typedef std::vector<foundation::Dictionary> WidgetDefinitionCollection;

    class IFormFactory
      : public foundation::NonCopyable
    {
      public:
        virtual ~IFormFactory() {}

        virtual void update(
            const foundation::Dictionary&   values,
            WidgetDefinitionCollection&     definitions) = 0;

      protected:
        typedef EntityEditorWindow::WidgetDefinitionCollection WidgetDefinitionCollection;

        static std::string get_value(
            const foundation::Dictionary&   values,
            const std::string&              name,
            const std::string&              default_value)
        {
            return values.strings().exist(name)
                ? values.strings().get<std::string>(name)
                : default_value;
        }
    };

    EntityEditorWindow(
        QWidget*                            parent,
        const std::string&                  window_title,
        std::auto_ptr<IFormFactory>         form_factory,
        const QVariant&                     payload);

    ~EntityEditorWindow();

  signals:
    void accepted(QVariant payload, foundation::Dictionary values);

  private:
    class IValueReader
      : public foundation::NonCopyable
    {
      public:
        virtual ~IValueReader() {}

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

    // Not wrapped in std::auto_ptr<> to avoid pulling in the UI definition code.
    Ui::EntityEditorWindow*             m_ui;

    std::auto_ptr<IFormFactory>         m_form_factory;
    const QVariant                      m_payload;

    QFormLayout*                        m_form_layout;
    WidgetDefinitionCollection          m_widget_definitions;
    ValueReaderCollection               m_value_readers;

    void create_form_layout();
    void rebuild_form(const foundation::Dictionary& values);

    void create_input_widget(const foundation::Dictionary& definition);
    void create_text_box_input_widget(const foundation::Dictionary& definition);
    void create_entity_picker_input_widget(const foundation::Dictionary& definition);
    void create_dropdown_list_input_widget(const foundation::Dictionary& definition);

    foundation::Dictionary get_values() const;

  private slots:
    void slot_accept();
    void slot_rebuild_form();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_ENTITYEDITORWINDOW_H
