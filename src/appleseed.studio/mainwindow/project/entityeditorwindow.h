
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORWINDOW_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORWINDOW_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/core/exceptions/exceptionnotimplemented.h"
#include "foundation/utility/containers/dictionary.h"

// Qt headers.
#include <QObject>
#include <QString>
#include <QWidget>

// Standard headers.
#include <map>
#include <memory>
#include <string>
#include <vector>

// Forward declarations.
namespace Ui { class EntityEditorWindow; }
class QFormLayout;
class QSignalMapper;

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
            WidgetDefinitionCollection&     definitions) const = 0;

      protected:
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

    class IEntityBrowser
      : public foundation::NonCopyable
    {
      public:
        virtual ~IEntityBrowser() {}

        virtual foundation::StringDictionary get_entities(
            const std::string&              type) const = 0;
    };

    EntityEditorWindow(
        QWidget*                            parent,
        const std::string&                  window_title,
        std::auto_ptr<IFormFactory>         form_factory,
        std::auto_ptr<IEntityBrowser>       entity_browser,
        const foundation::Dictionary&       values = foundation::Dictionary());

    ~EntityEditorWindow();

    void rebuild_form(const foundation::Dictionary& values);

  signals:
    void signal_accepted(foundation::Dictionary values);

  private:
    class IWidgetProxy
      : public foundation::NonCopyable
    {
      public:
        virtual ~IWidgetProxy() {}

        virtual void set(const std::string& value) = 0;

        virtual std::string get() const = 0;
    };

    struct Impl;

    typedef std::map<std::string, IWidgetProxy*> WidgetProxyCollection;

    // Not wrapped in std::auto_ptr<> to avoid pulling in the UI definition code.
    Ui::EntityEditorWindow*             m_ui;

    std::auto_ptr<IFormFactory>         m_form_factory;
    std::auto_ptr<IEntityBrowser>       m_entity_browser;

    QFormLayout*                        m_form_layout;
    WidgetDefinitionCollection          m_widget_definitions;
    WidgetProxyCollection               m_widget_proxies;

    QSignalMapper*                      m_entity_picker_signal_mapper;
    QSignalMapper*                      m_color_picker_signal_mapper;

    void create_form_layout();

    foundation::Dictionary get_widget_definition(const std::string& name) const;

    void create_input_widget(const foundation::Dictionary& definition);
    void create_text_box_input_widget(const foundation::Dictionary& definition);
    void create_dropdown_list_input_widget(const foundation::Dictionary& definition);
    void create_entity_picker_input_widget(const foundation::Dictionary& definition);
    void create_color_picker_input_widget(const foundation::Dictionary& definition);

    foundation::Dictionary get_values() const;

  private slots:
    void slot_rebuild_form();

    void slot_open_entity_browser(const QString& widget_name);
    void slot_entity_browser_accept(QString widget_name, QString page_name, QString entity_name);

    void slot_open_color_picker(const QString& widget_name);

    void slot_accept();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORWINDOW_H
