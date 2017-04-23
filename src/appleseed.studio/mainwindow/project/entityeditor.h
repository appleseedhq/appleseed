
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITOR_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITOR_H

// appleseed.studio headers.
#include "mainwindow/project/customentityui.h"
#include "utility/inputwidgetproxies.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/containers/dictionary.h"

// Qt headers.
#include <QObject>
#include <QWidget>

// Standard headers.
#include <memory>
#include <string>
#include <vector>

// Forward declarations.
namespace renderer  { class Project; }
class QColor;
class QVBoxLayout;
class QFormLayout;
class QSignalMapper;
class QString;

namespace appleseed {
namespace studio {

class EntityEditor
  : public QObject
{
    Q_OBJECT

  public:
    typedef std::vector<foundation::Dictionary> InputMetadataCollection;

    class IFormFactory
      : public foundation::NonCopyable
    {
      public:
        virtual ~IFormFactory() {}

        virtual void update(
            const foundation::Dictionary&   values,
            InputMetadataCollection&        metadata) const = 0;

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

    EntityEditor(
        QWidget*                            parent,
        const renderer::Project&            project,
        std::auto_ptr<IFormFactory>         form_factory,
        std::auto_ptr<IEntityBrowser>       entity_browser,
        std::auto_ptr<CustomEntityUI>       custom_ui,
        const foundation::Dictionary&       values = foundation::Dictionary());

    foundation::Dictionary get_values() const;

    void rebuild_form(const foundation::Dictionary& values);

  signals:
    void signal_applied(foundation::Dictionary values);

  private:
    QWidget*                                m_parent;
    const renderer::Project&                m_project;
    std::auto_ptr<IFormFactory>             m_form_factory;
    std::auto_ptr<IEntityBrowser>           m_entity_browser;
    std::auto_ptr<CustomEntityUI>           m_custom_ui;

    QVBoxLayout*                            m_top_layout;
    QFormLayout*                            m_form_layout;
    InputMetadataCollection                 m_input_metadata;
    InputWidgetProxyCollection              m_widget_proxies;

    QSignalMapper*                          m_entity_picker_bind_signal_mapper;
    QSignalMapper*                          m_color_picker_signal_mapper;
    QSignalMapper*                          m_file_picker_signal_mapper;

    void create_form_layout();
    void create_connections();

    const foundation::Dictionary& get_input_metadata(const std::string& name) const;

    // Return whether a widget should be visible, based on the visible_if predicate and the current parameter values.
    bool is_input_widget_visible(
        const foundation::Dictionary&       metadata,
        const foundation::Dictionary&       values) const;

    // Create one or multiple widgets given the definition of one input.
    void create_input_widgets(const foundation::Dictionary& definition, const bool input_widget_visible);

    std::auto_ptr<IInputWidgetProxy> create_text_input_widgets(const foundation::Dictionary& definition, const bool input_widget_visible);
    std::auto_ptr<IInputWidgetProxy> create_numeric_input_widgets(const foundation::Dictionary& definition, const bool input_widget_visible);
    std::auto_ptr<IInputWidgetProxy> create_colormap_input_widgets(const foundation::Dictionary& definition, const bool input_widget_visible);
    std::auto_ptr<IInputWidgetProxy> create_boolean_input_widgets(const foundation::Dictionary& definition, const bool input_widget_visible);
    std::auto_ptr<IInputWidgetProxy> create_enumeration_input_widgets(const foundation::Dictionary& definition, const bool input_widget_visible);
    std::auto_ptr<IInputWidgetProxy> create_entity_input_widgets(const foundation::Dictionary& definition, const bool input_widget_visible);
    std::auto_ptr<IInputWidgetProxy> create_color_input_widgets(const foundation::Dictionary& definition, const bool input_widget_visible);
    std::auto_ptr<IInputWidgetProxy> create_file_input_widgets(const foundation::Dictionary& definition, const bool input_widget_visible);

  private slots:
    void slot_rebuild_form();

    void slot_open_entity_browser(const QString& widget_name);
    void slot_entity_browser_accept(QString widget_name, QString page_name, QString entity_name);

    void slot_open_color_picker(const QString& widget_name);
    void slot_color_changed(const QString& widget_name, const QColor& color);

    void slot_open_file_picker(const QString& widget_name);

    void slot_apply();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITOR_H
