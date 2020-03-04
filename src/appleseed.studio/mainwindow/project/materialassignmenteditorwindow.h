
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

#pragma once

// appleseed.studio headers.
#include "utility/windowbase.h"

// appleseed.renderer headers.
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"

// Qt headers.
#include <QComboBox>
#include <QLineEdit>
#include <QObject>

// Standard headers.
#include <map>
#include <string>

// Forward declarations.
namespace appleseed { namespace studio { class EntityEditorContext; } }
namespace appleseed { namespace studio { class ObjectInstanceItem; } }
namespace renderer  { class Object; }
namespace Ui        { class MaterialAssignmentEditorWindow; }
class QGridLayout;
class QPushButton;
class QString;
class QVBoxLayout;
class QWidget;

namespace appleseed {
namespace studio {

class MaterialAssignmentEditorWindow
  : public WindowBase
{
    Q_OBJECT

  public:
    // Constructor.
    MaterialAssignmentEditorWindow(
        renderer::ObjectInstance&       object_instance,
        ObjectInstanceItem&             object_istance_item,
        EntityEditorContext&            editor_context,
        QWidget*                        parent = nullptr);

    // Destructor.
    ~MaterialAssignmentEditorWindow() override;

  signals:
    void signal_accepted(foundation::Dictionary values);

  private:
    class AssignMaterialsAction;

    // Not wrapped in std::unique_ptr<> to avoid pulling in the UI definition code.
    Ui::MaterialAssignmentEditorWindow* m_ui;

    renderer::ObjectInstance&           m_object_instance;
    ObjectInstanceItem&                 m_object_instance_item;
    renderer::Object*                   m_object;
    EntityEditorContext&                m_editor_context;

    typedef renderer::ObjectInstance::Side Side;

    struct SlotInfo
    {
        std::string     m_slot_name;
        Side            m_side;
        QComboBox*      m_combo_box;
        QLineEdit*      m_line_edit;

        QString get_mode() const
        {
            return m_combo_box->itemData(m_combo_box->currentIndex()).toString();
        }

        QString get_material_name() const
        {
            return m_line_edit->text();
        }
    };

    typedef std::map<QPushButton*, SlotInfo> SlotInfoCollection;

    struct SlotValue
    {
        std::string     m_slot_name;
        Side            m_side;
        std::string     m_material_name;
    };

    typedef std::vector<SlotValue> SlotValueCollection;

    SlotInfoCollection                  m_slot_infos;
    SlotValueCollection                 m_initial_slot_values;

    std::map<QComboBox*, QWidget*>      m_mode_combo_to_widget_group;

    void create_widgets();

    void create_widgets_for_slot(
        QVBoxLayout*    parent,
        const char*     slot_name);

    void create_widgets_for_side(
        QGridLayout*    parent,
        const int       row_index,
        const char*     slot_name,
        const Side      side);

    static void append_row(
        QVBoxLayout*    parent,
        QLayout*        row_layout);

    SlotValueCollection get_slot_values() const;
    SlotValue get_slot_value(const SlotInfo& slot_info) const;

    void assign_materials(const SlotValueCollection& slot_values);

  private slots:
    void slot_change_back_material_mode(int index);
    void slot_open_entity_browser();
    void slot_entity_browser_accept(QLineEdit* line_edit, QString page_name, QString entity_name);

    void slot_apply();
    void slot_accept();
    void slot_cancel();
};

}   // namespace studio
}   // namespace appleseed
