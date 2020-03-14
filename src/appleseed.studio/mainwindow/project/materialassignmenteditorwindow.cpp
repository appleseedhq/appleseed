
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
#include "materialassignmenteditorwindow.h"

// UI definition header.
#include "ui_materialassignmenteditorwindow.h"

// appleseed.studio headers.
#include "mainwindow/project/entitybrowser.h"
#include "mainwindow/project/entitybrowserwindow.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/objectinstanceitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/rendering/renderingmanager.h"

// appleseed.qtcommon headers.
#include "utility/interop.h"
#include "utility/miscellaneous.h"
#include "widgets/mousewheelfocuseventfilter.h"

// appleseed.renderer headers.
#include "renderer/api/object.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// Qt headers.
#include <QFrame>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QShortcut>
#include <QSizePolicy>
#include <QString>
#include <Qt>
#include <QVBoxLayout>
#include <QWidget>

// Standard headers.
#include <cstddef>
#include <memory>
#include <set>
#include <sstream>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

MaterialAssignmentEditorWindow::MaterialAssignmentEditorWindow(
    ObjectInstance&         object_instance,
    ObjectInstanceItem&     object_instance_item,
    EntityEditorContext&    editor_context,
    QWidget*                parent)
  : WindowBase(parent, "material_assignment_editor_window")
  , m_ui(new Ui::MaterialAssignmentEditorWindow())
  , m_object_instance(object_instance)
  , m_object_instance_item(object_instance_item)
  , m_object(m_object_instance.find_object())
  , m_editor_context(editor_context)
{
    m_ui->setupUi(this);

    setAttribute(Qt::WA_DeleteOnClose);
    setWindowFlags(Qt::Tool);
    setWindowModality(Qt::NonModal);

    resize(600, 400);

    m_ui->scrollarea->setProperty("hasFrame", true);

    create_widgets();

    m_initial_slot_values = get_slot_values();

    connect(m_ui->buttonbox, SIGNAL(accepted()), SLOT(slot_accept()));
    connect(m_ui->buttonbox, SIGNAL(rejected()), SLOT(slot_cancel()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Escape), SIGNAL(activated()),
        SLOT(slot_cancel()));

    WindowBase::load_settings();
}

MaterialAssignmentEditorWindow::~MaterialAssignmentEditorWindow()
{
    delete m_ui;
}

namespace
{
    std::set<std::string> get_slot_names_from_material_mappings(const ObjectInstance& object_instance)
    {
        std::set<std::string> slot_names;

        for (const_each<StringDictionary> i = object_instance.get_front_material_mappings(); i; ++i)
            slot_names.insert(i->key());

        for (const_each<StringDictionary> i = object_instance.get_back_material_mappings(); i; ++i)
            slot_names.insert(i->key());

        return slot_names;
    }
}

void MaterialAssignmentEditorWindow::create_widgets()
{
    QVBoxLayout* layout = new QVBoxLayout(m_ui->scrollarea_contents);
    layout->setAlignment(Qt::AlignTop);
    layout->setSpacing(20);

    if (m_object == nullptr)
    {
        QHBoxLayout* row_layout = new QHBoxLayout();
        row_layout->addWidget(new QLabel(QString("Object \"%1\" not found.").arg(m_object_instance.get_object_name())));
        append_row(layout, row_layout);
        return;
    }

    if (m_object->get_material_slot_count() > 0)
    {
        for (size_t i = 0; i < m_object->get_material_slot_count(); ++i)
            create_widgets_for_slot(layout, m_object->get_material_slot(i));
        return;
    }

    const std::set<std::string> slot_names = get_slot_names_from_material_mappings(m_object_instance);

    if (!slot_names.empty())
    {
        for (const_each<std::set<std::string>> i = slot_names; i; ++i)
            create_widgets_for_slot(layout, i->c_str());
        return;
    }

    create_widgets_for_slot(layout, ObjectInstanceItem::DefaultSlotName);
}

void MaterialAssignmentEditorWindow::create_widgets_for_slot(
    QVBoxLayout*        parent,
    const char*         slot_name)
{
    QGridLayout* row_layout = new QGridLayout();
    row_layout->setSpacing(10);

    row_layout->addWidget(new QLabel("Slot:"), 0, 0, Qt::AlignRight);
    row_layout->addWidget(new QLabel(QString("<b>%1</b>").arg(slot_name)), 0, 1);

    create_widgets_for_side(row_layout, 1, slot_name, ObjectInstance::FrontSide);
    create_widgets_for_side(row_layout, 2, slot_name, ObjectInstance::BackSide);

    append_row(parent, row_layout);
}

void MaterialAssignmentEditorWindow::create_widgets_for_side(
    QGridLayout*        parent,
    const int           row_index,
    const char*         slot_name,
    const Side          side)
{
    parent->addWidget(new QLabel(side == ObjectInstance::FrontSide ? "Front:" : "Back:"), row_index, 0, Qt::AlignRight);

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);

    QWidget* group = new QWidget();
    group->setLayout(new QHBoxLayout());
    group->layout()->setContentsMargins(0, 0, 0, 0);

    QComboBox* combo_box = nullptr;

    if (side == ObjectInstance::BackSide)
    {
        combo_box = new QComboBox();
        combo_box->addItem("Same As Front", "same");
        combo_box->addItem("None", "none");
        combo_box->addItem("Enabled", "enabled");
        combo_box->setCurrentIndex(-1);
        layout->addWidget(combo_box);

        new MouseWheelFocusEventFilter(combo_box);

        m_mode_combo_to_widget_group[combo_box] = group;

        const StringDictionary& front_mappings = m_object_instance.get_front_material_mappings();
        const StringDictionary& back_mappings = m_object_instance.get_back_material_mappings();

        if (!back_mappings.exist(slot_name))
        {
            combo_box->setCurrentIndex(combo_box->findData("none"));
            group->setEnabled(false);
        }
        else if (front_mappings.exist(slot_name) &&
                 front_mappings.get<std::string>(slot_name) == back_mappings.get<std::string>(slot_name))
        {
            combo_box->setCurrentIndex(combo_box->findData("same"));
            group->setEnabled(false);
        }
        else
        {
            combo_box->setCurrentIndex(combo_box->findData("enabled"));
            group->setEnabled(true);
        }

        connect(
            combo_box, SIGNAL(currentIndexChanged(int)),
            SLOT(slot_change_back_material_mode(int)));
    }

    QLineEdit* line_edit = new QLineEdit();
    group->layout()->addWidget(line_edit);

    const StringDictionary& mappings =
        side == ObjectInstance::FrontSide
            ? m_object_instance.get_front_material_mappings()
            : m_object_instance.get_back_material_mappings();

    if (m_object->get_material_slot_count() > 0)
    {
        if (mappings.exist(slot_name))
            line_edit->setText(mappings.get<QString>(slot_name));
    }
    else if (!mappings.empty())
    {
        line_edit->setText(mappings.begin().value<QString>());
    }

    connect(line_edit, SIGNAL(returnPressed()), SLOT(slot_apply()));

    QPushButton* browse_button = new QPushButton("Browse");
    browse_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    group->layout()->addWidget(browse_button);

    SlotInfo& slot_info = m_slot_infos[browse_button];
    slot_info.m_slot_name = slot_name;
    slot_info.m_side = side;
    slot_info.m_combo_box = combo_box;
    slot_info.m_line_edit = line_edit;

    connect(
        browse_button, SIGNAL(clicked()),
        SLOT(slot_open_entity_browser()));

    layout->addWidget(group);
    parent->addLayout(layout, row_index, 1);
}

void MaterialAssignmentEditorWindow::append_row(
    QVBoxLayout*        parent,
    QLayout*            row_layout)
{
    QFrame* row = new QFrame();
    row->setProperty("hasFrame", true);
    row->setLayout(row_layout);
    parent->addWidget(row);
}

MaterialAssignmentEditorWindow::SlotValueCollection MaterialAssignmentEditorWindow::get_slot_values() const
{
    SlotValueCollection slot_values;

    for (const_each<SlotInfoCollection> i = m_slot_infos; i; ++i)
        slot_values.push_back(get_slot_value(i->second));

    return slot_values;
}

MaterialAssignmentEditorWindow::SlotValue MaterialAssignmentEditorWindow::get_slot_value(const SlotInfo& slot_info) const
{
    SlotValue slot_value;
    slot_value.m_slot_name = slot_info.m_slot_name;
    slot_value.m_side = slot_info.m_side;

    if (slot_info.m_side == ObjectInstance::FrontSide)
        slot_value.m_material_name = slot_info.get_material_name().toStdString();
    else if (slot_info.get_mode() == "enabled")
        slot_value.m_material_name = slot_info.get_material_name().toStdString();
    else if (slot_info.get_mode() == "same")
    {
        const StringDictionary& front_mappings = m_object_instance.get_front_material_mappings();
        if (front_mappings.exist(slot_info.m_slot_name.c_str()))
            slot_value.m_material_name = front_mappings.get<std::string>(slot_info.m_slot_name.c_str());
    }

    return slot_value;
}

class MaterialAssignmentEditorWindow::AssignMaterialsAction
  : public RenderingManager::IScheduledAction
{
  public:
    AssignMaterialsAction(
        ObjectInstance&             object_instance,
        ObjectInstanceItem&         object_instance_item,
        const SlotValueCollection&  slot_values)
      : m_object_instance(object_instance)
      , m_object_instance_item(object_instance_item)
      , m_slot_values(slot_values)
    {
    }

    void operator()(
        Project&                    project) override
    {
        for (const_each<SlotValueCollection> i = m_slot_values; i; ++i)
        {
            i->m_material_name.empty()
                ? m_object_instance.unassign_material(i->m_slot_name.c_str(), i->m_side)
                : m_object_instance.assign_material(i->m_slot_name.c_str(), i->m_side, i->m_material_name.c_str());
        }

        m_object_instance_item.update_style();
    }

  private:
    ObjectInstance&                 m_object_instance;
    ObjectInstanceItem&             m_object_instance_item;
    const SlotValueCollection       m_slot_values;
};

void MaterialAssignmentEditorWindow::assign_materials(const SlotValueCollection& slot_values)
{
    const StringDictionary old_front_mappings = m_object_instance.get_front_material_mappings();
    const StringDictionary old_back_mappings = m_object_instance.get_back_material_mappings();

    m_editor_context.m_rendering_manager.schedule_or_execute(
        std::unique_ptr<RenderingManager::IScheduledAction>(
            new AssignMaterialsAction(
                m_object_instance,
                m_object_instance_item,
                slot_values)));

    if (old_front_mappings != m_object_instance.get_front_material_mappings() ||
        old_back_mappings != m_object_instance.get_back_material_mappings())
        m_editor_context.m_project_builder.slot_notify_project_modification();
}

void MaterialAssignmentEditorWindow::slot_change_back_material_mode(int index)
{
    QComboBox* combo_box = qobject_cast<QComboBox*>(QObject::sender());
    const QString mode = combo_box->itemData(index).value<QString>();

    QWidget* group = m_mode_combo_to_widget_group[combo_box];
    group->setEnabled(mode == "enabled");

    slot_apply();
}

namespace
{
    class ForwardAcceptedSignal
      : public QObject
    {
        Q_OBJECT

      public:
        ForwardAcceptedSignal(QObject* parent, QLineEdit* line_edit)
          : QObject(parent)
          , m_line_edit(line_edit)
        {
        }

      public slots:
        void slot_accept(QString page_name, QString entity_name)
        {
            emit signal_accepted(m_line_edit, page_name, entity_name);
        }

      signals:
        void signal_accepted(QLineEdit* line_edit, QString page_name, QString entity_name);

      private:
        QLineEdit* m_line_edit;
    };
}

void MaterialAssignmentEditorWindow::slot_open_entity_browser()
{
    QPushButton* browse_button = qobject_cast<QPushButton*>(QObject::sender());
    const SlotInfo& slot_info = m_slot_infos[browse_button];

    EntityBrowser<Assembly> entity_browser(
        *static_cast<const Assembly*>(m_object_instance.get_parent()));

    std::stringstream window_title;
    window_title << slot_info.m_slot_name;
    window_title << " (" << (slot_info.m_side == ObjectInstance::FrontSide ? "Front" : "Back") << ")";

    EntityBrowserWindow* browser_window = new EntityBrowserWindow(this, window_title.str());
    browser_window->add_items_page("material", "Materials", entity_browser.get_entities("material"));

    ForwardAcceptedSignal* forward_signal =
        new ForwardAcceptedSignal(browser_window, slot_info.m_line_edit);

    connect(
        browser_window, SIGNAL(signal_accepted(QString, QString)),
        forward_signal, SLOT(slot_accept(QString, QString)));

    connect(
        forward_signal, SIGNAL(signal_accepted(QLineEdit*, QString, QString)),
        SLOT(slot_entity_browser_accept(QLineEdit*, QString, QString)));

    browser_window->showNormal();
    browser_window->activateWindow();
}

void MaterialAssignmentEditorWindow::slot_entity_browser_accept(
    QLineEdit*          line_edit,
    QString             page_name,
    QString             entity_name)
{
    line_edit->setText(entity_name);

    assign_materials(get_slot_values());

    qobject_cast<QWidget*>(sender()->parent())->close();
}

void MaterialAssignmentEditorWindow::slot_apply()
{
    assign_materials(get_slot_values());
}

void MaterialAssignmentEditorWindow::slot_accept()
{
    assign_materials(get_slot_values());

    close();
}

void MaterialAssignmentEditorWindow::slot_cancel()
{
    assign_materials(m_initial_slot_values);

    close();
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/project/moc_cpp_materialassignmenteditorwindow.cxx"
