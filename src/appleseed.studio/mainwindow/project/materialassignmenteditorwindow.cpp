
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "mainwindow/project/projectbuilder.h"
#include "utility/interop.h"
#include "utility/spinboxeventfilter.h"
#include "utility/tweaks.h"

// appleseed.renderer headers.
#include "renderer/api/object.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// Qt headers.
#include <QFrame>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QShortcut>
#include <QSizePolicy>
#include <QString>
#include <Qt>
#include <QVBoxLayout>

// Standard headers.
#include <cstddef>
#include <set>
#include <sstream>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

MaterialAssignmentEditorWindow::MaterialAssignmentEditorWindow(
    QWidget*            parent,
    ObjectInstance&     object_instance,
    ProjectBuilder&     project_builder)
  : QWidget(parent)
  , m_ui(new Ui::MaterialAssignmentEditorWindow())
  , m_object_instance(object_instance)
  , m_object(m_object_instance.find_object())
  , m_project_builder(project_builder)
{
    m_ui->setupUi(this);

    setAttribute(Qt::WA_DeleteOnClose);
    setWindowFlags(Qt::Tool);

    resize(600, 400);

    m_ui->scrollarea->setProperty("hasFrame", true);

    create_widgets();

    connect(m_ui->buttonbox, SIGNAL(accepted()), this, SLOT(slot_accept()));
    connect(m_ui->buttonbox, SIGNAL(rejected()), this, SLOT(close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Return), SIGNAL(activated()),
        this, SLOT(slot_accept()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Enter), SIGNAL(activated()),
        this, SLOT(slot_accept()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Escape), SIGNAL(activated()),
        this, SLOT(close()));
}

MaterialAssignmentEditorWindow::~MaterialAssignmentEditorWindow()
{
    delete m_ui;
}

void MaterialAssignmentEditorWindow::create_widgets()
{
    QVBoxLayout* layout = new QVBoxLayout(m_ui->scrollarea_contents);
    layout->setAlignment(Qt::AlignTop);
    layout->setSpacing(20);

    if (m_object)
    {
        if (m_object->get_material_slot_count() > 0)
        {
            for (size_t i = 0; i < m_object->get_material_slot_count(); ++i)
                create_widgets_for_slot(layout, m_object->get_material_slot(i));
        }
        else
        {
            set<string> slot_names;

            for (const_each<StringDictionary> i = m_object_instance.get_front_material_mappings(); i; ++i)
                slot_names.insert(i->name());

            for (const_each<StringDictionary> i = m_object_instance.get_back_material_mappings(); i; ++i)
                slot_names.insert(i->name());

            for (const_each<set<string> > i = slot_names; i; ++i)
                create_widgets_for_slot(layout, i->c_str());
        }
    }
    else
    {
        QHBoxLayout* row_layout = new QHBoxLayout();
        row_layout->addWidget(new QLabel(QString("Object \"%1\" not found.").arg(m_object_instance.get_object_name())));
        append_row(layout, row_layout);
    }
}

void MaterialAssignmentEditorWindow::create_widgets_for_slot(
    QVBoxLayout*        parent,
    const char*         slot_name)
{
    QGridLayout* row_layout = new QGridLayout();
    row_layout->setHorizontalSpacing(20);
    row_layout->setVerticalSpacing(10);

    row_layout->addWidget(new QLabel("Slot:"), 0, 0);
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
    parent->addWidget(new QLabel(side == ObjectInstance::FrontSide ? "Front:" : "Back:"), row_index, 0);

    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(6);

    QWidget* group = new QWidget();
    group->setLayout(new QHBoxLayout());
    group->layout()->setContentsMargins(0, 0, 0, 0);

    QComboBox* combo_box = 0;

    if (side == ObjectInstance::BackSide)
    {
        combo_box = new QComboBox();
        combo_box->addItem("Same As Front", "same");
        combo_box->addItem("None", "none");
        combo_box->addItem("Enabled", "enabled");
        combo_box->setCurrentIndex(-1);
        layout->addWidget(combo_box);

        new SpinBoxEventFilter(combo_box);

        m_model_combo_to_widget_group[combo_box] = group;

        connect(
            combo_box, SIGNAL(currentIndexChanged(int)),
            this, SLOT(slot_change_back_material_mode(int)));

        const StringDictionary& front_mappings = m_object_instance.get_front_material_mappings();
        const StringDictionary& back_mappings = m_object_instance.get_back_material_mappings();

        if (!back_mappings.exist(slot_name))
            combo_box->setCurrentIndex(combo_box->findData("none"));
        else if (front_mappings.exist(slot_name) &&
                 front_mappings.get<string>(slot_name) == back_mappings.get<string>(slot_name))
            combo_box->setCurrentIndex(combo_box->findData("same"));
        else combo_box->setCurrentIndex(combo_box->findData("enabled"));
    }

    QLineEdit* line_edit = new QLineEdit();
    group->layout()->addWidget(line_edit);

    const StringDictionary& mappings =
        side == ObjectInstance::FrontSide
            ? m_object_instance.get_front_material_mappings()
            : m_object_instance.get_back_material_mappings();

    if (m_object->get_material_slot_count() > 1)
    {
        if (mappings.exist(slot_name))
            line_edit->setText(mappings.get<QString>(slot_name));
    }
    else if (!mappings.empty())
    {
        line_edit->setText(mappings.begin().value<QString>());
    }

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
        this, SLOT(slot_open_entity_browser()));

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

void MaterialAssignmentEditorWindow::assign_materials()
{
    for (const_each<SlotInfoCollection> i = m_slot_infos; i; ++i)
    {
        const SlotInfo& slot_info = i->second;

        if (slot_info.m_side == ObjectInstance::FrontSide)
            assign_material(slot_info);
        else
        {
            const QString mode = slot_info.get_mode();

            if (mode == "enabled")
                assign_material(slot_info);
            else if (mode == "none")
                assign_material(slot_info, QString());
            else
            {
                assert(mode == "same");

                const StringDictionary& front_mappings = m_object_instance.get_front_material_mappings();
                const QString material_name =
                    front_mappings.exist(slot_info.m_slot_name)
                        ? front_mappings.get<QString>(slot_info.m_slot_name)
                        : QString();

                assign_material(slot_info, material_name);
            }
        }
    }
}

void MaterialAssignmentEditorWindow::assign_material(const SlotInfo& slot_info)
{
    assign_material(slot_info, slot_info.m_line_edit->text());
}

void MaterialAssignmentEditorWindow::assign_material(
    const SlotInfo&     slot_info,
    const QString&      material_name)
{
    if (material_name.isEmpty())
    {
        m_object_instance.unassign_material(
            slot_info.m_slot_name.c_str(),
            slot_info.m_side);
    }
    else
    {
        m_object_instance.assign_material(
            slot_info.m_slot_name.c_str(),
            slot_info.m_side,
            material_name.toAscii().constData());
    }
}

void MaterialAssignmentEditorWindow::slot_change_back_material_mode(int index)
{
    QComboBox* combo_box = qobject_cast<QComboBox*>(QObject::sender());
    const QString mode = combo_box->itemData(index).value<QString>();

    QWidget* group = m_model_combo_to_widget_group[combo_box];
    group->setEnabled(mode == "enabled");
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

    stringstream window_title;
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
        this, SLOT(slot_entity_browser_accept(QLineEdit*, QString, QString)));

    browser_window->showNormal();
    browser_window->activateWindow();
}

void MaterialAssignmentEditorWindow::slot_entity_browser_accept(
    QLineEdit*          line_edit,
    QString             page_name,
    QString             entity_name)
{
    line_edit->setText(entity_name);

    qobject_cast<QWidget*>(sender()->parent())->close();
}

void MaterialAssignmentEditorWindow::slot_accept()
{
    const StringDictionary old_front_mappings = m_object_instance.get_front_material_mappings();
    const StringDictionary old_back_mappings = m_object_instance.get_back_material_mappings();

    assign_materials();

    if (old_front_mappings != m_object_instance.get_front_material_mappings() ||
        old_back_mappings != m_object_instance.get_back_material_mappings())
        m_project_builder.notify_project_modification();

    close();
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/project/moc_cpp_materialassignmenteditorwindow.cxx"
