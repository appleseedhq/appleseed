
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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
#include "falsecolorswindow.h"

// UI definition header.
#include "ui_falsecolorswindow.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.renderer headers.
#include "renderer/api/postprocessing.h"

// appleseed.foundation headers.
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <utility>

// Qt headers.
#include <QDialogButtonBox>
#include <QGroupBox>
#include <QShortcut>
#include <Qt>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

namespace
{
    class PostProcessingStageFormFactory
      : public EntityEditor::IFormFactory
    {
      public:
        explicit PostProcessingStageFormFactory(
            const DictionaryArray&                  input_metadata)
          : m_input_metadata(input_metadata)
        {
        }

        void update(
            const Dictionary&                       values,
            EntityEditor::InputMetadataCollection&  metadata) const override
        {
            metadata.clear();

            // Note that we don't expose name or model inputs.

            for (size_t i = 0; i < m_input_metadata.size(); ++i)
            {
                Dictionary im = m_input_metadata[i];
                const std::string input_name = im.get<std::string>("name");

                // Don't expose the order input either.
                if (input_name == "order")
                    continue;

                im.insert("value",
                    values.strings().exist(input_name.c_str()) ? values.get<std::string>(input_name.c_str()) :
                    im.strings().exist("default") ? im.get<std::string>("default") :
                    "");

                metadata.push_back(im);
            }
        }

      private:
        const DictionaryArray m_input_metadata;
    };
}

FalseColorsWindow::FalseColorsWindow(QWidget* parent)
  : WindowBase(parent, "false_colors_window")
  , m_ui(new Ui::FalseColorsWindow())
{
    m_ui->setupUi(this);

    setWindowFlags(Qt::Window);

    create_connections();

    WindowBase::load_settings();
}

FalseColorsWindow::~FalseColorsWindow()
{
    delete m_ui;
}

void FalseColorsWindow::initialize(
    const Project&      project,
    ParamArray&         settings,
    const Dictionary&   values)
{
    m_initial_values = values;

    std::unique_ptr<EntityEditor::IFormFactory> form_factory(
        new PostProcessingStageFormFactory(
            ColorMapPostProcessingStageFactory().get_input_metadata()));

    m_entity_editor.reset(
        new EntityEditor(
            m_ui->groupbox_false_colors,
            project,
            settings,
            std::move(form_factory),
            nullptr,    // no entity browser
            nullptr,    // no custom UI
            values));

    connect(
        m_entity_editor.get(), SIGNAL(signal_applied(foundation::Dictionary)),
        SLOT(slot_apply()));

    const bool enabled =
        values.strings().exist("enabled") &&
        values.strings().get<bool>("enabled");

    const bool were_blocked = m_ui->groupbox_false_colors->blockSignals(true);
    m_ui->groupbox_false_colors->setChecked(enabled);
    m_ui->groupbox_false_colors->blockSignals(were_blocked);
}

void FalseColorsWindow::create_connections()
{
    connect(
        m_ui->groupbox_false_colors, SIGNAL(toggled(bool)),
        SLOT(slot_apply()));

    connect(m_ui->buttonbox, SIGNAL(accepted()), SLOT(slot_accept()));
    connect(m_ui->buttonbox, SIGNAL(rejected()), SLOT(slot_cancel()));

    connect(
        create_window_local_shortcut(this, QKeySequence("Ctrl+Return")), SIGNAL(activated()),
        SLOT(slot_accept()));

    connect(
        create_window_local_shortcut(this, QKeySequence("Ctrl+Enter")), SIGNAL(activated()),
        SLOT(slot_accept()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Escape), SIGNAL(activated()),
        SLOT(slot_cancel()));
}

Dictionary FalseColorsWindow::get_values() const
{
    Dictionary values = m_entity_editor->get_values();
    values.insert("enabled", m_ui->groupbox_false_colors->isChecked());
    values.insert("order", 0);    // actual value irrelevant
    return values;
}

void FalseColorsWindow::slot_apply()
{
    emit signal_applied(get_values());
}

void FalseColorsWindow::slot_accept()
{
    emit signal_accepted(get_values());

    close();
}

void FalseColorsWindow::slot_cancel()
{
    if (m_initial_values != get_values())
        emit signal_canceled(m_initial_values);

    close();
}

}   // namespace studio
}   // namespace appleseed
