
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

// appleseed.studio headers.
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
#include <QShortcut>
#include <Qt>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

namespace
{
    struct FalseColorsPostProcessingStageFormFactory
      : public EntityEditor::IFormFactory
    {
        void update(
            const Dictionary&                       values,
            EntityEditor::InputMetadataCollection&  metadata) const override
        {
            metadata.clear();

            const DictionaryArray input_metadata =
                ColorMapPostProcessingStageFactory().get_input_metadata();

            // Note that we don't expose name or model inputs.

            for (size_t i = 0; i < input_metadata.size(); ++i)
            {
                Dictionary im = input_metadata[i];
                const string input_name = im.get<string>("name");

                // Don't expose the order input either. 
                if (input_name == "order")
                    continue;

                im.insert("value",
                    values.strings().exist(input_name) ? values.get<string>(input_name) :
                    im.strings().exist("default") ? im.get<string>("default") :
                    "");

                metadata.push_back(im);
            }
        }
    };
}

FalseColorsWindow::FalseColorsWindow(QWidget* parent)
  : QWidget(parent)
  , m_ui(new Ui::FalseColorsWindow())
{
    m_ui->setupUi(this);

    setWindowFlags(Qt::Window);

    create_connections();
}

FalseColorsWindow::~FalseColorsWindow()
{
    delete m_ui;
}

void FalseColorsWindow::initialize(
    const Project&                              project,
    ParamArray&                                 settings,
    const Dictionary&                           values)
{
    unique_ptr<EntityEditor::IFormFactory> form_factory(
        new FalseColorsPostProcessingStageFormFactory());

    m_initial_values = values;

    m_entity_editor.reset(
        new EntityEditor(
            m_ui->scrollarea_contents,
            project,
            settings,
            std::move(form_factory),
            nullptr,    // no entity browser
            nullptr,    // no custom UI
            values));

    connect(
        m_entity_editor.get(), SIGNAL(signal_applied(foundation::Dictionary)),
        SIGNAL(signal_applied(foundation::Dictionary)));

    const bool enabled =
        values.strings().exist("enabled") &&
        values.strings().get<bool>("enabled");

    m_ui->checkbox_enable_false_colors->setChecked(enabled);
    m_ui->scrollarea->setEnabled(enabled);
}

void FalseColorsWindow::create_connections()
{
    connect(
        m_ui->checkbox_enable_false_colors, SIGNAL(stateChanged(int)),
        SLOT(slot_toggle_enable_false_colors()));

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

void FalseColorsWindow::slot_toggle_enable_false_colors()
{
    const bool enabled = m_ui->checkbox_enable_false_colors->checkState() == Qt::Checked;

    m_ui->scrollarea->setEnabled(enabled);

    emit signal_set_enabled(enabled);;
}

void FalseColorsWindow::slot_accept()
{
    emit signal_accepted(m_entity_editor->get_values());

    close();
}

void FalseColorsWindow::slot_cancel()
{
    if (m_initial_values != m_entity_editor->get_values())
        emit signal_canceled(m_initial_values);

    close();
}

}   // namespace studio
}   // namespace appleseed
