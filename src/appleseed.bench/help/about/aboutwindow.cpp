
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
#include "aboutwindow.h"

// UI definition header.
#include "ui_aboutwindow.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/core/thirdparties.h"
#include "foundation/platform/compiler.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/apistringpair.h"

// Qt headers.
#include <QDialogButtonBox>
#include <QKeySequence>
#include <QShortcut>
#include <QString>
#include <Qt>
#include <QtGlobal>

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <vector>

using namespace foundation;

namespace appleseed {
namespace bench {

AboutWindow::AboutWindow(QWidget* parent)
  : WindowBase(parent, "about_window")
  , m_ui(new Ui::AboutWindow())
{
    m_ui->setupUi(this);

    setWindowFlags(Qt::Window);
    setAttribute(Qt::WA_DeleteOnClose);

    set_library_version();
    set_library_features();
    set_third_party_libraries_information();

    setFixedSize(width(), sizeHint().height());

    connect(m_ui->dialogbuttonbox, &QDialogButtonBox::accepted, this, &QWidget::close);

    connect(
        new QShortcut(QKeySequence(Qt::Key_Escape), this), &QShortcut::activated,
        this, &QWidget::close);

    WindowBase::load_settings();
}

AboutWindow::~AboutWindow()
{
    delete m_ui;
}

void AboutWindow::set_library_version()
{
    m_ui->label_version->setText(
        QString(
            "<p>Using <b>%1 version %2</b>, %3 configuration</p>"
            "<p>Compiled on %4 at %5 using %6 version %7</p>")
            .arg(Appleseed::get_lib_name())
            .arg(Appleseed::get_lib_version())
            .arg(Appleseed::get_lib_configuration())
            .arg(Appleseed::get_lib_compilation_date())
            .arg(Appleseed::get_lib_compilation_time())
            .arg(Compiler::get_compiler_name())
            .arg(Compiler::get_compiler_version()));
}

namespace
{
    const char* to_enabled_disabled(const bool value)
    {
        return value ? "enabled" : "disabled";
    }
}

void AboutWindow::set_library_features()
{
    const bool WithEmbree =
#ifdef APPLESEED_WITH_EMBREE
        true;
#else
        false;
#endif

    const bool WithSpectralSupport =
#ifdef APPLESEED_WITH_SPECTRAL_SUPPORT
        true;
#else
        false;
#endif

    const bool WithGPUSupport =
#ifdef APPLESEED_WITH_GPU
        true;
#else
        false;
#endif

    QString details;
    details += "This build of appleseed has the following features:\n\n";
    details += QString("  Instruction sets: %1\n").arg(Appleseed::get_lib_cpu_features());
    details += QString("  Embree: %1\n").arg(to_enabled_disabled(WithEmbree));
    details += QString("  Spectral support: %1\n").arg(to_enabled_disabled(WithSpectralSupport));
    details += QString("  GPU support: %1\n").arg(to_enabled_disabled(WithGPUSupport));
    details += "\n";
    m_ui->label_details->setText(m_ui->label_details->text() + details);
}

void AboutWindow::set_third_party_libraries_information()
{
    LibraryVersionArray versions = ThirdParties::get_versions();
    versions.push_back(APIStringPair("Qt", QT_VERSION_STR));

    // Create a vector of indices into `versions` to allow enumerating libraries in sorted order.
    std::vector<size_t> versions_indices(versions.size());
    for (size_t i = 0, e = versions.size(); i < e; ++i)
        versions_indices[i] = i;
    std::sort(
        versions_indices.begin(),
        versions_indices.end(),
        [&versions](const size_t lhs, const size_t rhs)
        {
            return strcmp_nocase(versions[lhs].m_first.c_str(), versions[rhs].m_first.c_str()) < 0;
        });

    QString details;
    details += "This build of appleseed uses the following third party libraries:\n\n";

    for (size_t i = 0, e = versions_indices.size(); i < e; ++i)
    {
        const APIStringPair& version = versions[versions_indices[i]];
        details +=
            QString("  %1 %2\n")
                .arg(version.m_first.c_str())
                .arg(version.m_second.c_str());
    }

    m_ui->label_details->setText(m_ui->label_details->text() + details);
}

}   // namespace bench
}   // namespace appleseed
