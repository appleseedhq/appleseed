
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

// Interface header.
#include "entitycreatorbase.h"

// appleseed.studio headers.
#include "utility/tweaks.h"

// appleseed.foundation headers.
#include "foundation/utility/string.h"

// Qt headers.
#include <QMessageBox>
#include <QString>

using namespace foundation;
using namespace std;

namespace appleseed {
namespace studio {

bool EntityCreatorBase::contains_valid_name(const Dictionary& values)
{
    return
        values.strings().exist("name") &&
        is_valid_name(values.get<string>("name"));
}

bool EntityCreatorBase::is_valid_name(const string& name)
{
    const string trimmed_name = trim_both(name);
    return !trimmed_name.empty();
}

void EntityCreatorBase::display_entity_creation_error(
    const QString&  entity_name,
    const QString&  message)
{
    QMessageBox msgbox;
    msgbox.setWindowTitle(QString("Failed to create %1").arg(entity_name));
    msgbox.setIcon(QMessageBox::Warning);
    msgbox.setText(message);
    msgbox.setStandardButtons(QMessageBox::Ok);
    msgbox.setDefaultButton(QMessageBox::Ok);
    set_minimum_width(msgbox, 300);
    msgbox.exec();
}

}   // namespace studio
}   // namespace appleseed
