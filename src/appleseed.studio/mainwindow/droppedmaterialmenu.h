
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

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// appleseed.renderer headers.
#include "renderer/modeling/scene/objectinstance.h"

// Qt headers.
#include <QMenu>
#include <QObject>

namespace appleseed {
namespace studio {

class DroppedMaterialMenu
    : public QMenu
{
    Q_OBJECT

public:
    // constructor
    DroppedMaterialMenu(const foundation::Vector2d& drop_pos, const std::string& material_name);

    // destructor
    ~DroppedMaterialMenu() override = default;

signals:
    void signal_apply_material(
        const foundation::Vector2d& drop_pos,
        const std::string& material_name,
        renderer::ObjectInstance::Side side);

private:
    const foundation::Vector2d& m_drop_pos;
    const std::string& m_material_name;

private slots:
    void slot_apply_to_front();
    void slot_apply_to_back();
    void slot_apply_to_both();

};

}   // namespace studio
}   // namespace appleseed
