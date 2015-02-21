
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Marius Avram, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALCUSTOMUI_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALCUSTOMUI_H

// appleseed.studio headers.
#include "mainwindow/project/customentityui.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

// Qt headers.
#include <QObject>

// Forward declarations.
namespace appleseed     { namespace studio { class DisneyMaterialLayerUI; } }
namespace foundation    { class Dictionary; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
class QPushButton;
class QVBoxLayout;
class QWidget;

namespace appleseed {
namespace studio {

class DisneyMaterialCustomUI
  : public CustomEntityUI
{
    Q_OBJECT

  public:
    DisneyMaterialCustomUI(
        const renderer::Project&        project,
        renderer::ParamArray&           settings);

    virtual void create_widgets(
        QVBoxLayout*                    layout,
        const foundation::Dictionary&   values) APPLESEED_OVERRIDE;

    virtual foundation::Dictionary get_values() const APPLESEED_OVERRIDE;

  private slots:
    void slot_add_layer();
    void slot_move_layer_up(QWidget* layer_widget);
    void slot_move_layer_down(QWidget* layer_widget);
    void slot_delete_layer(QWidget* layer_widget);

  private:
    const renderer::Project&            m_project;
    renderer::ParamArray&               m_settings;
    QWidget*                            m_parent;
    QVBoxLayout*                        m_layout;

    std::vector<DisneyMaterialLayerUI*> m_layers;
    QPushButton*                        m_add_layer_button;

    size_t find_layer_index_by_widget(const QWidget* layer_widget) const;
    std::vector<std::string> collect_layer_names() const;
    foundation::Dictionary make_new_layer_values();
    void append_new_layer(const foundation::Dictionary& layer_values);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALCUSTOMUI_H
