
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYBROWSER_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYBROWSER_H

// appleseed.studio headers.
#include "mainwindow/project/entityeditorwindow.h"

// Standard headers.
#include <string>

// Forward declarations.
namespace foundation    { class StringDictionary; }
namespace renderer      { class Assembly; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class Scene; }

namespace appleseed {
namespace studio {

template <typename ParentEntity> class EntityBrowser;

template <>
class EntityBrowser<renderer::BaseGroup>
  : public EntityEditorWindow::IEntityBrowser
{
  public:
    explicit EntityBrowser(const renderer::BaseGroup& base_group);

    virtual foundation::StringDictionary get_entities(const std::string& type) const;

  private:
    const renderer::BaseGroup& m_base_group;
};

template <>
class EntityBrowser<renderer::Scene>
  : public EntityBrowser<renderer::BaseGroup>
{
  public:
    explicit EntityBrowser(const renderer::Scene& scene);

    virtual foundation::StringDictionary get_entities(const std::string& type) const;

  private:
    const renderer::Scene& m_scene;
};

template <>
class EntityBrowser<renderer::Assembly>
  : public EntityBrowser<renderer::BaseGroup>
{
  public:
    explicit EntityBrowser(const renderer::Assembly& assembly);

    virtual foundation::StringDictionary get_entities(const std::string& type) const;

  private:
    const renderer::Assembly& m_assembly;

    foundation::StringDictionary get_entities(
        const renderer::Assembly&   assembly,
        const std::string&          type) const;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYBROWSER_H
