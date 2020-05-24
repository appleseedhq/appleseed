
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
#include "projectbuilder.h"

// appleseed.studio headers.
#include "mainwindow/project/entityeditorformfactorybase.h"
#include "mainwindow/project/exceptioninvalidentityname.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/string/string.h"

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

ProjectBuilder::ProjectBuilder(Project& project)
  : m_project(project)
{
}

Frame* ProjectBuilder::edit_frame(
    const Dictionary&   values) const
{
    const std::string name = get_entity_name(values);

    Dictionary clean_values(values);
    clean_values.strings().remove(EntityEditorFormFactoryBase::NameParameter.c_str());

    Frame* old_frame = m_project.get_frame();
    const size_t old_canvas_width = old_frame->image().properties().m_canvas_width;
    const size_t old_canvas_height = old_frame->image().properties().m_canvas_height;

    auto_release_ptr<Frame> new_frame = FrameFactory::create(name.c_str(), clean_values, old_frame->aovs(), old_frame->lpe_aovs());
    new_frame->post_processing_stages().swap(old_frame->post_processing_stages());
    const size_t new_canvas_width = new_frame->image().properties().m_canvas_width;
    const size_t new_canvas_height = new_frame->image().properties().m_canvas_height;

    if (new_canvas_width != old_canvas_width || new_canvas_height != old_canvas_height)
        new_frame->reset_crop_window();

    m_project.set_frame(new_frame);

    slot_notify_project_modification();
    emit signal_frame_modified();

    return m_project.get_frame();
}

void ProjectBuilder::slot_notify_project_modification() const
{
    emit signal_project_modified();
}

std::string ProjectBuilder::get_entity_name(const Dictionary& values)
{
    if (!values.strings().exist(EntityEditorFormFactoryBase::NameParameter.c_str()))
        throw ExceptionInvalidEntityName();

    const std::string name = trim_both(
        values.get<std::string>(EntityEditorFormFactoryBase::NameParameter.c_str()));

    if (!is_valid_entity_name(name))
        throw ExceptionInvalidEntityName();

    return name;
}

bool ProjectBuilder::is_valid_entity_name(const std::string& name)
{
    return !name.empty();
}

}   // namespace studio
}   // namespace appleseed
