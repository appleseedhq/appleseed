
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "archiveassembly.h"

// Standard headers.
#include <string>

// appleseed.renderer headers.
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/projectfilereader.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <string>

using namespace foundation;

namespace renderer
{

//
// ArchiveAssembly class implementation.
//

namespace
{
    const char* Model = "archive_assembly";
}

ArchiveAssembly::ArchiveAssembly(
    const char*         name,
    const ParamArray&   params)
  : ProceduralAssembly(name, params)
  , m_archive_opened(false)
{
}

void ArchiveAssembly::release()
{
    delete this;
}

const char* ArchiveAssembly::get_model() const
{
    return Model;
}

void ArchiveAssembly::collect_asset_paths(StringArray& paths) const
{
    if (m_params.strings().exist("filename"))
        paths.push_back(m_params.get("filename"));
}

void ArchiveAssembly::update_asset_paths(const StringDictionary& mappings)
{
    if (m_params.strings().exist("filename"))
        m_params.set("filename", mappings.get(m_params.get("filename")));
}

bool ArchiveAssembly::do_expand_contents(
    const Project&      project,
    const Assembly*     parent,
    IAbortSwitch*       abort_switch)
{
    if (!m_archive_opened)
    {
        // Establish and store the qualified path to the archive project.
        const SearchPaths& search_paths = project.search_paths();
        const std::string filepath =
            to_string(search_paths.qualify(m_params.get_required<std::string>("filename", "")));

        auto_release_ptr<Assembly> assembly =
            ProjectFileReader::read_archive(
                filepath.c_str(),
                nullptr,  // for now, we don't validate archives
                search_paths,
                ProjectFileReader::OmitProjectSchemaValidation);

        if (assembly.get())
        {
            swap_contents(*assembly);
            m_archive_opened = true;
        }
    }

    return true;
}


//
// ArchiveAssemblyFactory class implementation.
//

void ArchiveAssemblyFactory::release()
{
    delete this;
}

const char* ArchiveAssemblyFactory::get_model() const
{
    return Model;
}

Dictionary ArchiveAssemblyFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Archive Assembly");
}

DictionaryArray ArchiveAssemblyFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "filename")
            .insert("label", "File Path")
            .insert("type", "file")
            .insert("file_picker_mode", "open")
            .insert("file_picker_type", "project")
            .insert("use", "required"));

    return metadata;
}

auto_release_ptr<Assembly> ArchiveAssemblyFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Assembly>(new ArchiveAssembly(name, params));
}

}   // namespace renderer
