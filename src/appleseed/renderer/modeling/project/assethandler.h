
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Francois Beaune, The appleseedhq Organization
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

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <string>

// Forward declarations.
namespace renderer { class Project; }

namespace renderer
{

class AssetHandler
{
  public:
    enum Mode
    {
        CopyRelativeAssetsOnly,     // only bring asset files with relative paths
        CopyAllAssets               // bring all asset files
    };

    // Constructor.
    AssetHandler(
        Project&                    project,
        const char*                 filepath,
        const Mode                  mode);

    bool handle_assets() const;

  private:
    Project&                        m_project;
    const boost::filesystem::path   m_project_old_root_path;
    const boost::filesystem::path   m_project_new_root_path;
    const boost::filesystem::path   m_project_old_root_dir;
    const boost::filesystem::path   m_project_new_root_dir;
    const Mode                      m_mode;

    bool handle_asset(std::string& asset_path) const;
    bool handle_absolute_asset(std::string& asset_path) const;
    bool make_absolute_asset_path(std::string& asset_path) const;
    bool copy_absolute_asset(std::string& asset_path) const;
    bool copy_relative_asset(std::string& asset_path) const;
};

}   // namespace renderer
