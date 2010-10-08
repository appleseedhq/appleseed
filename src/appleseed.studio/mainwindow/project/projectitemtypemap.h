
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTITEMTYPEMAP_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTITEMTYPEMAP_H

// appleseed.renderer headers.
#include "renderer/api/scene.h"

// Forward declarations.
namespace appleseed { namespace studio { class AssemblyCollectionProjectItem; }}
namespace appleseed { namespace studio { class AssemblyInstanceCollectionProjectItem; }}
namespace appleseed { namespace studio { class ColorCollectionProjectItem; }}
namespace appleseed { namespace studio { class EnvironmentEDFCollectionProjectItem; }}
namespace appleseed { namespace studio { class EnvironmentShaderCollectionProjectItem; }}
namespace appleseed { namespace studio { class TextureCollectionProjectItem; }}
namespace appleseed { namespace studio { class TextureInstanceCollectionProjectItem; }}

namespace appleseed {
namespace studio {

template <typename> struct ProjectItemTypeMap;

// Containers.
template <> struct ProjectItemTypeMap<renderer::AssemblyContainer>           { typedef AssemblyCollectionProjectItem T; };
template <> struct ProjectItemTypeMap<renderer::AssemblyInstanceContainer>   { typedef AssemblyInstanceCollectionProjectItem T; };
template <> struct ProjectItemTypeMap<renderer::ColorContainer>              { typedef ColorCollectionProjectItem T; };
template <> struct ProjectItemTypeMap<renderer::EnvironmentEDFContainer>     { typedef EnvironmentEDFCollectionProjectItem T; };
template <> struct ProjectItemTypeMap<renderer::EnvironmentShaderContainer>  { typedef EnvironmentShaderCollectionProjectItem T; };
template <> struct ProjectItemTypeMap<renderer::TextureContainer>            { typedef TextureCollectionProjectItem T; };
template <> struct ProjectItemTypeMap<renderer::TextureInstanceContainer>    { typedef TextureInstanceCollectionProjectItem T; };

// Entities.

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_PROJECTITEMTYPEMAP_H
