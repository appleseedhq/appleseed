
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ITEMTYPEMAP_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ITEMTYPEMAP_H

// appleseed.renderer headers.
#include "renderer/api/scene.h"

// Forward declarations.
namespace appleseed { namespace studio { class AssemblyCollectionItem; }}
namespace appleseed { namespace studio { class AssemblyInstanceCollectionItem; }}
namespace appleseed { namespace studio { class AssemblyItem; }}
namespace appleseed { namespace studio { class BSDFCollectionItem; }}
namespace appleseed { namespace studio { class ColorCollectionItem; }}
namespace appleseed { namespace studio { class EDFCollectionItem; }}
namespace appleseed { namespace studio { class EntityItem; }}
namespace appleseed { namespace studio { class EnvironmentEDFCollectionItem; }}
namespace appleseed { namespace studio { class EnvironmentShaderCollectionItem; }}
namespace appleseed { namespace studio { class LightCollectionItem; }}
namespace appleseed { namespace studio { class MaterialCollectionItem; }}
namespace appleseed { namespace studio { class ObjectCollectionItem; }}
namespace appleseed { namespace studio { class ObjectInstanceCollectionItem; }}
namespace appleseed { namespace studio { class ObjectInstanceItem; }}
namespace appleseed { namespace studio { class SurfaceShaderCollectionItem; }}
namespace appleseed { namespace studio { class TextureCollectionItem; }}
namespace appleseed { namespace studio { class TextureInstanceCollectionItem; }}
namespace renderer  { class BSDF; }
namespace renderer  { class ColorEntity; }
namespace renderer  { class EDF; }
namespace renderer  { class EnvironmentEDF; }
namespace renderer  { class EnvironmentShader; }
namespace renderer  { class Light; }
namespace renderer  { class Material; }
namespace renderer  { class Object; }
namespace renderer  { class SurfaceShader; }
namespace renderer  { class Texture; }

namespace appleseed {
namespace studio {

//
// The ItemTypeMap structure maps entity or container types to project item types.
//

template <typename> struct ItemTypeMap;

template <> struct ItemTypeMap<renderer::AssemblyContainer>             { typedef AssemblyCollectionItem T; };
template <> struct ItemTypeMap<renderer::AssemblyInstanceContainer>     { typedef AssemblyInstanceCollectionItem T; };
template <> struct ItemTypeMap<renderer::BSDFContainer>                 { typedef BSDFCollectionItem T; };
template <> struct ItemTypeMap<renderer::ColorContainer>                { typedef ColorCollectionItem T; };
template <> struct ItemTypeMap<renderer::EDFContainer>                  { typedef EDFCollectionItem T; };
template <> struct ItemTypeMap<renderer::EnvironmentEDFContainer>       { typedef EnvironmentEDFCollectionItem T; };
template <> struct ItemTypeMap<renderer::EnvironmentShaderContainer>    { typedef EnvironmentShaderCollectionItem T; };
template <> struct ItemTypeMap<renderer::LightContainer>                { typedef LightCollectionItem T; };
template <> struct ItemTypeMap<renderer::MaterialContainer>             { typedef MaterialCollectionItem T; };
template <> struct ItemTypeMap<renderer::ObjectContainer>               { typedef ObjectCollectionItem T; };
template <> struct ItemTypeMap<renderer::ObjectInstanceContainer>       { typedef ObjectInstanceCollectionItem T; };
template <> struct ItemTypeMap<renderer::SurfaceShaderContainer>        { typedef SurfaceShaderCollectionItem T; };
template <> struct ItemTypeMap<renderer::TextureContainer>              { typedef TextureCollectionItem T; };
template <> struct ItemTypeMap<renderer::TextureInstanceContainer>      { typedef TextureInstanceCollectionItem T; };

template <> struct ItemTypeMap<renderer::Assembly>                      { typedef AssemblyItem T; };
template <> struct ItemTypeMap<renderer::AssemblyInstance>              { typedef EntityItem T; };
template <> struct ItemTypeMap<renderer::BSDF>                          { typedef EntityItem T; };
template <> struct ItemTypeMap<renderer::ColorEntity>                   { typedef EntityItem T; };
template <> struct ItemTypeMap<renderer::EDF>                           { typedef EntityItem T; };
template <> struct ItemTypeMap<renderer::EnvironmentEDF>                { typedef EntityItem T; };
template <> struct ItemTypeMap<renderer::EnvironmentShader>             { typedef EntityItem T; };
template <> struct ItemTypeMap<renderer::Light>                         { typedef EntityItem T; };
template <> struct ItemTypeMap<renderer::Material>                      { typedef EntityItem T; };
template <> struct ItemTypeMap<renderer::Object>                        { typedef EntityItem T; };
template <> struct ItemTypeMap<renderer::ObjectInstance>                { typedef ObjectInstanceItem T; };
template <> struct ItemTypeMap<renderer::SurfaceShader>                 { typedef EntityItem T; };
template <> struct ItemTypeMap<renderer::Texture>                       { typedef EntityItem T; };
template <> struct ItemTypeMap<renderer::TextureInstance>               { typedef EntityItem T; };

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ITEMTYPEMAP_H
