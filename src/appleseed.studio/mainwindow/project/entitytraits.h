
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYTRAITS_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYTRAITS_H

// appleseed.renderer headers.
#include "renderer/api/scene.h"

// Forward declarations.
namespace renderer  { class BSDF; }
namespace renderer  { class BSDFFactoryRegistrar; }
namespace renderer  { class Camera; }
namespace renderer  { class EDF; }
namespace renderer  { class EDFFactoryRegistrar; }
namespace renderer  { class EnvironmentEDF; }
namespace renderer  { class EnvironmentEDFFactoryRegistrar; }
namespace renderer  { class EnvironmentShader; }
namespace renderer  { class EnvironmentShaderFactoryRegistrar; }
namespace renderer  { class Light; }
namespace renderer  { class Material; }
namespace renderer  { class SurfaceShader; }
namespace renderer  { class SurfaceShaderFactoryRegistrar; }

namespace appleseed {
namespace studio {

template <typename Entity> struct EntityTraits;

template <>
struct EntityTraits<renderer::BSDF>
{
    typedef renderer::BSDFContainer ContainerType;
    typedef renderer::BSDFFactoryRegistrar FactoryRegistrarType;

    static const char* get_entity_type_name()                           { return "bsdf"; }
    static const char* get_human_readable_entity_type_name()            { return "BSDF"; }
    static const char* get_human_readable_collection_type_name()        { return "BSDFs"; }

    template <typename ParentEntity>
    static ContainerType& get_entity_container(ParentEntity& parent)    { return parent.bsdfs(); }
};

template <>
struct EntityTraits<renderer::EDF>
{
    typedef renderer::EDFContainer ContainerType;
    typedef renderer::EDFFactoryRegistrar FactoryRegistrarType;

    static const char* get_entity_type_name()                           { return "edf"; }
    static const char* get_human_readable_entity_type_name()            { return "EDF"; }
    static const char* get_human_readable_collection_type_name()        { return "EDFs"; }

    template <typename ParentEntity>
    static ContainerType& get_entity_container(ParentEntity& parent)    { return parent.edfs(); }
};

template <>
struct EntityTraits<renderer::EnvironmentEDF>
{
    typedef renderer::EnvironmentEDFContainer ContainerType;
    typedef renderer::EnvironmentEDFFactoryRegistrar FactoryRegistrarType;

    static const char* get_entity_type_name()                           { return "environment_edf"; }
    static const char* get_human_readable_entity_type_name()            { return "Environment EDF"; }
    static const char* get_human_readable_collection_type_name()        { return "Environment EDFs"; }

    template <typename ParentEntity>
    static ContainerType& get_entity_container(ParentEntity& parent)    { return parent.environment_edfs(); }
};

template <>
struct EntityTraits<renderer::Material>
{
    typedef renderer::MaterialContainer ContainerType;

    static const char* get_entity_type_name()                           { return "material"; }
    static const char* get_human_readable_entity_type_name()            { return "Material"; }
    static const char* get_human_readable_collection_type_name()        { return "Materials"; }

    template <typename ParentEntity>
    static ContainerType& get_entity_container(ParentEntity& parent)    { return parent.materials(); }
};

template <>
struct EntityTraits<renderer::SurfaceShader>
{
    typedef renderer::SurfaceShaderContainer ContainerType;
    typedef renderer::SurfaceShaderFactoryRegistrar FactoryRegistrarType;

    static const char* get_entity_type_name()                           { return "surface_shader"; }
    static const char* get_human_readable_entity_type_name()            { return "Surface Shader"; }
    static const char* get_human_readable_collection_type_name()        { return "Surface Shaders"; }

    template <typename ParentEntity>
    static ContainerType& get_entity_container(ParentEntity& parent)    { return parent.surface_shaders(); }
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYTRAITS_H
