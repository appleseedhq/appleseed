
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_RENDERER_MODELING_SCENE_CONTAINERS_H
#define APPLESEED_RENDERER_MODELING_SCENE_CONTAINERS_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/entity/entitymap.h"
#include "renderer/modeling/entity/entityvector.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/stringexception.h"

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class AssemblyInstance; }
namespace renderer  { class BSDF; }
namespace renderer  { class ColorEntity; }
namespace renderer  { class EDF; }
namespace renderer  { class EnvironmentEDF; }
namespace renderer  { class EnvironmentShader; }
namespace renderer  { class Light; }
namespace renderer  { class Material; }
namespace renderer  { class Object; }
namespace renderer  { class ObjectInstance; }
namespace renderer  { class SurfaceShader; }
namespace renderer  { class Texture; }
namespace renderer  { class TextureInstance; }

namespace renderer
{

//
// Entity containers.
//

typedef TypedEntityMap<Assembly>                AssemblyContainer;
typedef TypedEntityMap<AssemblyInstance>        AssemblyInstanceContainer;
typedef TypedEntityVector<BSDF>                 BSDFContainer;
typedef TypedEntityVector<ColorEntity>          ColorContainer;
typedef TypedEntityVector<EDF>                  EDFContainer;
typedef TypedEntityVector<EnvironmentEDF>       EnvironmentEDFContainer;
typedef TypedEntityVector<EnvironmentShader>    EnvironmentShaderContainer;
typedef TypedEntityVector<Light>                LightContainer;
typedef TypedEntityVector<Material>             MaterialContainer;
typedef TypedEntityVector<Object>               ObjectContainer;
typedef TypedEntityVector<ObjectInstance>       ObjectInstanceContainer;
typedef TypedEntityVector<SurfaceShader>        SurfaceShaderContainer;
typedef TypedEntityVector<Texture>              TextureContainer;
typedef TypedEntityVector<TextureInstance>      TextureInstanceContainer;


//
// Utilities.
//

// Exception thrown by renderer::get_entity() when an entity is not found.
struct ExceptionUnknownEntity
  : public foundation::StringException
{
    explicit ExceptionUnknownEntity(const char* s)
      : foundation::StringException("unknown entity", s)
    {
    }
};

// Retrieve a mandatory entity from a container.
// Throws:
//   - a renderer::ParamArray::ExceptionUnknownName if the parameter does not exist
//   - a renderer::ExceptionUnknownEntity exception if the requested entity does not exist
template <typename T, typename Container>
const T* get_required_entity(
    const Container&    container,
    const ParamArray&   params,
    const std::string&  param_name)
{
    const std::string entity_name =
        params.get<std::string>(param_name.c_str());

    const size_t entity_index = container.get_index(entity_name.c_str());

    if (entity_index == ~size_t(0))
        throw ExceptionUnknownEntity(entity_name.c_str());

    return container.get(entity_index);
}

// Retrieve an optional entity from a container.
// Returns 0 if the parameter does not exist.
// Throws a renderer::ExceptionUnknownEntity exception if the requested entity does not exist.
template <typename T, typename Container>
const T* get_optional_entity(
    const Container&    container,
    const ParamArray&   params,
    const std::string&  param_name)
{
    const std::string entity_name =
        params.get_optional<std::string>(param_name.c_str(), "");

    if (entity_name.empty())
        return 0;

    const size_t entity_index = container.get_index(entity_name.c_str());

    if (entity_index == ~size_t(0))
        throw ExceptionUnknownEntity(entity_name.c_str());

    return container.get(entity_index);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SCENE_CONTAINERS_H
