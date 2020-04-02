
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Fedor Matantsev, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/trianglekey.h"
#include "renderer/modeling/scene/objectinstance.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/memory/poolallocator.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/uid.h"

// Embree headers.
#include <embree3/rtcore.h>

// Standard headers.
#include <map>
#include <memory>
#include <vector>

// Forward declarations.
namespace renderer { class Assembly; }
namespace renderer { class ShadingPoint; }
namespace renderer { class ShadingRay; }

namespace renderer
{

class EmbreeGeometryData;

typedef std::vector<std::unique_ptr<EmbreeGeometryData>>  EmbreeGeometryDataContainer;

class EmbreeScene;

class EmbreeDevice
  : public foundation::NonCopyable
{
  public:
    EmbreeDevice();
    ~EmbreeDevice();

  private:
    friend class EmbreeScene;

    RTCDevice m_device;
};

class EmbreeScene
  : public foundation::NonCopyable
{
  public:
    struct Arguments
    {
        const EmbreeDevice&     m_device;
        const Assembly&         m_assembly;

        explicit Arguments(
            const EmbreeDevice&     embree_device,
            const Assembly&         assembly)
          : m_device(embree_device)
          , m_assembly(assembly)
        {}
    };

    explicit EmbreeScene(const Arguments& arguments);

    ~EmbreeScene();

    void intersect(ShadingPoint& shading_point) const;
    bool occlude(const ShadingRay& shading_ray) const;

  private:
    RTCDevice                   m_device;
    RTCScene                    m_scene;
    EmbreeGeometryDataContainer m_geometry_container;
};

typedef std::map<
    foundation::UniqueID,
    foundation::Lazy<EmbreeScene>*
> EmbreeSceneContainer;
typedef EmbreeSceneContainer::iterator EmbreeSceneIterator;
typedef EmbreeSceneContainer::const_iterator EmbreeSceneConstIterator;

typedef foundation::AccessCacheMap<
    EmbreeSceneContainer,
    EmbreeSceneAccessCacheLines,
    EmbreeSceneAccessCacheWays,
    foundation::PoolAllocator<void, EmbreeSceneAccessCacheLines * EmbreeSceneAccessCacheWays>
> EmbreeSceneAccessCache;


//
// Embree scene factory.
//

class EmbreeSceneFactory
  : public foundation::ILazyFactory<EmbreeScene>
{
  public:
    // Constructor.
    explicit EmbreeSceneFactory(const EmbreeScene::Arguments& arguments);

    // Create the embree scene.
    std::unique_ptr<EmbreeScene> create() override;

  private:
    const EmbreeScene::Arguments m_arguments;
};

}   // namespace renderer
