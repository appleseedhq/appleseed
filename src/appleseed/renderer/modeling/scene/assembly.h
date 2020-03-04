
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

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/scene/basegroup.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/iassemblyfactory.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/apiarray.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <utility>

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class StringArray; }
namespace foundation    { class StringDictionary; }
namespace renderer      { class ObjectInstance; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class OnRenderBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }

namespace renderer
{

//
// An array of object instances.
//

typedef std::pair<const ObjectInstance*, size_t> IndexedObjectInstance;

APPLESEED_DECLARE_APIARRAY(IndexedObjectInstanceArray, IndexedObjectInstance);


//
// An assembly is either entirely self-contained, or it references colors,
// textures and texture instances defined in the parent scene or assembly.
//

class APPLESEED_DLLSYMBOL Assembly
  : public Entity
  , public BaseGroup
{
  public:
    // Return a string identifying the model of this entity.
    virtual const char* get_model() const;

    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Delete this instance.
    void release() override;

    // Access the BSDFs.
    BSDFContainer& bsdfs() const;

    // Access the BSSRDFs.
    BSSRDFContainer& bssrdfs() const;

    // Access the EDFs.
    EDFContainer& edfs() const;

    // Access the surface shaders.
    SurfaceShaderContainer& surface_shaders() const;

    // Access the materials.
    MaterialContainer& materials() const;

    // Access the lights.
    LightContainer& lights() const;

    // Access the objects.
    ObjectContainer& objects() const;

    // Access the object instances.
    ObjectInstanceContainer& object_instances() const;

    // Access the volumes.
    VolumeContainer& volumes() const;

    // Clear the assembly contents.
    void clear();

    // Compute the local space bounding box of the assembly, including all child assemblies,
    // over the shutter interval.
    GAABB3 compute_local_bbox() const;

    // Compute the local space bounding box of this assembly, excluding all child assemblies,
    // over the shutter interval.
    GAABB3 compute_non_hierarchical_local_bbox() const;

    // Expose asset file paths referenced by this entity to the outside.
    void collect_asset_paths(foundation::StringArray& paths) const override;
    void update_asset_paths(const foundation::StringDictionary& mappings) override;

    bool on_render_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnRenderBeginRecorder&      recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr) override;

    bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr) override;

    void on_frame_end(
        const Project&              project,
        const BaseGroup*            parent) override;

    struct APPLESEED_DLLSYMBOL RenderData
    {
        IndexedObjectInstanceArray  m_procedural_object_instances;

        RenderData();

        void clear();
    };

    // Return render-time data of this entity.
    // Render-time data are available between on_frame_begin() and on_frame_end() calls.
    const RenderData& get_render_data() const;

  protected:
    RenderData m_render_data;

    // Constructor.
    Assembly(
        const char*                 name,
        const ParamArray&           params);

    // Destructor.
    ~Assembly() override;

  private:
    friend class AssemblyFactory;

    struct Impl;
    Impl* impl;
};


//
// Assembly factory.
//


class APPLESEED_DLLSYMBOL AssemblyFactory
  : public IAssemblyFactory
{
  public:
    // Delete this instance.
    void release() override;

    // Return a string identifying this assembly model.
    const char* get_model() const override;

    // Return metadata for this assembly model.
    foundation::Dictionary get_model_metadata() const override;

    // Return metadata for the inputs of this assembly model.
    foundation::DictionaryArray get_input_metadata() const override;

    // Create a new assembly.
    foundation::auto_release_ptr<Assembly> create(
        const char*         name,
        const ParamArray&   params = ParamArray()) const override;
};


//
// Assembly class implementation.
//

inline const Assembly::RenderData& Assembly::get_render_data() const
{
    return m_render_data;
}

}   // namespace renderer
