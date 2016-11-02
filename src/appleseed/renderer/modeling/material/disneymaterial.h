
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_MATERIAL_DISNEYMATERIAL_H
#define APPLESEED_RENDERER_MODELING_MATERIAL_DISNEYMATERIAL_H

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/disneybrdf.h"
#include "renderer/modeling/material/imaterialfactory.h"
#include "renderer/modeling/material/material.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// OpenImageIO headers.
#include "foundation/platform/oiioheaderguards.h"
BEGIN_OIIO_INCLUDES
#include "OpenImageIO/texture.h"
END_OIIO_INCLUDES

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace foundation    { class StringArray; }
namespace foundation    { class StringDictionary; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class MessageContext; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class ShadingContext; }

namespace renderer
{

//
// A layer in the Disney material.
//

class APPLESEED_DLLSYMBOL DisneyMaterialLayer
  : public Entity
{
  public:
    // Constructor.
    DisneyMaterialLayer(const DisneyMaterialLayer& other);

    // Destructor.
    ~DisneyMaterialLayer();

    // Delete this instance.
    virtual void release() APPLESEED_OVERRIDE;

    DisneyMaterialLayer& operator=(const DisneyMaterialLayer& other);

    bool operator<(const DisneyMaterialLayer& other) const;

    int get_layer_number() const;

    bool prepare_expressions() const;

    void evaluate_expressions(
        const ShadingPoint&             shading_point,
        OIIO::TextureSystem&            texture_system,
        foundation::Color3f&            base_color,
        DisneyBRDFInputValues&          values) const;

    static foundation::DictionaryArray get_input_metadata();
    static foundation::Dictionary get_default_values();

  private:
    friend class DisneyMaterial;

    struct Impl;
    Impl* impl;

    // Constructor.
    DisneyMaterialLayer(
        const char*                     name,
        const foundation::Dictionary&   params);
};


//
// Disney material.
//

class APPLESEED_DLLSYMBOL DisneyMaterial
  : public Material
{
  public:
    // Delete this instance.
    virtual void release() APPLESEED_OVERRIDE;

    // Return a string identifying the model of this material.
    virtual const char* get_model() const APPLESEED_OVERRIDE;

    // Expose asset file paths referenced by this entity to the outside.
    virtual void collect_asset_paths(foundation::StringArray& paths) const APPLESEED_OVERRIDE;
    virtual void update_asset_paths(const foundation::StringDictionary& mappings) APPLESEED_OVERRIDE;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = 0) APPLESEED_OVERRIDE;

    // This method is called once after rendering each frame (only if on_frame_begin() was called).
    virtual void on_frame_end(
        const Project&              project,
        const BaseGroup*            parent) APPLESEED_OVERRIDE;

    // Add a new layer with given values to the material.
    // A name and a number will be automatically assigned
    // to the layer if it lacks them.
    void add_layer(
        foundation::Dictionary      layer_values);

    // Add a new layer using default values to the material.
    void add_new_default_layer();

    // Retrieve the number of layers in the material.
    size_t get_layer_count() const;

    // Retrieve a given layer of the material.
    const DisneyMaterialLayer& get_layer(
        const size_t                index,
        const size_t                thread_index = ~0) const;

  private:
    friend class DisneyMaterialFactory;

    struct Impl;
    Impl* impl;

    // Constructor.
    DisneyMaterial(
        const char*         name,
        const ParamArray&   params);

    // Destructor.
    ~DisneyMaterial();

    // Prepare all layers for rendering. Returns true on success.
    bool prepare_layers(const MessageContext& context);
};


//
// Disney material factory.
//

class APPLESEED_DLLSYMBOL DisneyMaterialFactory
  : public IMaterialFactory
{
  public:
    // Return a string identifying this material model.
    virtual const char* get_model() const APPLESEED_OVERRIDE;

    // Return metadata for this material model.
    virtual foundation::Dictionary get_model_metadata() const APPLESEED_OVERRIDE;

    // Return metadata for the inputs of this material model.
    virtual foundation::DictionaryArray get_input_metadata() const APPLESEED_OVERRIDE;

    // Create a new material instance.
    virtual foundation::auto_release_ptr<Material> create(
        const char*         name,
        const ParamArray&   params) const APPLESEED_OVERRIDE;

    // Static variant of the create() method above.
    static foundation::auto_release_ptr<Material> static_create(
        const char*         name,
        const ParamArray&   params);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_MATERIAL_DISNEYMATERIAL_H
