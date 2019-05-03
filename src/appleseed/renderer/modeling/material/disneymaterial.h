
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/bsdf/disneybrdf.h"
#include "renderer/modeling/material/imaterialfactory.h"
#include "renderer/modeling/material/material.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace foundation    { class StringArray; }
namespace foundation    { class StringDictionary; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class MessageContext; }
namespace renderer      { class OIIOTextureSystem; }
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
    ~DisneyMaterialLayer() override;

    // Delete this instance.
    void release() override;

    DisneyMaterialLayer& operator=(const DisneyMaterialLayer& other);

    bool operator<(const DisneyMaterialLayer& other) const;

    int get_layer_number() const;

    bool prepare_expressions() const;

    void evaluate_expressions(
        const ShadingPoint&             shading_point,
        OIIOTextureSystem&              texture_system,
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
    void release() override;

    // Return a string identifying the model of this material.
    const char* get_model() const override;

    // Expose asset file paths referenced by this entity to the outside.
    void collect_asset_paths(foundation::StringArray& paths) const override;
    void update_asset_paths(const foundation::StringDictionary& mappings) override;

    bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr) override;

    void on_frame_end(
        const Project&              project,
        const BaseGroup*            parent) override;

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
        const size_t                thread_index = ~size_t(0)) const;

  private:
    friend class DisneyMaterialFactory;

    struct Impl;
    Impl* impl;

    // Constructor.
    DisneyMaterial(
        const char*         name,
        const ParamArray&   params);

    // Destructor.
    ~DisneyMaterial() override;

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
    // Delete this instance.
    void release() override;

    // Return a string identifying this material model.
    const char* get_model() const override;

    // Return metadata for this material model.
    foundation::Dictionary get_model_metadata() const override;

    // Return metadata for the inputs of this material model.
    foundation::DictionaryArray get_input_metadata() const override;

    // Create a new material instance.
    foundation::auto_release_ptr<Material> create(
        const char*         name,
        const ParamArray&   params) const override;
};

}   // namespace renderer
