
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// OpenImageIO headers
#include "OpenImageIO/texture.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace renderer      { class DisneyMaterial; }
namespace renderer      { class ParamArray; }
namespace renderer      { class ShadingContext; }

namespace renderer
{

class DLLSYMBOL DisneyParamExpression
  : public foundation::NonCopyable
{
  public:
    DisneyParamExpression(const char* expr);
    ~DisneyParamExpression();

    bool is_valid() const;

    const char* parse_error() const;

    void report_error(const char* message) const;

    bool is_constant() const;

  private:
    struct Impl;
    Impl* impl;
};

class DLLSYMBOL DisneyMaterialLayer
{
  public:
    DisneyMaterialLayer(const DisneyMaterialLayer& other);

    ~DisneyMaterialLayer();

    DisneyMaterialLayer& operator=(const DisneyMaterialLayer& other);

    bool operator<(const DisneyMaterialLayer& other) const;

    bool prepare_expressions() const;

    void evaluate_expressions(
        const ShadingPoint&     shading_point,
        OIIO::TextureSystem&    texture_system,
        foundation::Color3d&    base_color,
        DisneyBRDFInputValues&  values) const;

    static foundation::DictionaryArray get_input_metadata();

    static foundation::Dictionary get_default_values();

  private:
    friend class DisneyMaterial;

    struct Impl;
    Impl* impl;

    // Constructor
    DisneyMaterialLayer(
        const Project&                  project,
        const char*                     name,
        const foundation::Dictionary&   params);

    void swap(DisneyMaterialLayer& other);
};

class DLLSYMBOL DisneyMaterial
  : public Material
{
  public:
    // Delete this instance.
    virtual void release() OVERRIDE;

    // Return a string identifying the model of this material.
    virtual const char* get_model() const OVERRIDE;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&              project,
        const Assembly&             assembly,
        foundation::AbortSwitch*    abort_switch = 0) OVERRIDE;

    // This method is called once after rendering each frame.
    virtual void on_frame_end(
        const Project&              project,
        const Assembly&             assembly) OVERRIDE;

    std::size_t get_layer_count() const;

    const DisneyMaterialLayer& get_layer(const std::size_t index) const;

  private:
    friend class DisneyMaterialFactory;

    struct Impl;
    Impl* impl;

    // Constructor.
    DisneyMaterial(
        const char*         name,
        const ParamArray&   params);

    // Destructor
    ~DisneyMaterial();
};


//
// Disney material factory.
//

class DLLSYMBOL DisneyMaterialFactory
  : public IMaterialFactory
{
  public:
    // Return a string identifying this material model.
    virtual const char* get_model() const OVERRIDE;

    // Return a human-readable string identifying this material model.
    virtual const char* get_human_readable_model() const OVERRIDE;

    // Return a set of input metadata for this material model.
    virtual foundation::DictionaryArray get_input_metadata() const OVERRIDE;

    // Create a new material instance.
    virtual foundation::auto_release_ptr<Material> create(
        const char*         name,
        const ParamArray&   params) const OVERRIDE;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_MATERIAL_DISNEYMATERIAL_H
