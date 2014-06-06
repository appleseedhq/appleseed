
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
#include "renderer/modeling/material/imaterialfactory.h"
#include "renderer/modeling/material/material.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace renderer      { class ParamArray; }
namespace renderer      { class DisneyMaterial; }

namespace renderer
{

class DLLSYMBOL DisneyMaterialLayer
{
  public:
    // Destructor
    ~DisneyMaterialLayer();

    DisneyMaterialLayer(const DisneyMaterialLayer& other);

    DisneyMaterialLayer& operator=(const DisneyMaterialLayer& other);
    
    bool operator<(const DisneyMaterialLayer& other) const;

    bool check_expressions_syntax() const;
    
  private:
    friend class DisneyMaterial;

    // Constructor
    DisneyMaterialLayer(
        const char* name,
        const foundation::Dictionary& params);

    void swap(DisneyMaterialLayer& other);
    
    struct Impl;
    Impl *impl;
};

class DLLSYMBOL DisneyMaterial
  : public Material
{
  public:
    // Constructor.
    DisneyMaterial(
        const char*                 name,
        const ParamArray&           params);  
    
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

  private:
    // Destructor
    ~DisneyMaterial();

    std::size_t num_layers() const;
    const DisneyMaterialLayer& get_layer(std::size_t index) const;
    
    struct Impl;
    Impl *impl;
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
