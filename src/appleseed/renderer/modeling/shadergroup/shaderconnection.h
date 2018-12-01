
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
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace renderer  { class OSLShadingSystem; }
namespace renderer  { class ShaderGroup; }

namespace renderer
{

//
// A connection between two OSL shaders.
//

class APPLESEED_DLLSYMBOL ShaderConnection
  : public Entity
{
  public:
    // Delete this instance.
    void release() override;

    // Return the source layer name.
    const char* get_src_layer() const;

    // Return the source param name.
    const char* get_src_param() const;

    // Return the destination layer name.
    const char* get_dst_layer() const;

    // Return the destination param name.
    const char* get_dst_param() const;

  private:
    friend class ShaderGroup;

    struct Impl;
    Impl* impl;

    // Constructor.
    ShaderConnection(
        const char* src_layer,
        const char* src_param,
        const char* dst_layer,
        const char* dst_param);

    // Destructor.
    ~ShaderConnection() override;

    // Add this connection to OSL's shading system.
    bool add(OSLShadingSystem& shading_system);
};

}   // namespace renderer
