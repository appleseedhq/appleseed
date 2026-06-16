
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
#include "renderer/modeling/entity/connectableentity.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace renderer  { class AOVComponents; }
namespace renderer  { class ParamArray; }
namespace renderer  { class PixelContext; }
namespace renderer  { class ShadingComponents; }
namespace renderer  { class ShadingContext; }
namespace renderer  { class ShadingPoint; }
namespace renderer  { class ShadingResult; }
namespace renderer  { class ShadowCatcher; }

namespace renderer
{

//
// Surface shader.
//

class APPLESEED_DLLSYMBOL SurfaceShader
  : public ConnectableEntity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Constructor.
    SurfaceShader(
        const char*             name,
        const ParamArray&       params);

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    // Evaluate the shading at a given point.
    virtual void evaluate(
        SamplingContext&        sampling_context,
        const PixelContext&     pixel_context,
        const ShadingContext&   shading_context,
        const ShadingPoint&     shading_point,
        ShadingResult&          shading_result,
        ShadingComponents&      shading_components,
        AOVComponents&          aov_components,
        ShadowCatcher&          shadow_catcher) const = 0;
};

}   // namespace renderer
