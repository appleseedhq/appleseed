
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_SURFACESHADER_NPRSURFACESHADERHELPER_H
#define APPLESEED_RENDERER_MODELING_SURFACESHADER_NPRSURFACESHADERHELPER_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer      { class AOVComponents; }
namespace renderer      { class CompositeNPRClosure; }
namespace renderer      { class ShaderGroup; }
namespace renderer      { class ShadingComponents; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingPoint; }
namespace renderer      { class ShadingResult; }


namespace renderer
{

//
// A helper class to evaluate NPR shading and contours.
// This class is used by the PhysicalSurfaceShader when the
// object materials include NPR components.
//

class NPRSurfaceShaderHelper
  : foundation::NonCopyable
{
  public:
    void evaluate(
        SamplingContext&            sampling_context,
        const ShadingContext&       shading_context,
        const ShadingPoint&         shading_point,
        AOVComponents&              components,
        ShadingComponents&          radiance) const;

  private:
    void evaluate_npr(
        SamplingContext&            sampling_context,
        const ShadingContext&       shading_context,
        const ShadingPoint&         shading_point,
        const ShaderGroup*          sg,
        ShadingComponents&          radiance,
        AOVComponents&              components) const;

    foundation::Color4f evaluate_npr_contour(
        SamplingContext&            sampling_context,
        const ShadingContext&       shading_context,
        const ShadingPoint&         shading_point,
        const CompositeNPRClosure&  c,
        const size_t                closure_index) const;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SURFACESHADER_NPRSURFACESHADERHELPER_H
