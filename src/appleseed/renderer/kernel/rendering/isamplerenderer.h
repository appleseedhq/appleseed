
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_ISAMPLERENDERER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_ISAMPLERENDERER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// Forward declarations.
namespace renderer      { class ShadingResult; }

namespace renderer
{

//
// Sample renderer interface.
//

class RENDERERDLL ISampleRenderer
  : public foundation::IUnknown
{
  public:
    // Render a sample at a given point on the image plane.
    // The point is expressed in normalized device coordinates (NDC).
    // See renderer::Camera class documentation for details.
    virtual void render_sample(
        SamplingContext&                sampling_context,
        const foundation::Vector2d&     image_point,            // point in image plane, in NDC
        ShadingResult&                  shading_result) = 0;
};


//
// Interface of a ISampleRenderer factory that can cross DLL boundaries.
//

class RENDERERDLL ISampleRendererFactory
  : public foundation::IUnknown
{
  public:
    // Return a new sample renderer instance.
    virtual ISampleRenderer* create() = 0;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_ISAMPLERENDERER_H
