
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_MODELING_ENVIRONMENTSHADER_ENVIRONMENTSHADER_H
#define APPLESEED_RENDERER_MODELING_ENVIRONMENTSHADER_ENVIRONMENTSHADER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/entity/connectableentity.h"

// Forward declarations.
namespace renderer      { class InputEvaluator; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingResult; }

namespace renderer
{

//
// Environment shader.
//

class RENDERERDLL EnvironmentShader
  : public ConnectableEntity
{
  public:
    // Constructor.
    EnvironmentShader(
        const char*                 name,
        const ParamArray&           params);

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    // This method is called once before rendering each frame.
    virtual void on_frame_begin(const Project& project);

    // This method is called once after rendering each frame.
    virtual void on_frame_end(const Project& project);

    // Evaluate the environment for a given unit-length direction.
    virtual void evaluate(
        InputEvaluator&             input_evaluator,
        const foundation::Vector3d& direction,                      // world space direction, pointing toward the environment
        ShadingResult&              shading_result) const = 0;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_ENVIRONMENTSHADER_ENVIRONMENTSHADER_H
